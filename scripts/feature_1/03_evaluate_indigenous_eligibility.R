## ---------------------------------------------------------------------------
## 03_evaluate_indigenous_eligibility.R — feature-1 stage 3: tier resolution
##
## resolve_eligibility() assigns every community in the universe (stage 02) to
## one eligibility tier against the classified declarations (stage 01):
## direct_named_match, direct_spatial_county_match, statewide_state_match, or
## no_match. Final stage of the pipeline; called by 00_run_feature_1.R.
## ---------------------------------------------------------------------------
library(arrow)
library(tidyverse)
library(sf)
library(janitor)
library(here)

source(here::here("scripts", "utilities", "utilities.R"))

options(tigris_use_cache = TRUE)

#' Resolve a FEMA-disaster-declaration eligibility tier per community
#'
#' For every row in the Indigenous universe, classifies eligibility via a
#' cascade: direct tribal declaration name match, spatial intersection with
#' declaration counties, then a statewide declaration match. HHL rows skip the
#' direct path and resolve via `parent_county_fips`. Territorial rows also
#' resolve via `parent_county_fips`.
#'
#' @section Tier definitions (ordered factor, highest-first):
#' Tier labels describe the matching mechanism, not an editorial judgment.
#' \describe{
#'   \item{direct_named_match}{A tribal-direct FEMA declaration names the
#'     community (substring match on normalized `name` / `aliases` against
#'     `designated_area`).}
#'   \item{direct_spatial_county_match}{The community's interior overlaps
#'     (not merely touches the boundary of) a county that received a
#'     declaration in `county_declarations`.}
#'   \item{statewide_state_match}{Any state in the community's `state_list`
#'     received a statewide declaration (including AS/GU/MP territorial
#'     declarations, which FEMA routes as "statewide").}
#'   \item{no_match}{No declaration found by any path.}
#' }
#'
#' @param universe An `sf` tibble from `get_indigenous_universe()`.
#' @param tribal_declarations A tibble from `get_tribal_declarations()`.
#' @param county_declarations A tibble from `get_county_declarations()`.
#' @param statewide_declarations A tibble from `get_statewide_declarations()`.
#' @param counties_sf An `sf` of US counties — pass
#'   `tigris::counties(cb = TRUE, year = <year>)` with a CRS transformable to
#'   `EPSG:4269`.
#' @param min_name_match_precision Numeric in `[0, 1]`. Minimum
#'   `min(nchar)/max(nchar)` coverage between the community's **canonical**
#'   normalized name key (from `name`, not aliases) and a FEMA
#'   `designated_area` key for a candidate tribal-direct match to be retained.
#'   The canonical key must additionally have a bidirectional substring
#'   relationship with the declaration key (see "Canonical-key gate" below);
#'   this floor is a secondary check on coverage. Default `0.4`.
#' @param require_direct_state_match Logical. When `TRUE` (the default), a
#'   candidate tribal-direct match is retained only if the declaration's `state`
#'   appears in the community's `state_list` ∪ `home_state`. This blocks
#'   geographically impossible direct matches that otherwise slip through on
#'   substring collisions of common tokens.
#'
#' @section Canonical-key gate:
#' Each direct name match is gated on the community's **canonical** name key
#' (from `name`, normalized via `normalize_name_key()`) having a bidirectional
#' substring relationship with the FEMA `designated_area` key. Aliases are
#' kept in the output for transparency but are not used to qualify a match.
#' This guards against upstream contamination of the alias list — short
#' tigris-polygon names that got attached to longer BIA names by the
#' substring-based BIA↔tigris matcher (e.g., a tigris "Creek" polygon
#' attaching to the BIA "Village of Crooked Creek" row, then surfacing as a
#' bare `Creek` alias that collides with FEMA's "Creek (OTSA)" declaration).
#' Communities whose canonical key has no substring relationship with the
#' declaration key are dropped from the tribal-direct tier and resolved
#' through the spatial-intersect tier instead.
#'
#' @return A (non-`sf`) tibble with all universe columns plus the columns
#'   listed below. Row count equals `nrow(universe)`.
#'
#'   \describe{
#'     \item{tier}{Ordered factor with levels `"direct_named_match" >
#'       "direct_spatial_county_match" > "statewide_state_match" >
#'       "no_match"`.}
#'     \item{ia_flags_any, pa_flags_any}{Logical. `TRUE` if any declaration
#'       in any tier's matches had Individual Assistance / Public Assistance
#'       authorized.}
#'     \item{direct_declarations}{List-column. For each community, a tibble
#'       of matching rows from `tribal_declarations` (possibly 0 rows). Each
#'       row carries two extra columns added by the resolver:
#'       `name_match_precision` (numeric in `(0, 1]`, the
#'       `min(nchar)/max(nchar)` coverage of the best-matching
#'       universe-alias / `designated_area` pair) and `matched_on`
#'       (character, the two normalized keys that achieved that coverage,
#'       `"<universe_key>~<designated_area_key>"`).}
#'     \item{direct_match_precision_max}{Numeric in `(0, 1]`. The highest
#'       `name_match_precision` across the community's `direct_declarations`
#'       — interprets as the best-justified name-based FEMA match for the
#'       community. `NA` when there are no direct declarations.}
#'     \item{intersecting_county_declarations}{List-column. For each
#'       community, a tibble of `county_declarations` rows whose county's
#'       interior overlaps the community's geometry (for HHL and territorial
#'       rows: whose `county_fips == parent_county_fips`).}
#'     \item{statewide_declarations}{List-column. For each community, a
#'       tibble of `statewide_declarations` rows whose `state` appears in
#'       the community's `state_list`.}
#'     \item{notes}{Character. Caveat text tied to `recognition_status`
#'       (e.g., HHL eligibility follows the parent county; territorial
#'       eligibility follows the territory's single FEMA jurisdiction).
#'       `NA` for federal rows.}
#'   }
#'
#'   All universe columns (`community_id`, `name`, `aliases`,
#'   `recognition_status`, `geography_type`, `home_state`, `state_list`,
#'   `bia_name`, `aiannhr`, `parent_county_fips`, `has_geometry`,
#'   `match_strategy`, `geometry_source`, `resolution`, `confidence`, `note`)
#'   are preserved.
resolve_eligibility <- function(
  universe,
  tribal_declarations,
  county_declarations,
  statewide_declarations,
  counties_sf,
  min_name_match_precision = 0.4,
  require_direct_state_match = TRUE) {

  state_to_postal <- c(
    setNames(state.abb, state.abb),
    setNames(state.abb, state.name),
    DC = "DC", AS = "AS", GU = "GU", MP = "MP",
    "District of Columbia" = "DC",
    "American Samoa" = "AS",
    "Guam" = "GU",
    "Northern Mariana Islands" = "MP",
    "Commonwealth of the Northern Mariana Islands" = "MP")

  to_postal <- function(states) {
    if (length(states) == 0) return(character(0))
    out <- unname(state_to_postal[as.character(states)])
    unique(out[!is.na(out)])
  }

  ## -- Prepare spatial inputs --------------------------------------------------
  universe_sf <- universe %>%
    sf::st_as_sf() %>%
    sf::st_transform(CRS_GEO) %>%
    mutate(row_id = row_number())

  counties1 <- counties_sf %>%
    janitor::clean_names() %>%
    sf::st_transform(CRS_GEO) %>%
    select(county_fips = geoid, county_name = name, state_fips = statefp, geometry) %>%
    sf::st_make_valid()

  declaration_county_fips <- county_declarations %>%
    distinct(county_fips) %>%
    pull(county_fips)

  ## Silent-drop detector: a county declaration whose FIPS matches no county
  ## polygon can never reach a community through the spatial tier. Known
  ## causes: FEMA reusing retired FIPS (e.g., 46113 Shannon -> 46102 Oglala
  ## Lakota, pre-2022 CT counties, retired AK census areas) on older
  ## declarations, or pseudo-FIPS that classify_declarations() should be
  ## routing elsewhere.
  unmatched_declaration_fips <- setdiff(declaration_county_fips, counties1$county_fips)
  if (length(unmatched_declaration_fips) > 0) {
    warning(
      length(unmatched_declaration_fips),
      " county_declarations county_fips value(s) match no county polygon; ",
      "declarations for these areas cannot match any community via the ",
      "spatial tier: ",
      str_c(sort(unmatched_declaration_fips), collapse = ", "),
      call. = FALSE) }

  ## -- Step 1: Confirmed — name match vs tribal_declarations -------------------
  ## Bidirectional substring match on normalized keys (same strategy used by
  ## get_indigenous_universe() for BIA ↔ tigris linking). A universe row's set
  ## of aliases + canonical name are each normalized to a token-stripped key;
  ## a FEMA `designated_area` is similarly normalized; the row matches the
  ## declaration if any name key is a substring of the designated_area key or
  ## vice versa. Short keys (<3 chars after normalization) are skipped to
  ## avoid accidental prefix matches.

  tribal_with_keys <- tribal_declarations %>%
    mutate(designated_area_key = normalize_name_key(designated_area)) %>%
    filter(!is.na(designated_area_key), nchar(designated_area_key) >= 3)

  direct_match1 <- universe_sf %>%
    sf::st_drop_geometry() %>%
    select(row_id, community_id, name, aliases, home_state, state_list) %>%
    mutate(
      canonical_key = normalize_name_key(name),
      canonical_key = if_else(
        is.na(canonical_key) | nchar(canonical_key) < 3,
        NA_character_, canonical_key),
      community_states_postal = purrr::map2(
        state_list, home_state,
        ~ to_postal(c(unlist(.x), .y))))

  match_tribal_rows <- function(canonical_key, community_states) {
    empty_result <- tribal_with_keys[integer(0), ] %>%
      select(-designated_area_key) %>%
      mutate(
        name_match_precision = numeric(0),
        matched_on = character(0))
    if (is.na(canonical_key)) return(empty_result)

    ## Filter 1: state cross-check. A tribal-direct declaration carries the
    ## tribe's state; if it does not overlap the community's state_list /
    ## home_state, the "match" is a substring collision on a common token
    ## (e.g., AK village "Crooked Creek" colliding with OK "Creek (OTSA)").
    if (require_direct_state_match) {
      if (length(community_states) == 0) return(empty_result)
      candidate_idx <- which(tribal_with_keys$state %in% community_states)
    } else {
      candidate_idx <- seq_len(nrow(tribal_with_keys))
    }
    if (length(candidate_idx) == 0) return(empty_result)

    candidate_da_keys <- tribal_with_keys$designated_area_key[candidate_idx]

    ## Filter 2: canonical-key bidirectional substring gate. The community's
    ## canonical (BIA) name key — not an alias — must have a substring
    ## relationship with the declaration key. Aliases are skipped from the
    ## gate decision because the upstream BIA↔tigris matcher contaminates
    ## them with short tigris-polygon names (e.g., a `Creek` alias bolted
    ## onto "Village of Crooked Creek").
    gate_lgl <- bidirectional_substring_match(canonical_key, candidate_da_keys)
    if (!any(gate_lgl)) return(empty_result)

    ## Filter 3: coverage floor on the canonical-key precision. Once the
    ## gate is passed the bulk of contamination-driven false positives are
    ## already blocked; the floor catches the remaining short-canonical-key
    ## edge cases.
    gate_idx <- which(gate_lgl)
    coverages <- name_key_coverage(canonical_key, candidate_da_keys[gate_idx])
    keep_local <- !is.na(coverages) & coverages >= min_name_match_precision
    if (!any(keep_local)) return(empty_result)

    hit_global_idx <- candidate_idx[gate_idx[keep_local]]
    hit_coverages <- coverages[keep_local]
    hit_da_keys <- candidate_da_keys[gate_idx[keep_local]]

    tribal_with_keys[hit_global_idx, ] %>%
      select(-designated_area_key) %>%
      mutate(
        name_match_precision = hit_coverages,
        matched_on = str_c(canonical_key, "~", hit_da_keys))
  }

  direct_match2 <- direct_match1 %>%
    mutate(
      direct_declarations = purrr::map2(
        canonical_key, community_states_postal, match_tribal_rows),
      direct_hit = purrr::map_lgl(direct_declarations, ~ nrow(.x) > 0)) %>%
    select(row_id, direct_declarations, direct_hit)

  message(
    "Direct name match filters: canonical-key gate, state_match=",
    if (require_direct_state_match) "on" else "off",
    ", min precision=", format(min_name_match_precision),
    "; ", sum(direct_match2$direct_hit), " of ", nrow(direct_match2),
    " communities retained at least one direct-named declaration.")

  ## -- Step 2: Likely — interior overlap with declaration counties -------------
  ## DE-9IM "T********" requires the interiors to intersect. Plain
  ## st_intersects() also fires on pure boundary touches, crediting a county
  ## declaration to communities that merely abut the county line (e.g., AS's
  ## Eastern and Western Districts share a border on Tutuila, so each picked
  ## up the other's county-level declarations). st_relate() evaluates
  ## planar, which is safe here: community and county line-work share TIGER
  ## topology, so shared boundaries are exact.
  intersects_list <- suppressMessages(suppressWarnings(
    sf::st_relate(universe_sf, counties1, pattern = "T********")))

  intersects1 <- tibble(
    row_id = universe_sf$row_id,
    intersecting_county_fips = purrr::map(
      intersects_list,
      ~ counties1$county_fips[.x]))

  intersects2 <- intersects1 %>%
    mutate(
      intersecting_declaration_fips = purrr::map(
        intersecting_county_fips,
        ~ intersect(.x, declaration_county_fips)),
      intersecting_county_declarations = purrr::map(
        intersecting_declaration_fips,
        ~ county_declarations %>% filter(county_fips %in% .x)),
      intersect_hit = purrr::map_lgl(
        intersecting_declaration_fips, ~ length(.x) > 0))

  ## -- Step 2.5: Statewide — match state_list against statewide declarations --
  ## A statewide declaration (including AS/GU/MP territorial declarations,
  ## which FEMA routes as "statewide") applies to every community whose
  ## state_list includes the declared state. Aliased locally to avoid
  ## name collision with the new list-column.
  statewide_decls_input <- statewide_declarations
  statewide_states <- statewide_decls_input %>%
    distinct(state) %>%
    pull(state)

  statewide_lookup <- universe_sf %>%
    sf::st_drop_geometry() %>%
    select(row_id, state_list) %>%
    mutate(
      matched_states = purrr::map(
        state_list,
        ~ intersect(unlist(.x), statewide_states)),
      statewide_declarations = purrr::map(
        matched_states,
        ~ statewide_decls_input %>% filter(state %in% .x)),
      statewide_hit = purrr::map_lgl(
        matched_states, ~ length(.x) > 0)) %>%
    select(row_id, statewide_declarations, statewide_hit)

  ## -- Step 3: HHL + territory override — resolve via parent_county_fips -------
  ## Both recognition statuses map one-to-one onto a county-equivalent, so the
  ## county path is exact for them: a county-level declaration counts for that
  ## county-equivalent only (territorial project rule), never for a
  ## boundary-adjacent neighbor.
  parent_county_results <- universe_sf %>%
    sf::st_drop_geometry() %>%
    filter(recognition_status %in% c("hhl", "territory")) %>%
    select(row_id, parent_county_fips) %>%
    mutate(
      parent_county_declarations = purrr::map(
        parent_county_fips,
        ~ county_declarations %>% filter(county_fips == .x)),
      parent_county_hit = purrr::map_lgl(parent_county_declarations, ~ nrow(.x) > 0))

  ## -- Assemble tier + output --------------------------------------------------
  notes_for <- function(recognition_status) {
    case_when(
      recognition_status == "hhl" ~
        "Hawaiian Home Land. Native Hawaiian entities do not receive direct FEMA tribal declarations; eligibility follows the parent county.",
      recognition_status == "anrc" ~
        "Alaska Native Regional Corporation. ANCSA regional corporations do not receive direct FEMA tribal declarations; eligibility follows spatial overlap with declared areas and statewide (AK) declarations.",
      recognition_status == "territory" ~
        "Territorial government. Eligibility follows the territory's single FEMA jurisdiction.",
      TRUE ~ NA_character_)
  }

  any_program_flag <- function(decl_lists, col) {
    purrr::pmap_lgl(decl_lists, function(...) {
      any_flag_true(unlist(purrr::map(list(...), col)))
    })
  }

  assembled <- universe_sf %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    left_join(direct_match2, by = "row_id", relationship = "one-to-one") %>%
    left_join(
      intersects2 %>% select(row_id, intersecting_county_declarations, intersect_hit),
      by = "row_id", relationship = "one-to-one") %>%
    left_join(statewide_lookup, by = "row_id", relationship = "one-to-one") %>%
    left_join(
      parent_county_results %>%
        select(row_id, parent_county_declarations, parent_county_hit),
      by = "row_id", relationship = "one-to-one") %>%
    mutate(
      ## HHL + territory: resolve the intersect path via parent_county_fips.
      intersect_hit = if_else(
        recognition_status %in% c("hhl", "territory"),
        parent_county_hit, intersect_hit),
      intersecting_county_declarations = if_else(
        recognition_status %in% c("hhl", "territory"),
        parent_county_declarations,
        intersecting_county_declarations),
      ## HHL and ANRC do not receive tribal-direct declarations: suppress the
      ## direct-named tier (ANRC keeps the normal geometry-intersect path above;
      ## only HHL is rerouted through parent_county_fips).
      direct_hit = if_else(
        recognition_status %in% c("hhl", "anrc"), FALSE, direct_hit),
      direct_declarations = if_else(
        recognition_status %in% c("hhl", "anrc"),
        vector("list", nrow(.)),
        direct_declarations),
      tier = case_when(
        direct_hit ~ "direct_named_match",
        intersect_hit ~ "direct_spatial_county_match",
        statewide_hit ~ "statewide_state_match",
        TRUE ~ "no_match") %>%
        factor(
          levels = c("direct_named_match",
                     "direct_spatial_county_match",
                     "statewide_state_match",
                     "no_match"),
          ordered = TRUE),
      ia_flags_any = any_program_flag(
        list(direct_declarations, intersecting_county_declarations,
             statewide_declarations),
        "ia_program_declared"),
      pa_flags_any = any_program_flag(
        list(direct_declarations, intersecting_county_declarations,
             statewide_declarations),
        "pa_program_declared"),
      direct_match_precision_max = purrr::map_dbl(
        direct_declarations,
        function(tbl) {
          if (is.null(tbl) || nrow(tbl) == 0 ||
              !"name_match_precision" %in% names(tbl)) return(NA_real_)
          vals <- tbl$name_match_precision
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) NA_real_ else max(vals)
        }),
      notes = notes_for(recognition_status)) %>%
    select(
      community_id, name, recognition_status, geography_type, home_state,
      tier, ia_flags_any, pa_flags_any,
      direct_declarations, intersecting_county_declarations,
      statewide_declarations,
      aliases, state_list, bia_name, aiannhr, parent_county_fips,
      has_geometry, match_strategy, geometry_source,
      resolution, confidence, note,
      direct_match_precision_max, notes)

  message(
    "Eligibility tiers: ",
    str_c(
      levels(assembled$tier),
      "=",
      purrr::map_int(levels(assembled$tier), ~ sum(assembled$tier == .x)),
      collapse = ", "))

  assembled
}
