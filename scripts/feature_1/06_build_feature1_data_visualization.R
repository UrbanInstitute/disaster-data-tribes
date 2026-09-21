## ---------------------------------------------------------------------------
## 06_build_feature1_data_visualization.R — feature-1 "data viz" deliverable
##
## build_feature1_data_visualization() reshapes the resolved eligibility detail
## into a long, one-row-per-community-per-disaster table for data visualization.
## Rendered by _feature1.qmd and documented by the codebook in
## 08_get_feature1_data_visualization_codebook.R.
## ---------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(janitor)
library(here)

source(here::here("scripts", "feature_1", "format_disaster_name.R"))
source(here::here("scripts", "feature_1", "shorten_community_name.R"))

#' Build the per-community-per-disaster feature-1 data visualization dataframe
#'
#' Reshapes the resolved feature-1 eligibility detail into a long
#' table designed to back data visualization. Each row is one
#' (Indigenous community x FEMA disaster declaration) pair.
#'
#' Row universe — all matched tiers contribute (direct named,
#' direct spatial county intersect, statewide). The full pipeline window
#' of `eligibility_detail`
#' (default 2014-2026) is preserved here; no per-program window is
#' applied. Source FEMA data are already restricted to Major Disaster
#' Declarations (`declaration_type == "DR"`) upstream in
#' `refresh_fema_declarations()` and `classify_declarations()`, so this
#' function does not re-filter.
#'
#' Per-row derivations (display-formatted; these are the shipped values
#' documented by the codebook in
#' 08_get_feature1_data_visualization_codebook.R):
#' * `disaster_designated_area` —
#'     * `"Statewide declaration"` when the priority match for the pair
#'       is the statewide tier;
#'     * the FEMA `designated_area` string(s) from the matched
#'       tribal-direct row(s) when the priority match is the direct
#'       (named) tier (multiple distinct strings `"; "`-joined),
#'       parenthetical qualifiers removed and `" (Tribal area)"`
#'       appended;
#'     * otherwise, up to three intersecting `"County, ST"` labels from
#'       the community's intersect-tier match against this disaster,
#'       `"; "`-joined.
#' * `disaster_lead_applicant` — the community name when any FEMA row
#'   for the disaster carries `tribal_request == TRUE`; otherwise
#'   `"State of <name>"` for the 50 states, or the bare
#'   territory / District of Columbia name from the FEMA `state` field.
#'
#' Invariant enforced (fail-closed): every `tribal_request == TRUE` row
#' in `classified_declarations` must sit on a tribal-direct route. If a
#' future FEMA snapshot violates that, the lead-applicant derivation
#' breaks down (a tribal-filed declaration would be propagating to
#' non-filing communities via intersect/statewide tiers), and the
#' function stops.
#'
#' @param eligibility_detail Tibble from `resolve_eligibility()`. Must
#'   carry `direct_declarations`, `intersecting_county_declarations`, and
#'   `statewide_declarations` list-columns plus `community_id` and
#'   `name`.
#' @param classified_declarations Tibble from `classify_declarations()` —
#'   used for disaster-level lookups: `declaration_title`,
#'   `incident_type`, `declaration_date`, `tribal_request`, `state`, and
#'   `designated_area`.
#' @param counties_sf An `sf` of U.S. counties with `GEOID`, `NAME`, and
#'   `STUSPS` columns (i.e. `tigris::counties(cb = TRUE)`).
#'
#' @return A tibble with one row per (`community_id`, `disaster_number`)
#'   and 9 columns matching `final_columns_data_visualization` in
#'   `_feature1.qmd`: `community_id`, `community_name`,
#'   `community_name_short`, `disaster_id`, `disaster_title`,
#'   `disaster_year_declared`, `disaster_type`, `disaster_designated_area`,
#'   `disaster_lead_applicant`. `disaster_title` is display-formatted via
#'   `format_disaster_name()`; `disaster_type` is lower-cased (with
#'   "Mud/Landslide" spelled out as "mudslide or landslide");
#'   `community_name` carries the suffix "Alaska Native Regional Corporation"
#'   on ANRC rows; `community_name_short` is the bare (pre-suffix) name via
#'   `shorten_community_name()`.
build_feature1_data_visualization <- function(eligibility_detail,
                                              classified_declarations,
                                              counties_sf) {

  ## --- State / territory name lookup -----------------------------------------
  state_name_lookup <- c(
    setNames(state.name, state.abb),
    DC = "District of Columbia",
    AS = "American Samoa",
    GU = "Guam",
    MP = "Northern Mariana Islands")

  ## --- County label lookup ---------------------------------------------------
  ## Full legal county-equivalent names via TIGER NAMELSAD ("... County",
  ## "... Parish", "... Borough", "... Census Area", "... Planning Region",
  ## etc.), so the designated-area list carries each unit's proper descriptor.
  ## The one lowercase LSAD — independent cities' trailing "city" (e.g.,
  ## "Bedford city") — is capitalized for display.
  county_lookup1 <- counties_sf %>%
    sf::st_drop_geometry() %>%
    janitor::clean_names() %>%
    transmute(
      county_fips = geoid,
      county_label = str_c(
        str_replace(namelsad, " city$", " City"), ", ", stusps))

  ## --- Defensive invariant: tribal_request only on tribal_direct rows -------
  ## A tribal-requested declaration applies only to the filing tribe; it
  ## should never propagate to other communities via intersect/statewide.
  ## If this ever fires, revisit the lead-applicant branch — the
  ## community_name in a cross-route row would be the wrong filer.
  cross_route_tribal1 <- classified_declarations %>%
    filter(
      replace_na(as.logical(tribal_request), FALSE),
      declaration_route != "tribal_direct")
  if (nrow(cross_route_tribal1) > 0) {
    stop(
      "build_feature1_data_visualization(): ", nrow(cross_route_tribal1),
      " row(s) carry tribal_request == TRUE but are NOT on a ",
      "tribal_direct route. Lead-applicant derivation assumes tribal ",
      "requests never cross-propagate; re-evaluate before proceeding.",
      call. = FALSE)
  }

  ## --- Disaster-level lookup from classified declarations -------------------
  ## A FEMA `disaster_number` is single-state in the OpenFEMA
  ## DisasterDeclarationsSummaries table (different states for the "same"
  ## event get distinct disaster numbers). Multi-state aggregation
  ## therefore should not happen here; a warning fires if it does.
  multi_state_disasters1 <- classified_declarations %>%
    summarize(.by = disaster_number, n_states = n_distinct(state)) %>%
    filter(n_states > 1)
  if (nrow(multi_state_disasters1) > 0) {
    warning(
      "build_feature1_data_visualization(): ", nrow(multi_state_disasters1),
      " disaster_number(s) span multiple `state` values. Lead-applicant ",
      "will use the first state alphabetically.")
  }

  disaster_info1 <- classified_declarations %>%
    summarize(
      .by = disaster_number,
      disaster_title = dplyr::first(declaration_title),
      disaster_year_declared = as.integer(
        lubridate::year(min(declaration_date, na.rm = TRUE))),
      disaster_type = dplyr::first(incident_type),
      tribal_request_any = any(
        replace_na(as.logical(tribal_request), FALSE)),
      disaster_state = dplyr::first(sort(unique(state))))

  ## --- Long table of community-disaster matches (yes-tier sources only) -----
  keep_cols1 <- c("disaster_number", "declaration_date", "county_fips",
                  "state", "designated_area")

  extract_one_source <- function(decl_col, source_label) {
    purrr::map2(
      eligibility_detail$community_id, decl_col,
      function(cid, tbl) {
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        tbl %>%
          as_tibble() %>%
          select(any_of(keep_cols1)) %>%
          mutate(community_id = cid, match_source = source_label, .before = 1)
      }) %>%
      purrr::list_rbind()
  }

  yes_long1 <- bind_rows(
    extract_one_source(
      eligibility_detail$direct_declarations, "direct"),
    extract_one_source(
      eligibility_detail$intersecting_county_declarations, "intersect"),
    extract_one_source(
      eligibility_detail$statewide_declarations, "statewide"))

  ## --- Priority source per (community, disaster) -----------------------------
  ## direct > intersect > statewide. Used to decide the designated_area
  ## representation when a community matched a single disaster via multiple
  ## tiers (rare but possible: e.g., a tribal-direct row plus a
  ## same-state statewide row).
  source_priority <- c(direct = 1L, intersect = 2L, statewide = 3L)
  pair_priority1 <- yes_long1 %>%
    mutate(priority = source_priority[match_source]) %>%
    summarize(
      .by = c(community_id, disaster_number),
      priority_source = match_source[which.min(priority)])

  ## --- Intersecting counties per (community, disaster) ----------------------
  ## Up to three county labels, drawn from intersect-tier rows in the
  ## eligibility_detail list-col (statewide / direct rows lack
  ## `county_fips` or have a "000" suffix that won't match).
  intersect_counties1 <- yes_long1 %>%
    filter(match_source == "intersect", !is.na(county_fips)) %>%
    distinct(community_id, disaster_number, county_fips) %>%
    left_join(county_lookup1, by = "county_fips",
              relationship = "many-to-one") %>%
    filter(!is.na(county_label)) %>%
    arrange(community_id, disaster_number, county_label) %>%
    summarize(
      .by = c(community_id, disaster_number),
      intersect_counties_label = str_c(head(county_label, 3), collapse = "; "))

  ## --- Direct-tier designated_area per (community, disaster) ----------------
  ## When a community matched a disaster via the direct (named) tier, the
  ## matched tribal-direct row(s) carry FEMA's free-text designated_area
  ## for the tribal entity (e.g., "Burns Paiute Indian Reservation"). A
  ## single community-disaster pair can resolve to multiple distinct
  ## designated_area strings (e.g., several reservations); join them
  ## with "; ".
  direct_designated1 <- yes_long1 %>%
    filter(match_source == "direct", !is.na(designated_area)) %>%
    distinct(community_id, disaster_number, designated_area) %>%
    arrange(community_id, disaster_number, designated_area) %>%
    summarize(
      .by = c(community_id, disaster_number),
      direct_designated_area = str_c(designated_area, collapse = "; "))

  ## --- Identifier columns ----------------------------------------------------
  identifiers1 <- eligibility_detail %>%
    transmute(community_id, community_name = name)

  ## --- Assemble -------------------------------------------------------------
  result1 <- yes_long1 %>%
    distinct(community_id, disaster_number) %>%
    left_join(pair_priority1,
              by = c("community_id", "disaster_number"),
              relationship = "one-to-one") %>%
    left_join(identifiers1, by = "community_id",
              relationship = "many-to-one") %>%
    left_join(disaster_info1, by = "disaster_number",
              relationship = "many-to-one") %>%
    left_join(intersect_counties1,
              by = c("community_id", "disaster_number"),
              relationship = "one-to-one") %>%
    left_join(direct_designated1,
              by = c("community_id", "disaster_number"),
              relationship = "one-to-one") %>%
    mutate(
      disaster_designated_area = case_when(
        priority_source == "statewide" ~ "Statewide",
        priority_source == "direct" ~ direct_designated_area,
        !is.na(intersect_counties_label) ~ intersect_counties_label,
        .default = NA_character_),
      disaster_lead_applicant = if_else(
        tribal_request_any,
        community_name,
        unname(state_name_lookup[disaster_state])))

  unmapped_states1 <- result1 %>%
    filter(
      !tribal_request_any,
      !is.na(disaster_state),
      is.na(disaster_lead_applicant))
  if (nrow(unmapped_states1) > 0) {
    warning(
      "build_feature1_data_visualization(): ", nrow(unmapped_states1),
      " row(s) had a FEMA `state` code outside the state/territory ",
      "lookup; lead_applicant is NA. Unmapped codes: ",
      str_c(sort(unique(unmapped_states1$disaster_state)),
            collapse = ", "))
  }

  result1 %>%
    mutate(
      ## Short name is computed from the bare name BEFORE the ANRC suffix is
      ## appended: only the long display name carries the entity type.
      community_name_short = shorten_community_name(community_name),
      community_name = if_else(
        str_starts(community_id, "anrc_"),
        str_c(community_name, " Alaska Native Regional Corporation"),
        community_name)) %>%
    transmute(
      community_id,
      community_name,
      community_name_short,
      disaster_id = disaster_number,
      disaster_title = format_disaster_name(disaster_title),
      disaster_year_declared,
      ## FEMA incident type, lower-cased; the one slashed value
      ## ("Mud/Landslide") is spelled out, and any other "/" becomes " or ".
      disaster_type = disaster_type %>%
        str_to_lower() %>%
        str_replace_all(fixed("mud/landslide"), "mudslide or landslide") %>%
        str_replace_all("/", " or "),
      ## Display formatting (previously applied post-hoc in _feature1.qmd):
      ## these are the shipped values, and the codebook in
      ## 08_get_feature1_data_visualization_codebook.R describes them.
      ## Statewide-tier rows read "Statewide declaration"; direct-tier rows
      ## are the FEMA tribal designated area with parenthetical qualifiers
      ## removed and " (Tribal area)" appended — keyed on priority_source
      ## rather than on the absence of a ", ST" county suffix; intersect-tier
      ## rows keep their "County, ST" labels.
      disaster_designated_area = case_when(
        priority_source == "statewide" ~ "Statewide declaration",
        priority_source == "direct" & !is.na(disaster_designated_area) ~
          disaster_designated_area %>%
            str_remove_all("\\([^)]*\\)") %>%
            str_squish() %>%
            str_c(" (Tribal area)"),
        .default = disaster_designated_area),
      ## "State of <name>" for the 50 states; DC and the territory names
      ## (American Samoa, Guam, Northern Mariana Islands) are already
      ## self-describing and stay bare, as does the community name on
      ## tribally-filed rows.
      disaster_lead_applicant = if_else(
        disaster_lead_applicant %in% state.name,
        str_c("State of ", disaster_lead_applicant),
        disaster_lead_applicant)) %>%
    arrange(community_id, desc(disaster_year_declared), disaster_id)
}
