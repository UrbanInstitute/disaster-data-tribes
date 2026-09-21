## ---------------------------------------------------------------------------
## 02_get_indigenous_universe.R — feature-1 stage 2: community universe
##
## get_indigenous_universe() assembles the full universe of Indigenous
## communities — federal tribes/ANVs (via build_indigenous_tribe_polygons.R),
## Hawaiian Home Lands, Alaska Native Regional Corporations, and the AS/GU/MP
## territories — and attaches each community's home state and state_list.
## Sourced by 00_run_feature_1.R; see universe-definition.md for edge cases.
## ---------------------------------------------------------------------------
library(arrow)
library(tidyverse)
library(sf)
library(tigris)
library(esri2sf)
library(janitor)
library(here)

source(here::here("scripts", "utilities", "utilities.R"))
## Sources compare_bia_tigris_native_areas.R + generate_indigenous_universe.R and
## defines get_federal_tribe_rows(), get_bia_tigris_native_areas_crosswalk(),
## bia_tigris_match_corrections, build_indigenous_tribe_polygons(), and
## assemble_indigenous_tribe_polygons().
source(here::here("scripts", "utilities", "build_indigenous_tribe_polygons.R"))

options(tigris_use_cache = TRUE)

## Columns every universe subset emits (federal / hhl / anrc / territory). The
## federal subset is produced by build_indigenous_tribe_polygons(); its audited
## provenance lives in `resolution` / `confidence` / `note` (the old
## directory-substring diagnostics — name_match_coverage_*, name_match_distance,
## nearest_native_area_* — were retired with the substring matcher).
universe_cols <- c(
  "community_id", "name", "aliases", "recognition_status", "geography_type",
  "home_state", "state_list", "bia_name", "aiannhr", "parent_county_fips",
  "has_geometry", "match_strategy", "geometry_source",
  "resolution", "confidence", "note")

## Manual FR-name -> BIA-Directory home-state overrides. Populated for any FR
## tribe whose name does not resolve to a BIA Tribal Leaders Directory entry by
## normalized-key / substring matching (see the residual warning in
## attach_directory_home_state()). Two-letter USPS codes.
federal_home_state_overrides <- tibble::tribble(
  ~tribe_name,                 ~home_state,
  "Native Village of Akhiok",  "AK")

#' Attach a BIA-Directory home state to each FR tribe (helper)
#'
#' The FR-2026 list carries only region (Alaska / contiguous), not a state. The
#' BIA Tribal Leaders Directory feature service carries a `state` field; this
#' joins it to FR `tribe_name` by normalized name key (exact, then bidirectional
#' substring on the residuals), then applies `federal_home_state_overrides` for
#' anything still unresolved. Warns (does not error) on remaining residuals.
#'
#' @param tribe_names Character vector of FR `tribe_name` values.
#' @param year Integer TIGER/Directory vintage (unused by the service call but
#'   kept for signature symmetry).
#' @return Character vector of two-letter USPS codes, same length/order as
#'   `tribe_names` (`NA` where unresolved).
attach_directory_home_state <- function(tribe_names, year = 2024) {
  bia_url <- str_c(
    "https://services1.arcgis.com/UxqqIfhng71wUT9x/arcgis/rest/services/",
    "TribalLeadership_Directory/FeatureServer/0")

  ## Directory `state` values are usually full state names, but be robust to
  ## postal abbreviations and DC/territory rows, which `state.name` alone
  ## would silently drop to NA.
  state_to_postal <- c(
    set_names(state.abb, state.name),
    "District of Columbia" = "DC",
    "American Samoa" = "AS",
    "Guam" = "GU",
    "Commonwealth of the Northern Mariana Islands" = "MP",
    "Northern Mariana Islands" = "MP",
    "Puerto Rico" = "PR",
    "U.S. Virgin Islands" = "VI",
    "Virgin Islands" = "VI")
  valid_postal <- unique(unname(state_to_postal))

  dir_raw <- esri2sf::esri2sf(bia_url) %>%
    janitor::clean_names() %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    transmute(
      dir_name = tribefullname,
      dir_state_raw = str_trim(state),
      dir_state_postal = case_when(
        str_to_upper(dir_state_raw) %in% valid_postal ~
          str_to_upper(dir_state_raw),
        .default = unname(state_to_postal[dir_state_raw]))) %>%
    select(-dir_state_raw) %>%
    filter(!is.na(dir_name), !is.na(dir_state_postal)) %>%
    mutate(dir_key = normalize_name_key(dir_name)) %>%
    distinct(dir_key, .keep_all = TRUE)

  tribe_key <- normalize_name_key(tribe_names)

  ## Exact normalized-key match.
  home_state <- dir_raw$dir_state_postal[match(tribe_key, dir_raw$dir_key)]

  ## Residuals: best bidirectional-substring directory key.
  resid <- which(is.na(home_state) & !is.na(tribe_key) & nchar(tribe_key) >= 3)
  for (i in resid) {
    hits <- which(bidirectional_substring_match(tribe_key[i], dir_raw$dir_key))
    if (length(hits) == 0) next
    cov <- name_key_coverage(tribe_key[i], dir_raw$dir_key[hits])
    home_state[i] <- dir_raw$dir_state_postal[hits[which.max(cov)]]
  }

  ## Manual overrides for anything still unresolved.
  ovr <- federal_home_state_overrides$home_state[
    match(tribe_names, federal_home_state_overrides$tribe_name)]
  home_state <- coalesce(home_state, ovr)

  n_resid <- sum(is.na(home_state))
  if (n_resid > 0) {
    warning(
      "attach_directory_home_state: ", n_resid,
      " FR tribe(s) have no BIA-Directory home state (add to ",
      "federal_home_state_overrides): ",
      str_c(head(tribe_names[is.na(home_state)], 8), collapse = "; "),
      if (n_resid > 8) ", ..." else "", call. = FALSE)
  }
  home_state
}

#' Per-row state set from a tribal geometry (helper)
#'
#' For each (non-empty) geometry, the set of states whose intersection with the
#' geometry is either >= `acres_threshold` acres or >= 99.9% of the geometry's
#' area. Mirrors the FEMA-routing `state_list` rule used previously. Empty
#' geometries return `character(0)` (caller supplies the home-state fallback).
#'
#' @param geom_sf An `sf` with a single geometry column (EPSG:4269).
#' @param states_proj_sf States layer in the project equal-area CRS (`CRS_PROJ`, EPSG:6933) with `state_postal`.
#' @return A list-column (length `nrow(geom_sf)`) of USPS-code character vectors.
state_list_from_geometry <- function(geom_sf, states_proj_sf) {
  acres_threshold_m2 <- 100 * 4046.8564224
  containment_ratio <- 0.999

  rows <- geom_sf %>%
    mutate(row_id = row_number()) %>%
    select(row_id)
  nonempty_idx <- which(!sf::st_is_empty(rows))
  out <- rep(list(character(0)), nrow(rows))
  if (length(nonempty_idx) == 0) return(out)

  nonempty <- rows[nonempty_idx, ] %>%
    sf::st_transform(CRS_PROJ) %>%
    sf::st_make_valid()
  row_area <- tibble(
    row_id = nonempty$row_id,
    row_area_m2 = as.numeric(sf::st_area(nonempty)))

  inter <- suppressWarnings(
      sf::st_intersection(nonempty, states_proj_sf)) %>%
    sf::st_make_valid() %>%
    mutate(intersect_area_m2 = as.numeric(sf::st_area(geometry))) %>%
    sf::st_drop_geometry() %>%
    left_join(row_area, by = "row_id") %>%
    filter(
      intersect_area_m2 >= acres_threshold_m2 |
        (row_area_m2 > 0 & intersect_area_m2 / row_area_m2 >= containment_ratio)) %>%
    summarize(states = list(sort(unique(state_postal))), .by = row_id)

  for (k in seq_len(nrow(inter))) {
    out[[inter$row_id[k]]] <- inter$states[[k]]
  }
  out
}

#' Build the unified Indigenous community universe
#'
#' Assembles four subsets into one `sf` tibble keyed by `community_id`:
#' \describe{
#'   \item{federal}{One row per BIA Federal-Register tribe (Tribes and ANVs),
#'     from `assemble_indigenous_tribe_polygons()` — the FR-2026-authoritative,
#'     audited universe (one unioned `tigris::native_areas()` polygon per tribe;
#'     landless tribes carry empty geometry). The 5 `tigris_no_tribe` polygons
#'     are dropped (not communities). `home_state` is joined from the BIA Tribal
#'     Leaders Directory; `state_list` is derived from the unioned geometry.}
#'   \item{hhl}{`tigris::native_areas()` Hawaiian Home Land rows intersected
#'     with HI counties — one row per HHL x county.}
#'   \item{anrc}{`tigris::alaska_native_regional_corporations()` — one row per
#'     land-based regional corporation. Resolves via spatial overlap + AK
#'     statewide only (no tribal-direct declarations).}
#'   \item{territory}{AS / GU / MP county-equivalents from `tigris::counties()`,
#'     one row each (PR / USVI excluded).}
#' }
#'
#' @param year Integer. TIGER vintage. Default `2024`.
#' @return An `sf` tibble in EPSG:4269, one row per Indigenous community, with
#'   columns `community_id`, `name`, `aliases`, `recognition_status`
#'   (`federal` / `hhl` / `anrc` / `territory`), `geography_type`, `home_state`,
#'   `state_list`, `bia_name`, `aiannhr`, `parent_county_fips`, `has_geometry`,
#'   `match_strategy`, `geometry_source`, `resolution`, `confidence`, `note`,
#'   and `geometry`.
get_indigenous_universe <- function(year = 2024) {

  prep_tigris_sf <- function(x) {
    x %>%
      janitor::clean_names() %>%
      sf::st_transform(CRS_GEO) %>%
      sf::st_make_valid()
  }

  states_proj_sf <- tigris::states(year = year, cb = TRUE) %>%
    prep_tigris_sf() %>%
    select(state_postal = stusps, geometry) %>%
    sf::st_transform(CRS_PROJ) %>%
    sf::st_make_valid()

  ## -- Federal: one audited polygon per FR tribe ------------------------------
  tribes <- assemble_indigenous_tribe_polygons(year = year) %>%
    filter(record_type == "federally_recognized_tribe") %>%
    sf::st_transform(CRS_GEO) %>%
    sf::st_make_valid() %>%
    arrange(tribe_name)

  home_state_vec <- attach_directory_home_state(tribes$tribe_name, year = year)
  geom_states <- state_list_from_geometry(tribes, states_proj_sf)

  federal1 <- tribes %>%
    mutate(
      community_id = str_c("fed_", str_pad(row_number(), 4, pad = "0")),
      ## Display normalization of two verbatim BIA Federal Register quirks:
      ## the missing space in "PuliklaTribe of Yurok People ...", and the
      ## leading "The" carried by five FR entries (Chickasaw, Choctaw Nation
      ## of Oklahoma, Muscogee (Creek) Nation, Osage Nation, and Seminole
      ## Nation of Oklahoma).
      ##
      ## Both happen here, and not upstream, because every name-keyed match —
      ## including attach_directory_home_state() above — keys on the verbatim
      ## FR string. Renaming earlier changes the normalized match key and
      ## silently costs the tribe its home state. The direct-name match in
      ## resolve_eligibility() runs downstream of this and does key on `name`,
      ## but normalize_name_key() discards "THE" as a stopword, so the
      ## canonical key and the resulting matches are unchanged.
      tribe_name_display = tribe_name %>%
        str_replace("^PuliklaTribe\\b", "Pulikla Tribe") %>%
        str_replace("^The\\s+", ""),
      name = tribe_name_display,
      bia_name = tribe_name_display,
      aliases = purrr::map2(
        tribe_name_display, tigris_names,
        ~ unique(discard(c(.x, if (is.na(.y)) NULL else str_split_1(.y, "; ")),
                         is.na))),
      recognition_status = "federal",
      geography_type = "bia_federal",
      home_state = home_state_vec,
      state_list = purrr::pmap(
        list(geom_states, has_polygon, home_state),
        function(derived, has_geom, hs) {
          if (isTRUE(has_geom) && length(derived) > 0) sort(unique(derived))
          else if (!is.na(hs)) hs
          else character(0)
        }),
      aiannhr = "F",
      parent_county_fips = NA_character_,
      has_geometry = has_polygon,
      match_strategy = resolution,
      geometry_source = if_else(has_polygon, "native_areas", NA_character_)) %>%
    select(all_of(universe_cols), geometry)

  message(
    "Federal subset: ", nrow(federal1), " FR tribes (",
    sum(federal1$has_geometry), " with a polygon, ",
    sum(!federal1$has_geometry), " landless).")

  ## -- Hawaiian Home Lands: one row per HHL-county intersection ----------------
  native_all <- tigris::native_areas(year = year, cb = FALSE) %>% prep_tigris_sf()
  hhl_rows <- native_all %>% filter(str_detect(namelsad, "Hawaii"))

  hi_counties <- tigris::counties(state = "HI", year = year, cb = FALSE) %>%
    prep_tigris_sf() %>%
    select(county_fips = geoid, county_name = name, geometry)

  ## Keep only substantive HHL x county pieces, mirroring the
  ## state_list_from_geometry() sliver rule: an intersection counts if it is
  ## >= 100 acres or >= 99.9% of the HHL polygon's area (a small HHL fully
  ## within one county) — not any positive-area boundary sliver.
  hhl_sliver_threshold_m2 <- 100 * 4046.8564224
  hhl_containment_ratio <- 0.999

  hhl_total_areas <- hhl_rows %>%
    sf::st_transform(CRS_PROJ) %>%
    mutate(hhl_area_m2 = as.numeric(sf::st_area(geometry))) %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    select(geoid, hhl_area_m2)

  hhl1 <- hhl_rows %>%
    select(geoid, name, namelsad, aiannhr, geometry) %>%
    sf::st_intersection(hi_counties) %>%
    tidylog::left_join(hhl_total_areas, by = "geoid",
                       relationship = "many-to-one") %>%
    mutate(intersect_area_m2 = as.numeric(
      sf::st_area(sf::st_transform(geometry, CRS_PROJ)))) %>%
    filter(
      intersect_area_m2 >= hhl_sliver_threshold_m2 |
        (hhl_area_m2 > 0 &
           intersect_area_m2 / hhl_area_m2 >= hhl_containment_ratio)) %>%
    mutate(
      community_id = str_c("hhl_", geoid, "_", county_fips),
      recognition_status = "hhl",
      geography_type = "hhl",
      bia_name = NA_character_,
      name_display = str_c(name, " (", county_name, " County)"),
      aliases = purrr::map2(name, county_name, ~ c(.x, str_c(.x, ", ", .y))),
      home_state = "HI",
      state_list = rep(list("HI"), nrow(.)),
      parent_county_fips = county_fips,
      has_geometry = TRUE,
      match_strategy = "tigris_hhl_x_county",
      geometry_source = "native_areas",
      resolution = NA_character_,
      confidence = NA_character_,
      note = NA_character_,
      name = name_display) %>%
    select(all_of(universe_cols), geometry)

  message("Hawaiian Home Lands: ", nrow(hhl1), " HHL-county rows.")

  ## -- ANRCs: one row per land-based regional corporation ----------------------
  ## Routed via spatial overlap + AK statewide only (resolve_eligibility() skips
  ## the direct-named tier for recognition_status == "anrc").
  anrc1 <- tigris::alaska_native_regional_corporations(year = year) %>%
    prep_tigris_sf() %>%
    filter(!sf::st_is_empty(geometry)) %>%
    mutate(
      community_id = str_c("anrc_", geoid),
      aliases = purrr::map(name, ~ .x),
      recognition_status = "anrc",
      geography_type = "anrc",
      bia_name = NA_character_,
      home_state = "AK",
      state_list = rep(list("AK"), nrow(.)),
      aiannhr = NA_character_,
      parent_county_fips = NA_character_,
      has_geometry = TRUE,
      match_strategy = "tigris_anrc",
      geometry_source = "alaska_native_regional_corporations",
      resolution = NA_character_,
      confidence = NA_character_,
      note = NA_character_) %>%
    select(all_of(universe_cols), geometry)

  message("ANRCs: ", nrow(anrc1), " land-based regional corporations.")

  ## -- Territorial governments: AS, GU, MP -------------------------------------
  ## One row per county-equivalent (Guam 1, American Samoa 5, CNMI 4). Keeps
  ## district-specific FEMA declarations scoped; territory-wide declarations
  ## propagate via state_list. PR / USVI excluded.
  territory_abbrs <- c("AS", "GU", "MP")
  territory_names <- c(
    AS = "American Samoa",
    GU = "Guam",
    MP = "Commonwealth of the Northern Mariana Islands")

  territory_counties <- purrr::map(
      territory_abbrs,
      ~ tigris::counties(state = .x, year = year, cb = FALSE) %>%
        janitor::clean_names() %>%
        mutate(state_abbr = .x)) %>%
    bind_rows() %>%
    sf::st_transform(CRS_GEO) %>%
    sf::st_make_valid()

  territory1 <- territory_counties %>%
    mutate(
      county_name_orig = name,
      full_territory_name = unname(territory_names[state_abbr]),
      community_id = str_c("territory_", state_abbr, "_", geoid),
      name = str_c(full_territory_name, " — ", county_name_orig),
      aliases = purrr::pmap(
        list(full_territory_name, state_abbr, county_name_orig, name),
        function(ft, abbr, cn, nm) unique(c(ft, abbr, cn, nm))),
      recognition_status = "territory",
      geography_type = "territory",
      bia_name = NA_character_,
      home_state = state_abbr,
      state_list = as.list(state_abbr),
      aiannhr = NA_character_,
      parent_county_fips = geoid,
      has_geometry = TRUE,
      match_strategy = "tigris_territory_county_equivalent",
      geometry_source = "counties",
      resolution = NA_character_,
      confidence = NA_character_,
      note = NA_character_) %>%
    select(all_of(universe_cols), geometry)

  territory_counts <- territory1 %>%
    sf::st_drop_geometry() %>%
    count(home_state)
  message(
    "Territories: ", nrow(territory1), " county-equivalent row(s) (",
    str_c(territory_counts$home_state, "=", territory_counts$n, collapse = ", "),
    ").")

  ## -- Bind all subsets --------------------------------------------------------
  universe <- bind_rows(federal1, hhl1, anrc1, territory1) %>%
    sf::st_as_sf()

  message(
    "Unified Indigenous universe: ", nrow(universe), " rows (",
    sum(universe$recognition_status == "federal"), " federal, ",
    sum(universe$recognition_status == "hhl"), " HHL, ",
    sum(universe$recognition_status == "anrc"), " ANRC, ",
    sum(universe$recognition_status == "territory"), " territorial); ",
    sum(!universe$has_geometry), " rows have no geometry.")

  universe
}
