## ---------------------------------------------------------------------------
## 05_build_feature1_search.R — feature-1 "search" deliverable
##
## build_feature1_search() reshapes the resolved eligibility detail into the
## flat, one-row-per-community search dataset (eligibility flags plus EDA /
## HMGP / SBA program windows). Rendered by _feature1.qmd and documented by the
## codebook in 07_get_feature1_search_codebook.R.
## ---------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(janitor)
library(here)

source(here::here("scripts", "feature_1", "04_get_eda_edrs.R"))
source(here::here("scripts", "feature_1", "format_disaster_name.R"))
source(here::here("scripts", "feature_1", "shorten_community_name.R"))

#' Build the per-community feature-1 search dataframe
#'
#' Reshapes the resolved feature-1 eligibility detail into a flat
#' community-level table designed to back a search UI. Each row is one
#' Indigenous community.
#'
#' The pipeline upstream of this function pulls a wide window of FEMA
#' declarations (default `2014:2026`). This function then applies a
#' separate calendar-year window per program, so a community can be
#' "yes" for one program but "no" for another even though its underlying
#' spatial matches span all of 2014-2026.
#'
#' Program-window logic, applied to declarations in the
#' `direct_declarations` / `intersecting_county_declarations` /
#' `statewide_declarations` list-columns:
#' * **EDA factor / declaration slots** consider declarations with
#'   `year(declaration_date) %in% eda_window`. Factor is `yes` if any
#'   in-window declaration appears in the direct / intersect / statewide
#'   list-cols; `no` otherwise. The three slot columns
#'   (`*_1`, `*_2`, `*_3`) hold the three most recent in-window
#'   declarations by `declaration_date`. Each slot exposes the
#'   declaration name, calendar year, FEMA `disaster_number`, and
#'   intersecting-county label.
#' * **FEMA HMGP columns** consider declarations from the
#'   direct / intersect / statewide list-cols with
#'   `year(declaration_date) %in% hmgp_window` and
#'   `hm_program_declared == TRUE`.
#'   The binary is `yes` if any in-window flagged declaration exists;
#'   the lead-applicant declaration binary is `yes` when a tribal-direct
#'   declaration carries the flag; the state-year column reports the
#'   most recent calendar year of an intersect/statewide flagged
#'   declaration in window.
#' * **SBA** is `yes` if any direct / intersect / statewide declaration
#'   has `incident_end_date` (looked up from `classified_declarations`
#'   by `disaster_number`) in `sba_window`. The window applies to
#'   incident-end date, not declaration date — matching SBA's
#'   active-loan semantics.
#'
#' Counties listed per EDA slot are up to three distinct counties from
#' the community's spatial intersect with that declaration, formatted
#' `"<County>, <ST>"` and joined with `"; "`. Tribal-direct and
#' statewide declarations have no county granularity, so their slot is
#' `NA`.
#'
#' @param eligibility_detail Tibble from `resolve_eligibility()` — must
#'   carry the three list-columns plus `community_id`, `name`, and
#'   `home_state`. The `tier` column is **not** consulted by this
#'   function; per-program eligibility is re-derived from the list-cols
#'   under each window so the wide-pipeline `tier` does not contaminate
#'   the narrower program views.
#' @param classified_declarations Tibble from `classify_declarations()` —
#'   used only for the `disaster_number` -> `incident_end_date` lookup
#'   that backs the SBA column.
#' @param counties_sf An `sf` of U.S. counties with `GEOID`, `NAME`, and
#'   `STUSPS` columns (i.e. `tigris::counties(cb = TRUE)`).
#' @param eda_edrs Tibble of EDA EDR contacts (default `get_eda_edrs()`).
#' @param eda_window,hmgp_window Integer vectors of calendar
#'   years applied to `year(declaration_date)` for each program.
#' @param sba_window Integer vector of calendar years applied to
#'   `year(incident_end_date)`.
#'
#' @return A tibble with one row per `community_id` and 23 columns
#'   matching the feature-1 search-bar specification in `_feature1.qmd`.
#'   `community_name` carries the suffix "Alaska Native Regional Corporation"
#'   on ANRC rows; `community_name_short` is the bare (pre-suffix) name
#'   shortened for narrow screens via `shorten_community_name()`; the three
#'   `eda_declaration_name_*` slots are display-formatted via
#'   `format_disaster_name()`.
build_feature1_search <- function(eligibility_detail,
                                  classified_declarations,
                                  counties_sf,
                                  eda_edrs = get_eda_edrs(),
                                  eda_window = 2023:2024,
                                  hmgp_window = 2025:2026,
                                  sba_window = 2025:2026) {

  ## --- State normalization ----------------------------------------------------
  ## `home_state` is a mix of postal abbreviations and full state names in
  ## the universe; the EDR crosswalk is keyed on postal abbreviations, so
  ## we coerce to a single form before joining.
  state_lookup <- c(
    setNames(state.abb, state.abb),
    setNames(state.abb, state.name),
    DC = "DC", AS = "AS", GU = "GU", MP = "MP",
    "District of Columbia" = "DC",
    "American Samoa" = "AS",
    "Guam" = "GU",
    "Northern Mariana Islands" = "MP",
    "Commonwealth of the Northern Mariana Islands" = "MP")

  ## --- County label lookup ----------------------------------------------------
  county_lookup1 <- counties_sf %>%
    sf::st_drop_geometry() %>%
    janitor::clean_names() %>%
    transmute(
      county_fips = geoid,
      county_label = str_c(name, ", ", stusps))

  ## --- Disaster end-date lookup (for SBA) ------------------------------------
  ## `incident_end_date` lives in `classified_declarations`, not in the
  ## aggregated declaration list-cols. One disaster -> one end date.
  disaster_end1 <- classified_declarations %>%
    filter(!is.na(incident_end_date)) %>%
    summarize(
      .by = disaster_number,
      incident_end_date = max(incident_end_date))

  ## --- Identifier columns -----------------------------------------------------
  base1 <- eligibility_detail %>%
    transmute(
      community_id,
      community_name = name,
      community_state = unname(state_lookup[home_state]))

  if (any(is.na(base1$community_state) & !is.na(eligibility_detail$home_state))) {
    unmapped <- eligibility_detail$home_state[
      is.na(base1$community_state) & !is.na(eligibility_detail$home_state)]
    warning(
      "build_feature1_search(): unmapped home_state values dropped to NA: ",
      str_c(unique(unmapped), collapse = ", "))
  }

  ## --- EDR join (collapse multi-EDR states) ----------------------------------
  ## Collapse name and email together off the same distinct (name, email)
  ## pairs so the i-th email in `community_edr_email` aligns with the i-th
  ## name in `community_edr`.
  edr_per_state1 <- eda_edrs %>%
    distinct(state, edr_name, edr_email) %>%
    arrange(state, edr_name) %>%
    summarize(
      .by = state,
      community_edr = str_c(edr_name, collapse = "; "),
      community_edr_email = str_c(edr_email, collapse = "; "))

  base2 <- base1 %>%
    left_join(
      edr_per_state1,
      by = c("community_state" = "state"),
      relationship = "many-to-one")

  ## --- Long table of declarations (one row per community-declaration) --------
  ## `pivot_longer` can't combine the three list-cols directly because each
  ## carries inner tibbles with a different column schema. Stack them via
  ## list_rbind after projecting onto the common column set we need.
  keep_cols1 <- c("disaster_number", "declaration_date", "declaration_title",
                  "county_fips", "state",
                  "hm_program_declared",
                  "ia_program_declared", "ih_program_declared")

  extract_one_source <- function(decl_col, source_label) {
    purrr::map2(
      eligibility_detail$community_id, decl_col,
      function(cid, tbl) {
        if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
        tbl %>%
          as_tibble() %>%
          select(any_of(keep_cols1)) %>%
          mutate(community_id = cid, source = source_label, .before = 1)
      }) %>%
      purrr::list_rbind()
  }

  decl_long1 <- bind_rows(
    extract_one_source(eligibility_detail$direct_declarations,
                       "direct_declarations"),
    extract_one_source(eligibility_detail$intersecting_county_declarations,
                       "intersecting_county_declarations"),
    extract_one_source(eligibility_detail$statewide_declarations,
                       "statewide_declarations")) %>%
    mutate(decl_year = as.integer(lubridate::year(declaration_date)))

  yes_sources1 <- c("direct_declarations",
                    "intersecting_county_declarations",
                    "statewide_declarations")

  ## --- EDA factor (re-derived within eda_window) -----------------------------
  ## Any direct/intersect/statewide in-window declaration -> "yes";
  ## otherwise "no".
  eda_decls1 <- decl_long1 %>%
    filter(decl_year %in% eda_window)

  eda_factor1 <- tibble(community_id = eligibility_detail$community_id) %>%
    left_join(
      eda_decls1 %>%
        summarize(
          .by = community_id,
          any_yes_source = any(source %in% yes_sources1)),
      by = "community_id",
      relationship = "one-to-one") %>%
    mutate(
      any_yes_source = replace_na(any_yes_source, FALSE),
      eda_factor_eligibility = if_else(any_yes_source, "yes", "no"))

  ## --- EDA declarations: top 3 in-window per community by date ---------------
  eda_eligible1 <- eda_decls1 %>%
    filter(source %in% yes_sources1)

  ## Per-disaster county labels (only county-level list-cols carry
  ## `county_fips`; tribal-direct and statewide are NA there).
  eda_counties1 <- eda_eligible1 %>%
    filter(!is.na(county_fips)) %>%
    distinct(community_id, disaster_number, county_fips) %>%
    left_join(county_lookup1, by = "county_fips",
              relationship = "many-to-one") %>%
    filter(!is.na(county_label)) %>%
    arrange(community_id, disaster_number, county_label) %>%
    summarize(
      .by = c(community_id, disaster_number),
      counties_label = str_c(head(county_label, 3), collapse = "; "))

  eda_top3 <- eda_eligible1 %>%
    summarize(
      .by = c(community_id, disaster_number),
      declaration_title = dplyr::first(declaration_title),
      declaration_date = dplyr::first(declaration_date)) %>%
    left_join(eda_counties1,
              by = c("community_id", "disaster_number"),
              relationship = "one-to-one") %>%
    arrange(community_id, desc(declaration_date), disaster_number) %>%
    slice_head(n = 3, by = community_id) %>%
    mutate(slot = row_number(), .by = community_id) %>%
    mutate(declaration_year = as.integer(lubridate::year(declaration_date)))

  ## Force a 3-slot wide layout even when no community has 3 disasters.
  slots_template1 <- expand_grid(
    community_id = unique(eligibility_detail$community_id),
    slot = 1:3)

  eda_wide1 <- slots_template1 %>%
    left_join(
      eda_top3 %>% select(community_id, slot, declaration_title,
                          declaration_year, disaster_number, counties_label),
      by = c("community_id", "slot"),
      relationship = "one-to-one") %>%
    pivot_wider(
      id_cols = community_id,
      names_from = slot,
      values_from = c(declaration_title, declaration_year,
                      disaster_number, counties_label),
      names_glue = "{.value}_{slot}") %>%
    rename_with(
      .cols = matches("^declaration_title_"),
      .fn = ~ str_replace(
        .x, "^declaration_title",
        "eda_declaration_name")) %>%
    rename_with(
      .cols = matches("^declaration_year_"),
      .fn = ~ str_replace(
        .x, "^declaration_year",
        "eda_declaration_year")) %>%
    rename_with(
      .cols = matches("^disaster_number_"),
      .fn = ~ str_replace(
        .x, "^disaster_number",
        "eda_declaration_disaster_number")) %>%
    rename_with(
      .cols = matches("^counties_label_"),
      .fn = ~ str_replace(
        .x, "^counties_label",
        "eda_spatial_intersection_counties"))

  ## --- HMGP / SBA per-community flags ----------------------------------------
  ## Each program restricted to the right window applied to the right date
  ## column (declaration_date for HMGP, incident_end_date for SBA).
  decl_qualifying1 <- decl_long1 %>%
    filter(source %in% yes_sources1) %>%
    left_join(disaster_end1, by = "disaster_number",
              relationship = "many-to-one") %>%
    mutate(
      is_direct = source == "direct_declarations",
      end_year  = as.integer(lubridate::year(incident_end_date)),
      hm_yes = replace_na(hm_program_declared, FALSE),
      in_hmgp_window = decl_year %in% hmgp_window,
      in_sba_window  = end_year %in% sba_window)

  flag_summary1 <- decl_qualifying1 %>%
    summarize(
      .by = community_id,
      hm_any_direct = any(is_direct & hm_yes & in_hmgp_window),
      hm_any_state  = any(!is_direct & hm_yes & in_hmgp_window),
      hm_state_year_max = {
        v <- decl_year[!is_direct & hm_yes & in_hmgp_window]
        if (length(v) == 0) NA_integer_ else max(v, na.rm = TRUE)
      },
      sba_any = any(in_sba_window, na.rm = TRUE))

  flags_per_community1 <- tibble(
      community_id = base1$community_id) %>%
    left_join(flag_summary1, by = "community_id",
              relationship = "one-to-one") %>%
    mutate(
      across(
        c(hm_any_direct, hm_any_state, sba_any),
        ~ replace_na(.x, FALSE)),
      fema_hmgp_binary =
        if_else(hm_any_direct | hm_any_state, "yes", "no"),
      fema_hmgp_lead_applicant_declaration_binary =
        if_else(hm_any_direct, "yes", "no"),
      fema_hmgp_lead_applicant_state_year =
        hm_state_year_max,
      sba_binary =
        if_else(sba_any, "yes", "no")) %>%
    select(
      community_id,
      fema_hmgp_binary,
      fema_hmgp_lead_applicant_declaration_binary,
      fema_hmgp_lead_applicant_state_year,
      sba_binary)

  ## --- Assemble in spec order ------------------------------------------------
  base2 %>%
    left_join(
      eda_factor1 %>% select(community_id, eda_factor_eligibility),
      by = "community_id",
      relationship = "one-to-one") %>%
    select(community_id, community_name, community_state, community_edr,
           community_edr_email, eda_factor_eligibility) %>%
    left_join(eda_wide1, by = "community_id",
              relationship = "one-to-one") %>%
    left_join(flags_per_community1, by = "community_id",
              relationship = "one-to-one") %>%
    mutate(
      ## Short name is computed from the bare name BEFORE the ANRC suffix is
      ## appended: only the long display name carries the entity type.
      community_name_short = shorten_community_name(community_name),
      community_name = if_else(
        str_starts(community_id, "anrc_"),
        str_c(community_name, " Alaska Native Regional Corporation"),
        community_name),
      across(
        c(eda_declaration_name_1, eda_declaration_name_2,
          eda_declaration_name_3),
        format_disaster_name)) %>%
    select(
      community_id,
      community_name,
      community_name_short,
      community_state,
      community_edr,
      community_edr_email,
      eda_factor_eligibility,
      eda_declaration_name_1,
      eda_declaration_name_2,
      eda_declaration_name_3,
      eda_declaration_year_1,
      eda_declaration_year_2,
      eda_declaration_year_3,
      eda_declaration_disaster_number_1,
      eda_declaration_disaster_number_2,
      eda_declaration_disaster_number_3,
      eda_spatial_intersection_counties_1,
      eda_spatial_intersection_counties_2,
      eda_spatial_intersection_counties_3,
      fema_hmgp_binary,
      fema_hmgp_lead_applicant_declaration_binary,
      fema_hmgp_lead_applicant_state_year,
      sba_binary)
}
