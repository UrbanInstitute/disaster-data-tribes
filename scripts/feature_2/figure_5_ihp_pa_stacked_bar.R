## ---------------------------------------------------------------------------
## figure_5_ihp_pa_stacked_bar.R — data prep for feature-2 figure 5
##
## prepare_figure_5_data(): IHP and PA federal award dollars per Indigenous
## community, for the figure-5 stacked bar. Sourced/called by _feature2.qmd,
## where cross-community aggregation and plotting happen.
## ---------------------------------------------------------------------------
library(arrow)
library(tidyverse)
library(janitor)
library(here)
library(rfema)

source(here::here("scripts", "feature_2", "get_tribal_crosswalks.R"))
source(here::here("scripts", "utilities", "utilities.R"))  # adjust_dollars_to_2025()

## OpenFEMA snapshots live in the user's Box data-cache, not in this repo.
## Built per-user so the path is portable across machines; this mirrors the
## canonical `default_fema_box_dir()` in
## scripts/feature_1/01_prepare_fema_declarations.R.
default_openfema_box_dir <- function() {
  file.path(
    "C:", "Users", climateapi::get_system_username(), "Box", "data-cache", "openfema") }

#' Prepare per-community IHP and PA award data for figure 5
#'
#' Returns one row per (community, year, source) for IHP and PA federal
#' disaster awards flowing to Indigenous communities. Aggregation across
#' communities (e.g., to year × source for the stacked bar) is performed in
#' the qmd, not here, so callers can subset by `territory_flag` or otherwise
#' slice the data before aggregating.
#'
#' On the IHP side, registrations are joined to the ZCTA→AIANNH crosswalk
#' (which already includes one row per territorial ZCTA mapped to its
#' territory) and dollars are allocated by `allocation_factor_source_to_target`.
#' On the PA side, project obligations are classified as "Territories" by
#' state (American Samoa, Guam, Northern Mariana Islands) and as
#' "Federally-recognized tribes and ANVs" by applicant_name pattern. Hawaii
#' is excluded — it is not in feature 2's Indigenous-community universe.
#'
#' @param ihp_path Path to the IHP valid registrations parquet snapshot.
#'   Defaults to the dated snapshot in the user's Box `openfema/` cache.
#' @param pa_path Path to the PA funded project summaries parquet snapshot.
#'   Defaults to the dated snapshot in the user's Box `openfema/` cache.
#'
#' @return A tibble with columns `community_name`, `year_declared`,
#'   `territory_flag` ("Territories" or "Federally-recognized tribes and ANVs"),
#'   `home_state` (2-letter state code; carried from the ZCTA-crosswalk on
#'   the IHP side and from the PA `state` full-name on the PA side; used
#'   downstream for four-way community-count QC),
#'   `type` ("ihp" or "pa"), and `amount` (federal dollars, expressed in
#'   2025 USD — nominal amounts are deflated from the declaration year via
#'   the annual PCE price index). PA totals are net of deobligations:
#'   negative `federal_obligated_amount` rows are kept, so a
#'   community-year's PA amount can in principle be negative.
prepare_figure_5_data = function(
    ihp_path = file.path(
      default_openfema_box_dir(),
      "IndividualsAndHouseholdsProgramValidRegistrations_2026_03_04.parquet"),
    pa_path = file.path(
      default_openfema_box_dir(),
      "PublicAssistanceFundedProjectsSummaries_2026_03_04.parquet")) {

  ## virtually no zip code missingness, compared to "censusGeoid", which has high missingness
  ## so we'll interpolate from zip code to tribal area. `territory_flag` is
  ## carried directly from the crosswalk — for territorial rows,
  ## `target_geography_name` is the county-equivalent name, not the territory
  ## name, so it can't be used as a partition key on its own anymore.
  zip_tribal_crosswalk = get_tribal_crosswalk(source_geography = "zcta")

  tribal_zctas = zip_tribal_crosswalk %>%
    filter(!is.na(source_geoid)) %>%
    pull(source_geoid) %>%
    unique()

  ihp_raw = arrow::read_parquet(ihp_path) %>%
    janitor::clean_names() %>%
    filter(damaged_zip_code %in% tribal_zctas)

  ## I'm unsure how to QC this join because there's no expectation that FEMA has made IHP
  ## awards to every indigenous community, so we would anticipate non-joining records from
  ## both sides of the join...
  ihp_by_community = tidylog::left_join(
      zip_tribal_crosswalk,
      ihp_raw %>% mutate(year_declared = year(declaration_date)),
      by = c("source_geoid" = "damaged_zip_code"),
      relationship = "many-to-many") %>%
    summarize(
      .by = c(target_geography_name, territory_flag, home_state, year_declared),
      ## Territorial rows scaled to the county-equivalent's Indigenous (NHPI
      ## alone-or-in-combination) population share (figure-1 parity); tribal
      ## rows unscaled (indigenous_share NA, so the if_else returns 1).
      amount = sum(
        ihp_amount * allocation_factor_source_to_target *
          if_else(territory_flag == "Territories", coalesce(indigenous_share, 0), 1),
        na.rm = TRUE),
      ## Unscaled total (allocation only) carried alongside for the
      ## territorial-scaling QC; equals the scaled amount for tribal rows.
      amount_unscaled = sum(
        ihp_amount * allocation_factor_source_to_target, na.rm = TRUE)) %>%
    transmute(
      community_name = target_geography_name,
      territory_flag,
      home_state,
      year_declared,
      type = "ihp",
      amount,
      amount_unscaled)

  ## PA records: territories identified by state, tribal applicants by name pattern.
  pa_territory_states = c("American Samoa", "Guam", "Northern Mariana Islands")
  pa_state_full_to_abbr = tidycensus::fips_codes %>%
    distinct(state_name, state) %>%
    deframe()
  ## Case-insensitive with word boundaries: PA applicant names are frequently
  ## all-caps ("OGLALA SIOUX TRIBE", "MESCALERO APACHE TRIBE"), which the
  ## previous case-sensitive pattern missed entirely (~$41.6M within the
  ## plotted window); word boundaries keep e.g. "Preservation" and
  ## "Alternatives Inc." from matching "reservation" and "native". Match and
  ## exclusion sets validated by reviewing every distinct matched
  ## applicant_name in the 2026-03-04 snapshot.
  tribal_name_pattern = regex(
    str_c(
      "\\btribes?\\b|\\btribal\\b|\\bnation$|\\bnation rancheria\\b|",
      "\\breservation\\b|\\bnative\\b|\\bANV\\b|\\(ANV|ANVSA|",
      "\\bpueblos?\\b|\\brancherias?\\b"),
    ignore_case = TRUE)
  ## Exclusions: county/municipal governments (incl. Pueblo CO's county/city
  ## entities); non-government entities that hit a tribal token (an acequia
  ## association, a museum, the Pueblo West metro district); and
  ## state-recognized-only entities (Coharie, Southern Band Tuscasora,
  ## Poospatuck) outside the federally-recognized universe this figure
  ## describes.
  non_tribal_pattern = regex(
    str_c(
      "County|City|Town of|Acequia|Museum|Metropolitan District|",
      "Coharie|Tuscasora|Poospatuck"),
    ignore_case = TRUE)

  pa_by_community = arrow::read_parquet(pa_path) %>%
    janitor::clean_names() %>%
    ## FEMA "DO NOT USE" duplicate applicant records must be excluded from
    ## every branch — territorial rows are classified by state before any
    ## name pattern is checked, so this cannot live in `non_tribal_pattern`
    ## (which only gates the tribal branch).
    filter(!str_detect(applicant_name, regex("DO NOT USE", ignore_case = TRUE))) %>%
    mutate(
      territory_flag = case_when(
        state %in% pa_territory_states ~ "Territories",
        !str_detect(applicant_name, non_tribal_pattern) &
          str_detect(applicant_name, tribal_name_pattern) ~ "Federally-recognized tribes and ANVs",
        TRUE ~ NA_character_),
      ## PA's `state` is a full state name; convert to 2-letter code so
      ## downstream community-count classification can use the same
      ## `home_state` column as crosswalk-based figures.
      home_state = unname(pa_state_full_to_abbr[state])) %>%
    filter(
      !is.na(territory_flag),
      ## Keep negative rows: FEMA records deobligations (dollars taken back
      ## after an obligation) as negative amounts, so summed totals are net
      ## of those reversals. Zero-dollar rows add nothing and are dropped.
      !is.na(federal_obligated_amount),
      federal_obligated_amount != 0) %>%
    mutate(year_declared = year(declaration_date)) %>%
    summarize(
      .by = c(applicant_name, territory_flag, home_state, year_declared),
      amount_unscaled = sum(federal_obligated_amount, na.rm = TRUE)) %>%
    ## PA is reported per territorial government (not per county-equivalent), so
    ## territorial awards are scaled by the territory-wide Indigenous (NHPI
    ## alone-or-in-combination) population share; tribal rows are unscaled. The
    ## unscaled total is carried alongside for the territorial-scaling QC.
    tidylog::left_join(
      get_territory_indigenous_share_by_state() %>% select(home_state, indigenous_share),
      by = "home_state",
      relationship = "many-to-one") %>%
    mutate(
      amount = if_else(
        territory_flag == "Territories",
        amount_unscaled * coalesce(indigenous_share, 0),
        amount_unscaled)) %>%
    transmute(
      community_name = applicant_name,
      territory_flag,
      home_state,
      year_declared,
      type = "pa",
      amount,
      amount_unscaled)

  result = bind_rows(ihp_by_community, pa_by_community) %>%
    filter(!is.na(year_declared)) %>%
    ## Convert nominal award dollars to 2025 USD, deflating each row from
    ## its declaration year (the only date these records carry) via the
    ## annual PCE price index. The unscaled QC column gets the same
    ## treatment so scaled/unscaled stay directly comparable.
    adjust_dollars_to_2025(
      year_variable = "year_declared",
      dollar_variables = c("amount", "amount_unscaled"))

  return(result)
}

#' Prepare national IHP and PA award totals by declaration year
#'
#' Companion to `prepare_figure_5_data()` for the in-text comparison
#' statistic: total IHP and PA federal award dollars across ALL applicants
#' nationwide (no Indigenous-community filter), by declaration year, in
#' 2025 USD. Applies the same record-level treatment as the Indigenous
#' path — PA "DO NOT USE" duplicate applicant rows dropped, zero-dollar PA
#' rows dropped, negative PA rows (deobligations) kept, dollars deflated
#' from the declaration year via the annual PCE price index. Reads only the
#' needed columns from the (large, national) parquet snapshots.
#'
#' @param ihp_path Path to the IHP valid registrations parquet snapshot.
#' @param pa_path Path to the PA funded project summaries parquet snapshot.
#'
#' @return A tibble with columns `year_declared`, `type` ("ihp" or "pa"),
#'   and `amount` (federal dollars, 2025 USD).
prepare_national_ihp_pa_totals = function(
    ihp_path = file.path(
      default_openfema_box_dir(),
      "IndividualsAndHouseholdsProgramValidRegistrations_2026_03_04.parquet"),
    pa_path = file.path(
      default_openfema_box_dir(),
      "PublicAssistanceFundedProjectsSummaries_2026_03_04.parquet")) {

  ## Look up a snapshot's raw (camelCase) column names by their
  ## janitor-cleaned equivalents, so the lazy arrow select stays robust to
  ## the snapshots' naming without collecting the full national tables.
  raw_names_for = function(dataset, clean_names_wanted) {
    raw = names(dataset)
    set_names(raw, janitor::make_clean_names(raw))[clean_names_wanted] %>%
      unname()
  }

  ihp_dataset = arrow::open_dataset(ihp_path)
  ihp_national = ihp_dataset %>%
    select(all_of(raw_names_for(ihp_dataset, c("ihp_amount", "declaration_date")))) %>%
    collect() %>%
    janitor::clean_names() %>%
    mutate(year_declared = year(declaration_date)) %>%
    summarize(
      .by = year_declared,
      amount = sum(ihp_amount, na.rm = TRUE)) %>%
    mutate(type = "ihp")

  pa_dataset = arrow::open_dataset(pa_path)
  pa_national = pa_dataset %>%
    select(all_of(raw_names_for(
      pa_dataset,
      c("applicant_name", "federal_obligated_amount", "declaration_date")))) %>%
    collect() %>%
    janitor::clean_names() %>%
    filter(
      !str_detect(applicant_name, regex("DO NOT USE", ignore_case = TRUE)),
      !is.na(federal_obligated_amount),
      federal_obligated_amount != 0) %>%
    mutate(year_declared = year(declaration_date)) %>%
    summarize(
      .by = year_declared,
      amount = sum(federal_obligated_amount, na.rm = TRUE)) %>%
    mutate(type = "pa")

  result = bind_rows(ihp_national, pa_national) %>%
    filter(!is.na(year_declared)) %>%
    adjust_dollars_to_2025(
      year_variable = "year_declared",
      dollar_variables = "amount")

  return(result)
}
