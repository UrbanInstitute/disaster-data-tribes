## ---------------------------------------------------------------------------
## 01_prepare_fema_declarations.R — feature-1 stage 1: FEMA declarations
##
## Pulls (or loads a cached snapshot of) OpenFEMA disaster declarations and
## classifies each row into a routing slice — tribal-direct, county-level, or
## statewide/territory-wide. Sourced by 00_run_feature_1.R and the background/
## scripts; stage 03 resolves eligibility from its classified output.
## ---------------------------------------------------------------------------
library(arrow)
library(tidyverse)
library(climateapi)
library(janitor)
library(here)

source(here::here("scripts", "utilities", "utilities.R"))

default_fema_box_dir <- function() {
  file.path(
    "C:", "Users", climateapi::get_system_username(), "Box", "data-cache", "openfema") }

#' Refresh the cached FEMA disaster declarations parquet
#'
#' Pulls the full FEMA `DisasterDeclarationsSummaries` table (filtered to
#' Major Disaster Declarations, `declarationType = "DR"`) via `rfema::open_fema()`
#' and writes a date-stamped parquet to the Box `openfema/` cache. Column names
#' are normalized with `janitor::clean_names()`.
#'
#' @param box_dir Character. Directory holding FEMA parquet snapshots. When
#'   `NULL` (the default), resolves to
#'   `C:/Users/<user>/Box/data-cache/openfema` via
#'   `climateapi::get_system_username()`. Must already exist; the function does
#'   not create it.
#' @param overwrite Logical. If `FALSE` (default), skip the download when a
#'   snapshot for today's date already exists in `box_dir`. If `TRUE`, replace
#'   the existing file.
#'
#' @return Character scalar, returned invisibly: the full path to the written
#'   (or pre-existing) parquet file, named
#'   `DisasterDeclarationsSummaries_YYYY_MM_DD.parquet`. Side effect: writes
#'   (or skips writing) the parquet at that path.
refresh_fema_declarations <- function(box_dir = NULL, overwrite = FALSE) {

  if (is.null(box_dir)) { box_dir <- default_fema_box_dir() }

  if (!dir.exists(box_dir)) { stop("Box cache directory not found: ", box_dir) }

  snapshot_date <- format(Sys.Date(), "%Y_%m_%d")
  out_path <- file.path(
    box_dir,
    str_c("DisasterDeclarationsSummaries_", snapshot_date, ".parquet"))

  if (file.exists(out_path) && !overwrite) {
    message("Snapshot already exists at ", out_path, "; pass overwrite = TRUE to replace.")
    return(invisible(out_path)) }

  ## DR only: grant eligibility for this feature is scoped to Major Disaster
  ## Declarations. Emergency (EM) and Fire Management (FM) declarations do not
  ## qualify under the program statute and are excluded at the API call.
  declarations <- rfema::open_fema(
    data_set = "DisasterDeclarationsSummaries",
    filters = list(declarationType = "=DR"),
    ask_before_call = FALSE) %>%
    janitor::clean_names() %>%
    as_tibble()

  arrow::write_parquet(declarations, out_path)

  message("Wrote ", nrow(declarations), " DR declarations to ", out_path, ".")

  invisible(out_path)
}

#' Load FEMA disaster declarations from the cached parquet
#'
#' Reads a dated snapshot of FEMA `DisasterDeclarationsSummaries` from the Box
#' `openfema/` cache. By default returns the most recent snapshot; can pull a
#' fresh snapshot via `rfema` or restrict to a historical vintage for
#' reproducibility.
#'
#' @param refresh Logical. If `TRUE`, call `refresh_fema_declarations()` first
#'   to pull a fresh snapshot before reading. Default `FALSE`.
#' @param as_of Date (or `NULL`). If supplied, return the dated snapshot
#'   closest to and on-or-before this date. When `NULL` (default), return the
#'   most recent available snapshot.
#' @param box_dir Character. Directory holding FEMA parquet snapshots. When
#'   `NULL` (the default), resolves to `C:/Users/<user>/Box/data-cache/openfema`.
#'
#' @return A tibble with one row per FEMA disaster declaration. Column names
#'   are cleaned with `janitor::clean_names()`. Core columns (per FEMA's
#'   DisasterDeclarationsSummaries API):
#'
#'   \describe{
#'     \item{disaster_number}{Integer. FEMA disaster identifier.}
#'     \item{declaration_type}{Character. `"DR"` for Major Disaster, `"EM"`
#'       for Emergency, `"FM"` for Fire Management (refresh pulls only `DR`).}
#'     \item{declaration_date}{Date-time. When FEMA issued the declaration.}
#'     \item{incident_type}{Character. Hazard type (e.g., `"Hurricane"`,
#'       `"Severe Storm"`).}
#'     \item{declaration_title}{Character. Free-text title of the declaration.}
#'     \item{designated_area}{Character. FEMA's free-text designated area for
#'       this row (e.g., `"Miami-Dade (County)"`, `"Navajo Nation (TN)"`,
#'       `"STATEWIDE"`).}
#'     \item{state}{Character. Two-letter state/territory postal code.}
#'     \item{fips_state_code, fips_county_code}{Character. FIPS codes that, when
#'       concatenated, identify a county (or `"000"` for tribal-designated rows).}
#'     \item{place_code}{Character. FEMA place code, typically populated for
#'       tribal-designated areas.}
#'     \item{tribal_request}{Logical/character. `TRUE` when the declaration
#'       was requested directly by a tribal government under the 2013 pilot.}
#'     \item{ia_program_declared, pa_program_declared, hm_program_declared,
#'       ih_program_declared}{Logical/character. Whether Individual Assistance,
#'       Public Assistance, Hazard Mitigation, and Individual and Households
#'       programs (respectively) were authorized.}
#'     \item{id}{Character. FEMA's internal row identifier.}
#'   }
#'
#'   Additional columns from the FEMA API are passed through unchanged.
get_fema_declarations <- function(refresh = FALSE, as_of = NULL, box_dir = NULL) {

  if (is.null(box_dir)) { box_dir <- default_fema_box_dir() }

  if (refresh) { refresh_fema_declarations(box_dir = box_dir) }

  snapshots <- tibble(
    path = list.files(
      box_dir,
      pattern = "^DisasterDeclarationsSummaries_\\d{4}_\\d{2}_\\d{2}\\.parquet$",
      full.names = TRUE)) %>%
    mutate(
      snapshot_date = path %>%
        str_extract("\\d{4}_\\d{2}_\\d{2}") %>%
        str_replace_all("_", "-") %>%
        ymd())

  if (nrow(snapshots) == 0) {
    stop(
      "No cached FEMA parquet snapshots found in ", box_dir, ".\n",
      "Run refresh_fema_declarations() first, or pass refresh = TRUE.") }

  chosen_snapshot <- if (!is.null(as_of)) {
    snapshots %>%
      filter(snapshot_date <= as.Date(as_of)) %>%
      slice_max(snapshot_date, n = 1)
  } else { snapshots %>% slice_max(snapshot_date, n = 1)  }

  if (nrow(chosen_snapshot) == 0) {  stop("No FEMA snapshots on or before ", as_of, ".") }

  message("Loading FEMA declarations snapshot from ", chosen_snapshot$snapshot_date, ".")

  arrow::read_parquet(chosen_snapshot$path) %>%
    janitor::clean_names() %>%
    as_tibble()
}

#' Classify FEMA declarations with eligibility flags and routing pattern
#'
#' Filters to `declaration_type == "DR"`, normalizes `incident_type` (including
#' the `"STRAIGHT-LINE WINDS"` fix), computes derived columns used by the
#' downstream resolver: an `eligible_event` flag for declarations falling in
#' `year_range` with a recognized natural hazard, a concatenated `county_fips`,
#' and a `declaration_route` label that categorizes how FEMA routed each row.
#' Calls `stop()` (fail-closed) if any `incident_type` falls outside both the
#' natural-hazard list and the known non-natural exclusion list — the caller
#' must explicitly classify the new type rather than letting it silently
#' drop out of eligibility.
#'
#' @param declarations Tibble. The output of `get_fema_declarations()`.
#' @param year_range Integer vector. Calendar years for which a declaration
#'   counts toward `eligible_event = 1`. This filters on the calendar year FEMA
#'   *issued* the declaration (`year(declaration_date)`), NOT the calendar
#'   year in which the underlying incident occurred
#'   (`year(incident_begin_date)`). Events that occurred in one year but were
#'   declared in the next are attributed to the declaration year. Default
#'   `2023:2024`.
#' @param natural_hazards Character vector or `NULL`. `incident_type` values
#'   treated as natural hazards. When `NULL` (default), uses the built-in list:
#'   Fire, Flood, Hurricane, Severe Storm, Winter Storm, Tornado, Snowstorm,
#'   Earthquake, Mud/Landslide, Coastal Storm, Severe Ice Storm, Tropical
#'   Storm, Typhoon, Volcanic Eruption, Tsunami, Freezing, Drought, Tropical
#'   Depression, Straight-Line Winds.
#'
#' @return A tibble with all input columns plus:
#'
#'   \describe{
#'     \item{calendar_year_declared}{Integer. Calendar year of
#'       `declaration_date`.}
#'     \item{county_fips}{Character(5). `str_c(fips_state_code,
#'       fips_county_code)`. Last three digits are `"000"` for tribal-direct
#'       rows. Retired county FIPS (Shannon SD, Bedford City VA, three
#'       pre-2015 AK census areas, the eight legacy CT counties) are
#'       normalized to their current successor county-equivalents; a retired
#'       code with several successors duplicates the row per successor.
#'       Tribal-requested rows that ship a real county code (DR-4919) are
#'       normalized to the tribal `"000"` slot. The raw `fips_state_code` /
#'       `fips_county_code` columns keep their as-shipped values.}
#'     \item{eligible_event}{Integer. `1` iff `calendar_year_declared %in%
#'       year_range` AND `incident_type %in% natural_hazards`; else `0`.}
#'     \item{declaration_route}{Character. Routing pattern for this row:
#'       \itemize{
#'         \item `"statewide"` — `designated_area` matches `"statewide"`
#'           (case-insensitive), or the row carries FEMA's whole-CNMI
#'           pseudo-FIPS `69010` (a territory-wide designation that exists in
#'           no TIGER vintage; routing it statewide propagates it to every MP
#'           county-equivalent).
#'         \item `"tribal_direct"` — the row carries `tribal_request = TRUE`
#'           (authoritative: a tribal request applies to the filing tribe,
#'           never to a county, even when FEMA stamps a real county FIPS on
#'           the row as on DR-4919), or the last three digits of
#'           `county_fips` are `"000"` (FEMA's convention for
#'           tribal-designated areas).
#'         \item `"county_level"` — every other row (county or county-
#'           equivalent).
#'       }}
#'   }
classify_declarations <- function(declarations,
                                  year_range = 2023:2024,
                                  natural_hazards = NULL) {

  if (is.null(natural_hazards)) {
    natural_hazards <- c(
      "Fire", "Flood", "Hurricane", "Severe Storm", "Winter Storm", "Tornado",
      "Snowstorm", "Earthquake", "Mud/Landslide", "Coastal Storm",
      "Severe Ice Storm", "Tropical Storm", "Typhoon", "Volcanic Eruption",
      "Tsunami", "Freezing", "Drought", "Tropical Depression",
      "Straight-Line Winds")
  }

  known_non_natural <- c(
    "Biological", "Dam/Levee Break", "Terrorist", "Human Cause", "Toxic Substances",
    "Fishing Losses", "Other")

  classified1 <- declarations %>%
    filter(declaration_type == "DR") %>%
    mutate(
      ## some incident_types are classified as "Other" but the declaration titles indicate they are
      ## natural hazard-related; only "Other" rows are remapped so FEMA's own
      ## classification is never overwritten (titles like "SEVERE STORMS,
      ## FLOODING, AND STRAIGHT-LINE WINDS" appear on ~5,000 rows FEMA already
      ## classifies as Severe Storm, Flood, etc.)
      incident_type = case_when(
        incident_type == "Other" &
          str_detect(declaration_title, "STRAIGHT-LINE WINDS") ~ "Straight-Line Winds",
        incident_type == "Other" &
          str_detect(declaration_title, "WIND STORM") ~ "Straight-Line Winds",
        incident_type == "Other" &
          str_detect(declaration_title, "SEVERE WEATHER CONDITIONS") ~ "Severe Storm",
        TRUE ~ incident_type),
      calendar_year_declared = year(declaration_date),
      county_fips = str_c(fips_state_code, fips_county_code))

  ## Older declarations carry county FIPS that have since been retired and so
  ## match no current TIGER polygon — they would silently fail all spatial
  ## matching. Normalize each retired code to the current county-equivalent(s)
  ## covering the retired unit's territory; 1:many successors duplicate the
  ## declaration row per successor, which the downstream per-(county ×
  ## disaster) collapse absorbs. The CT legacy-county → 2022-planning-region
  ## rows are approximate at region fringes but exact for New London →
  ## Southeastern CT, the only CT geography with federally recognized tribes;
  ## Shannon County SD → Oglala Lakota County recovers Pine Ridge matches.
  retired_fips_lookup <- tibble::tribble(
    ~retired_fips, ~current_fips,
    "46113", "46102", # Shannon County SD -> Oglala Lakota County (Pine Ridge)
    "51515", "51019", # Bedford (independent) City VA -> Bedford County
    "02270", "02158", # Wade Hampton Census Area AK -> Kusilvak Census Area
    "02201", "02198", # Prince of Wales-Outer Ketchikan AK -> Prince of Wales-Hyder,
    "02201", "02130", #   Ketchikan Gateway Borough (annexed Outer Ketchikan parts),
    "02201", "02275", #   and Wrangell City and Borough (Meyers Chuck area)
    "02280", "02195", # Wrangell-Petersburg Census Area AK -> Petersburg Borough
    "02280", "02275", #   and Wrangell City and Borough
    "09001", "09120", # Fairfield CT -> Greater Bridgeport,
    "09001", "09140", #   Naugatuck Valley (Shelton),
    "09001", "09190", #   and Western CT
    "09003", "09110", # Hartford CT -> Capitol
    "09003", "09140", #   and Naugatuck Valley (Bristol, Plainville, Plymouth)
    "09005", "09160", # Litchfield CT -> Northwest Hills,
    "09005", "09140", #   Naugatuck Valley (Thomaston, Watertown),
    "09005", "09190", #   and Western CT (New Milford, Sherman)
    "09007", "09130", # Middlesex CT -> Lower CT River Valley
    "09009", "09170", # New Haven CT -> South Central CT
    "09009", "09140", #   and Naugatuck Valley (Waterbury, Naugatuck)
    "09011", "09180", # New London CT -> Southeastern CT
    "09013", "09110", # Tolland CT -> Capitol
    "09013", "09150", #   and Northeastern CT (Union)
    "09015", "09150") # Windham CT -> Northeastern CT

  n_retired_rows <- sum(classified1$county_fips %in% retired_fips_lookup$retired_fips)
  if (n_retired_rows > 0) {
    message(
      "classify_declarations: normalizing ", n_retired_rows,
      " declaration row(s) carrying retired county FIPS to current ",
      "county-equivalents (1:many successors duplicate rows).")
  }

  classified1a <- classified1 %>%
    tidylog::left_join(
      retired_fips_lookup,
      by = c("county_fips" = "retired_fips"),
      relationship = "many-to-many") %>%
    mutate(county_fips = coalesce(current_fips, county_fips)) %>%
    select(-current_fips)

  unexpected_summary <- classified1a %>%
    filter(
      !incident_type %in% natural_hazards,
      !incident_type %in% known_non_natural) %>%
    summarize(
      .by = incident_type,
      n_rows = n(),
      n_disasters = n_distinct(disaster_number)) %>%
    arrange(desc(n_rows))

  if (nrow(unexpected_summary) > 0) {
    stop(
      "Unexpected incident_type values in declarations (not in ",
      "natural_hazards, not in known_non_natural). Fix by adding each to ",
      "the appropriate list in classify_declarations(). Unknown types:\n",
      str_c(
        "  - ", unexpected_summary$incident_type,
        " (", unexpected_summary$n_rows, " rows, ",
        unexpected_summary$n_disasters, " disasters)",
        collapse = "\n"),
      call. = FALSE) }

  ## A tribal request applies to the filing tribe, not to whatever county
  ## FEMA happened to stamp on the row. FEMA's convention is
  ## fips_county_code "000" for tribal designated areas, but DR-4919
  ## ("Mashpee Wampanoag Tribe", MA, 2026-06-30) shipped a real county code
  ## (25001, Barnstable) on a tribal-requested row — which would have routed
  ## it county_level and cross-propagated it to every community intersecting
  ## the county. Treat the designated area as the tribe: normalize the county
  ## slot to the tribal "000" convention (keeping `indigenous_id =
  ## county_fips + place_code` stable across a tribe's declarations) and
  ## route on `tribal_request` ahead of the FIPS rule.
  tribal_request_nonstandard_fips <- classified1a %>%
    filter(
      replace_na(as.logical(tribal_request), FALSE),
      str_sub(county_fips, 3, 5) != "000")
  if (nrow(tribal_request_nonstandard_fips) > 0) {
    message(
      "classify_declarations: ", nrow(tribal_request_nonstandard_fips),
      " tribal-requested row(s) carry a non-'000' county FIPS ",
      "(FEMA convention break); normalizing to the tribal county slot and ",
      "routing tribal_direct: ",
      str_c(
        unique(str_c(
          "DR-", tribal_request_nonstandard_fips$disaster_number, " ",
          tribal_request_nonstandard_fips$designated_area)),
        collapse = "; "))
  }

  classified2 <- classified1a %>%
    mutate(
      is_tribal_request = replace_na(as.logical(tribal_request), FALSE),
      county_fips = if_else(
        is_tribal_request & str_sub(county_fips, 3, 5) != "000",
        str_c(fips_state_code, "000"),
        county_fips),
      eligible_event = as.integer(
        calendar_year_declared %in% year_range &
          incident_type %in% natural_hazards),
      declaration_route = case_when(
        is_tribal_request ~ "tribal_direct",
        str_detect(designated_area, regex("statewide", ignore_case = TRUE)) ~ "statewide",
        ## FEMA tags whole-CNMI declarations with pseudo-FIPS 69010
        ## ("Northern Mariana Islands (County-equivalent)"), which exists in
        ## no TIGER vintage (the real MP county-equivalents are 69085, 69100,
        ## 69110, 69120), so these rows would otherwise sit in
        ## county_declarations and silently match nothing. They designate the
        ## entire territory, so they route as statewide and propagate to every
        ## MP county-equivalent. Guam's analog 66010 is a real GEOID (Guam is
        ## a single county-equivalent) and correctly stays county_level.
        county_fips == "69010" ~ "statewide",
        str_sub(county_fips, 3, 5) == "000" ~ "tribal_direct",
        TRUE ~ "county_level")) %>%
    select(-is_tribal_request)

  classified2
}

#' Extract tribal-direct FEMA declarations for the eligibility window
#'
#' Filters to rows where FEMA routed the declaration directly to a tribal
#' designated area (`declaration_route == "tribal_direct"`) and the event is
#' eligible. Dedupes the `place_code`-collision cases FEMA ships (the same
#' (`county_fips`, `designated_area`) pair appearing with multiple
#' `place_code` values — the first `place_code` wins, after a warning), then
#' collapses to one row per tribal entity × disaster number and constructs a
#' stable `indigenous_id`. Note `indigenous_id` embeds the state FIPS, so a
#' multi-state tribal declaration yields one row per state for the same
#' `disaster_number`; count disasters via `n_distinct(disaster_number)`, not
#' rows.
#'
#' @param classified_declarations Tibble. Output of `classify_declarations()`.
#' @param year_range Integer vector. Calendar years to include, filtered on
#'   the calendar year FEMA issued the declaration
#'   (`year(declaration_date)`), NOT the year the incident occurred. Default
#'   `2023:2024`.
#'
#' @return A tibble with one row per (`indigenous_id`, `designated_area`,
#'   `state`, `disaster_number`). Columns:
#'
#'   \describe{
#'     \item{indigenous_id}{Character. `str_c(county_fips, place_code)` —
#'       stable identifier for the tribal designated area, unique across
#'       FEMA's occasional place-code duplication.}
#'     \item{designated_area}{Character. FEMA's free-text designation for the
#'       tribal area.}
#'     \item{state}{Character. Two-letter state/territory code.}
#'     \item{disaster_number}{Integer. FEMA disaster identifier.}
#'     \item{declaration_date}{Date-time. Earliest declaration date across
#'       deduped rows.}
#'     \item{declaration_title}{Character. Declaration title (first value
#'       across deduped rows).}
#'     \item{incident_type}{Character. Hazard type (first value across
#'       deduped rows).}
#'     \item{ia_program_declared, pa_program_declared, hm_program_declared,
#'       ih_program_declared}{Logical. `TRUE` if the corresponding assistance
#'       program was authorized in any deduped source row.}
#'     \item{tribal_request}{Logical. `TRUE` if any deduped source row was
#'       flagged as a direct tribal request.}
#'   }
get_tribal_declarations <- function(classified_declarations,
                                    year_range = 2023:2024) {

  tribal1 <- classified_declarations %>%
    filter(
      declaration_route == "tribal_direct",
      calendar_year_declared %in% year_range,
      eligible_event == 1)

  tribal2 <- tribal1 %>%
    warn_if_multivalued(
      c("county_fips", "designated_area"), "place_code",
      context = "get_tribal_declarations") %>%
    mutate(
      .by = c(county_fips, designated_area),
      place_code = first(place_code)) %>%
    mutate(indigenous_id = str_c(county_fips, place_code))

  tribal3 <- tribal2 %>%
    warn_if_multivalued(
      c("indigenous_id", "designated_area", "state", "disaster_number"),
      "declaration_title", context = "get_tribal_declarations") %>%
    warn_if_multivalued(
      c("indigenous_id", "designated_area", "state", "disaster_number"),
      "incident_type", context = "get_tribal_declarations") %>%
    summarize(
      .by = c(indigenous_id, designated_area, state, disaster_number),
      declaration_date = min(declaration_date, na.rm = TRUE),
      declaration_title = first(declaration_title),
      incident_type = first(incident_type),
      ia_program_declared = any_flag_true(ia_program_declared),
      pa_program_declared = any_flag_true(pa_program_declared),
      hm_program_declared = any_flag_true(hm_program_declared),
      ih_program_declared = any_flag_true(ih_program_declared),
      tribal_request = any_flag_true(tribal_request))

  tribal3
}

#' Extract county-level FEMA declarations for spatial joins
#'
#' Filters to rows where FEMA routed the declaration to a county or
#' county-equivalent (`declaration_route == "county_level"`) and the event is
#' eligible, then collapses to one row per (county × disaster) so the row key
#' is suitable for spatial joins against Indigenous geographies.
#'
#' @param classified_declarations Tibble. Output of `classify_declarations()`.
#' @param year_range Integer vector. Calendar years to include, filtered on
#'   the calendar year FEMA issued the declaration
#'   (`year(declaration_date)`), NOT the year the incident occurred. Default
#'   `2023:2024`.
#'
#' @return A tibble with one row per (`county_fips`, `state`,
#'   `disaster_number`). Columns:
#'
#'   \describe{
#'     \item{county_fips}{Character(5). State + county FIPS.}
#'     \item{state}{Character. Two-letter state/territory code.}
#'     \item{disaster_number}{Integer. FEMA disaster identifier.}
#'     \item{declaration_date}{Date-time. Earliest declaration date across
#'       deduped rows.}
#'     \item{declaration_title, incident_type}{Character. First value
#'       across deduped source rows (invariant within group in practice; a
#'       warning fires if variation is detected).}
#'     \item{designated_area}{Character. All distinct `designated_area`
#'       labels from the deduped source rows, sorted and pipe-delimited
#'       (`" | "`). FEMA sometimes designates disasters at a sub-county
#'       level (e.g., Alaska REAAs) — concatenating preserves every label
#'       instead of silently picking one.}
#'     \item{ia_program_declared, pa_program_declared, hm_program_declared,
#'       ih_program_declared}{Logical. `TRUE` if the corresponding assistance
#'       program was authorized in any deduped source row.}
#'   }
get_county_declarations <- function(classified_declarations,
                                    year_range = 2023:2024) {

  county1 <- classified_declarations %>%
    filter(
      declaration_route == "county_level",
      calendar_year_declared %in% year_range,
      eligible_event == 1)

  ## `designated_area` can legitimately vary within a (county_fips, state,
  ## disaster_number) group — FEMA uses sub-county labels in some states
  ## (e.g., Alaska Regional Educational Attendance Areas all share one census
  ## area's county_fips but carry distinct REAA names). Concatenate the
  ## distinct labels rather than picking one via `first()` so the aggregated
  ## row preserves the full set.
  county2 <- county1 %>%
    warn_if_multivalued(
      c("county_fips", "state", "disaster_number"), "declaration_title",
      context = "get_county_declarations") %>%
    warn_if_multivalued(
      c("county_fips", "state", "disaster_number"), "incident_type",
      context = "get_county_declarations") %>%
    summarize(
      .by = c(county_fips, state, disaster_number),
      declaration_date = min(declaration_date, na.rm = TRUE),
      declaration_title = first(declaration_title),
      incident_type = first(incident_type),
      designated_area = str_c(sort(unique(designated_area)), collapse = " | "),
      ia_program_declared = any_flag_true(ia_program_declared),
      pa_program_declared = any_flag_true(pa_program_declared),
      hm_program_declared = any_flag_true(hm_program_declared),
      ih_program_declared = any_flag_true(ih_program_declared))

  county2
}

#' Extract statewide FEMA declarations for whole-state propagation
#'
#' Filters to rows routed as `statewide` (`designated_area` matches
#' "statewide") within `year_range`, then collapses to one row per
#' (state × disaster). Statewide declarations apply to every community
#' whose `state_list` includes the declared state — `resolve_eligibility()`
#' propagates them on that basis. Territorial-level FEMA declarations for
#' American Samoa, Guam, and CNMI also route through this path (they are
#' "statewide" from FEMA's perspective).
#'
#' @param classified_declarations Tibble. Output of `classify_declarations()`.
#' @param year_range Integer vector. Calendar years to include, filtered on
#'   the calendar year FEMA issued the declaration
#'   (`year(declaration_date)`), NOT the year the incident occurred. Default
#'   `2023:2024`.
#'
#' @return A tibble with one row per (`state`, `disaster_number`). Columns:
#'
#'   \describe{
#'     \item{state}{Character. Two-letter state/territory code.}
#'     \item{disaster_number}{Integer. FEMA disaster identifier.}
#'     \item{declaration_date}{Date-time. Earliest declaration date across
#'       deduped rows.}
#'     \item{declaration_title, incident_type, designated_area}{Character.
#'       First value across deduped source rows.}
#'     \item{ia_program_declared, pa_program_declared, hm_program_declared,
#'       ih_program_declared}{Logical. `TRUE` if the corresponding assistance
#'       program was authorized in any deduped source row.}
#'   }
get_statewide_declarations <- function(classified_declarations,
                                       year_range = 2023:2024) {

  statewide1 <- classified_declarations %>%
    filter(
      declaration_route == "statewide",
      calendar_year_declared %in% year_range,
      eligible_event == 1)

  statewide2 <- statewide1 %>%
    warn_if_multivalued(
      c("state", "disaster_number"), "declaration_title",
      context = "get_statewide_declarations") %>%
    warn_if_multivalued(
      c("state", "disaster_number"), "incident_type",
      context = "get_statewide_declarations") %>%
    warn_if_multivalued(
      c("state", "disaster_number"), "designated_area",
      context = "get_statewide_declarations") %>%
    summarize(
      .by = c(state, disaster_number),
      declaration_date = min(declaration_date, na.rm = TRUE),
      declaration_title = first(declaration_title),
      incident_type = first(incident_type),
      designated_area = first(designated_area),
      ia_program_declared = any_flag_true(ia_program_declared),
      pa_program_declared = any_flag_true(pa_program_declared),
      hm_program_declared = any_flag_true(hm_program_declared),
      ih_program_declared = any_flag_true(ih_program_declared))

  statewide2
}
