## ---------------------------------------------------------------------------
## figure_7_hma_stacked_bar.R — data prep for feature-2 figure 7
##
## prepare_figure_7_data(): Hazard Mitigation Assistance awards to Indigenous
## entities, unioning FEMAGO (HmaSubapplications) with the legacy v4 endpoint,
## for the figure-7 stacked bar. Sourced/called by _feature2.qmd.
## ---------------------------------------------------------------------------
library(arrow)
library(tidyverse)
library(sf)
library(janitor)
library(here)

## get_territory_indigenous_share_by_state(): territory-wide Native population
## share, used to scale territorial (AS/GU/MP) awards to the Indigenous share.
source(here::here("scripts", "feature_2", "get_tribal_crosswalks.R"))
source(here::here("scripts", "utilities", "utilities.R"))  # adjust_dollars_to_2025()

## OpenFEMA snapshots live in the user's Box data-cache, not in this repo.
## Built per-user so the path is portable across machines; this mirrors the
## canonical `default_fema_box_dir()` in
## scripts/feature_1/01_prepare_fema_declarations.R.
default_openfema_box_dir <- function() {
  file.path(
    "C:", "Users", climateapi::get_system_username(), "Box", "data-cache", "openfema") }

#' Prepare HMA awards to Indigenous entities, by fiscal year, program, and
#' source
#'
#' Combines two OpenFEMA sources into one tidy tibble of awards to Indigenous
#' entities (federally recognized tribes, Hawaiian Home Lands, and
#' the AS/GU/MP territories):
#'
#' \describe{
#'   \item{\strong{femago} — \code{HmaSubapplications}}{FEMA's current
#'     subapplication-tracking system, populated from ~FY2020 onward.
#'     Indigenous rows are those with
#'     \code{subapplicant_type == "Indian Tribal Government"} OR
#'     \code{subapplicant_state_abbreviation} in \code{c("AS","GU","MP")}.
#'     Non-award statuses (Did Not Meet HMA Requirements, Not Selected,
#'     Withdrawn, NA) are dropped, matching the prior scope.}
#'   \item{\strong{hma_projects_v4} —
#'     \code{HazardMitigationAssistanceProjectsV4}}{FEMA's legacy
#'     project-level record, which predates FEMAGO and covers the earlier
#'     years of HMGP/PDM/FMA/BRIC. It has no "Indian Tribal Government"
#'     applicant-type flag, so Indigenous rows are selected in two passes
#'     and unioned: (i) case-insensitive regex on \code{subrecipient} /
#'     \code{recipient} matching tribal / Alaska Native / pueblo / OTSA
#'     name tokens plus explicit territory names (Guam, Mariana Islands);
#'     (ii) \code{state} in the full-name territories (American Samoa,
#'     Guam, Northern Mariana Islands). A small exclusion list suppresses
#'     known non-Indigenous false positives that otherwise match the regex
#'     (e.g., Indian Trail Improvement District, Indian River County,
#'     generic "(County)" subrecipients).}
#' }
#'
#' The two endpoints describe different phases of the HMA pipeline: FEMAGO
#' rows are subapplications selected for further review (selection amounts);
#' v4 rows are obligated projects. FEMAGO's Indigenous slice is entirely
#' BRIC (FY2020+ competitions), while v4's Indigenous rows from the same
#' years are obligations of earlier cohorts (program_fy <= 2019) plus live
#' HMGP — as of the 2026-03-04 snapshots, no record-level duplicates exist
#' across the sources (verified: no cross-source pairs share an entity,
#' program, and amount). The overlap risk is confined to BRIC: as v4 catches
#' up, obligations of the same projects FEMAGO shows as selections may begin
#' to appear. No record-level deduplication is attempted; each row carries a
#' \code{source} column naming its origin.
#'
#' @param femago_path Character. Absolute path to the cached FEMAGO
#'   \code{HmaSubapplications} parquet. Defaults to the dated snapshot in the
#'   user's Box \code{openfema/} cache.
#' @param hma_projects_path Character. Absolute path to the cached
#'   \code{HazardMitigationAssistanceProjectsV4} parquet. Defaults to the
#'   dated snapshot in the user's Box \code{openfema/} cache. When the file does
#'   not exist and \code{hma_projects} is \code{NULL}, the function falls
#'   back to a live \code{rfema::open_fema()} call.
#' @param year_range Integer vector of calendar years to include. Default
#'   \code{2000:2026}. Applied to \code{year_submitted} in both branches —
#'   the submission year in FEMAGO and the approval year (with
#'   \code{program_fy} fallback) in v4. v4 rows with neither an approval
#'   date nor a \code{program_fy} are dropped, with a \code{message()}
#'   reporting the count.
#' @param femago Optional tibble. Pre-loaded FEMAGO data. Supplying this
#'   skips the parquet read.
#' @param hma_projects Optional tibble. Pre-loaded v4 data. Supplying this
#'   skips the parquet read and the API fallback.
#'
#' @return A tibble with one row per source × fiscal-year × subapplicant ×
#'   territory_flag × program × project_type and columns:
#'   \describe{
#'     \item{source}{Character. \code{"femago"} or \code{"hma_projects_v4"}.}
#'     \item{year_submitted}{Integer. The year each record is plotted
#'       against: for FEMAGO, the calendar year the subapplication was
#'       submitted to FEMA; for v4, the calendar year the project was
#'       approved (\code{date_approved}), falling back to \code{program_fy}
#'       when no approval date exists or when the approval year is
#'       implausible (outside 1989-2026 — the v4 snapshot contains
#'       data-entry typos such as years 25 and 6023).}
#'     \item{subapplicant_name}{Character.}
#'     \item{territory_flag}{Character. \code{"Territories"} for AS/GU/MP
#'       rows (identified from \code{subapplicant_state_abbreviation} in
#'       FEMAGO and from \code{state} in v4); otherwise
#'       \code{"Federally-recognized tribes and ANVs"}.}
#'     \item{home_state}{Character. 2-letter state code for the
#'       subapplicant — \code{subapplicant_state_abbreviation} from FEMAGO;
#'       derived from v4's full-name \code{state} via
#'       \code{tidycensus::fips_codes}. Used downstream for four-way
#'       community-count QC.}
#'     \item{program}{Character. HMA program area (HMGP, PDM, BRIC, FMA, …).}
#'     \item{project_type}{Character. What the project was for — the mitigation
#'       activity / project type. \code{project_type} in both FEMAGO and v4.}
#'     \item{n_awards}{Integer. Row count within the group.}
#'     \item{federal_share_amount}{Numeric. Sum of federal share
#'       (\code{selection_federal_share_amount} for FEMAGO;
#'       \code{federal_share_obligated} for v4), expressed in 2025 USD —
#'       nominal amounts are deflated from \code{year_submitted} via the
#'       annual PCE price index.}
#'   }

download_openfema_datasets(
  endpoints = c("HmaSubapplications", "HazardMitigationAssistanceProjects"),
  download_directory = default_openfema_box_dir(),
  format_preference = "parquet",
  overwrite = FALSE)

prepare_figure_7_data <- function(
  femago_path = file.path(
    default_openfema_box_dir(), "HmaSubapplications_2026_03_04.parquet"),
  hma_projects_path = file.path(
    default_openfema_box_dir(), "HazardMitigationAssistanceProjects_2026_03_04.parquet"),
  year_range = 2000:2026,
  femago = NULL,
  hma_projects = NULL) {

  ## -- 1. FEMAGO (HmaSubapplications) branch --------------------------------
  femago1 <- if (is.null(femago)) {
    arrow::read_parquet(femago_path) |>
      janitor::clean_names()
  } else {
    femago |>
      janitor::clean_names()
  }

  femago2 <- femago1 |>
    mutate(year_submitted = year(date_submitted_to_fema)) |>
    filter(
      year_submitted %in% year_range,
      !selection_status %in% c(
        "Did Not Meet HMA Requirements", "Not Selected", "Withdrawn", NA),
      subapplicant_type == "Indian Tribal Government" |
        subapplicant_state_abbreviation %in% c("AS", "GU", "MP")) |>
    transmute(
      source = "femago",
      year_submitted,
      subapplicant_name,
      territory_flag = if_else(
        subapplicant_state_abbreviation %in% c("AS", "GU", "MP"),
        "Territories",
        "Federally-recognized tribes and ANVs"),
      ## `home_state` carried straight from FEMAGO's 2-letter state code.
      home_state = subapplicant_state_abbreviation,
      program,
      ## what the project was for (e.g., the mitigation activity / project type).
      project_type = primary_activity,
      ## the amount approved during the selection phase — the most accurate
      ## reflection of intended federal funding in FEMAGO
      federal_share_amount = selection_federal_share_amount)

  femago_summary <- femago2 |>
    summarize(
      .by = c(source, year_submitted, subapplicant_name, territory_flag, home_state, program, project_type),
      n_awards = n(),
      federal_share_amount = sum(federal_share_amount, na.rm = TRUE))
  
  ## -- 2. HMA Projects V4 branch --------------------------------------------
  hma_raw <- if (!is.null(hma_projects)) {
    hma_projects |> janitor::clean_names()
  } else if (file.exists(hma_projects_path)) {
    arrow::read_parquet(hma_projects_path) |>
      janitor::clean_names()
  } else {
    message(
      "HMA Projects V4 cache not found at ", hma_projects_path,
      "; pulling live via rfema (may take several minutes).")
    rfema::open_fema(
      data_set = "HazardMitigationAssistanceProjectsV4",
      ask_before_call = FALSE) |>
      janitor::clean_names() |>
      as_tibble()
  }

  hma1 <- hma_raw |>
    mutate(
      year_approved = year(date_approved),
      .row = row_number())
  
  ## -- 2a. Indigenous identification: regex + territory passes, unioned ----
  ## (i) Regex keyword match on raw subrecipient / recipient names. Each
  ##     token was validated against the full v4 snapshot (all years) to
  ##     confirm zero non-Indigenous / non-territorial matches before
  ##     inclusion. A bare "colony" token is deliberately absent: it matched
  ##     only non-tribal entities (Key Colony Beach and Jupiter Inlet Colony
  ##     FL, the towns of Colony AL/OK and Iowa Colony TX, Old Colony
  ##     Planning Council MA), while every genuine Indian colony in the data
  ##     also matches another token; "indian colony" is kept for
  ##     future-proofing.
  indigenous_regex <- regex(
    str_c(
      "\\b(",
      str_c(
        c("tribe", "tribes", "tribal",
          "native",
          "indian", "indians",
          "rancheria", "pueblo of", "pueblo de", "santa clara pueblo",
          "reservation", "indian colony",
          "band of",
          "nation",
          "confederated", "aian",
          "anv", "anvsa", "otsa",
          "guam", "mariana islands"),
        collapse = "|"),
      ")\\b"),
    ignore_case = TRUE)

  ## Non-Indigenous entities whose names coincidentally match the regex.
  ## Extend this list as new false positives are found in the data.
  exclusion_regex <- regex(
    "Indian Trail Improvement District|\\(county\\)|^Indian River$|Town of",
    ignore_case = TRUE)

  regex_rows <- hma1 |>
    filter(
      str_detect(coalesce(subrecipient, ""), indigenous_regex) |
        str_detect(coalesce(recipient, ""), indigenous_regex),
      !str_detect(coalesce(subrecipient, ""), exclusion_regex)) |>
    pull(.row)

  ## (ii) Territories: AS/GU/MP awards are Indigenous for this project's
  ##      purposes regardless of subapplicant name. OpenFEMA v4's `state`
  ##      column uses full names ("American Samoa", "Guam", "Northern
  ##      Mariana Islands").
  territory_rows <- hma1 |>
    filter(str_detect(coalesce(state, ""), "Samoa|Mariana|Guam")) |>
    pull(.row)

  indigenous_rows <- unique(c(regex_rows, territory_rows))

  ## Rows where the subrecipient itself matched (vs. matching only via the
  ## recipient) — used below to pick which name represents the row.
  subrecipient_match_rows <- hma1 |>
    filter(
      str_detect(coalesce(subrecipient, ""), indigenous_regex),
      !str_detect(coalesce(subrecipient, ""), exclusion_regex)) |>
    pull(.row)

  ## v4's `state` is a full state name; convert to 2-letter code so the
  ## combined output has a uniform `home_state` column.
  v4_state_full_to_abbr = tidycensus::fips_codes |>
    distinct(state_name, state) |>
    deframe()

  hma2 <- hma1 |>
    filter(.row %in% indigenous_rows) |>
    transmute(
      source = "hma_projects_v4",
      indigenous_identification_method = case_when(
        .row %in% regex_rows ~ "regex",
        .row %in% territory_rows ~ "territory"),
      ## slightly different, but aligning with femago data
      ## using year submitted/approved here because fiscal year corresponds to the fiscal year of the nofo
      ## (I think) / lags enormously, so it portrays a weird temporal picture of awards/funding disbursement
      ## Some v4 approval dates carry data-entry typos (years like 25 or
      ## 6023 appear in the 2026-03-04 snapshot). An implausible year would
      ## silently drop the row at the year-window filter AND corrupt the
      ## inflation deflator, so only trust approval years within 1989 (the
      ## first program_fy in v4, when HMGP began) through 2026; otherwise
      ## fall back to program_fy.
      year_submitted = if_else(
        !is.na(year_approved) & between(year_approved, 1989, 2026),
        year_approved,
        as.numeric(program_fy)),
      ## When only the recipient matched the regex, the subrecipient is a
      ## pass-through agency or project label ("NC EMERGENCY MANAGEMENT"
      ## administering the Eastern Cherokee Indian Reservation's mitigation
      ## plan, "Copper River Erosion Project" for the Native Village of
      ## Kluti-Kaah) — name the row for the Indigenous recipient so
      ## downstream community counts don't tally agencies as communities.
      ## Territory rows keep the subrecipient name: their partition and
      ## community-count QC run on home_state, not the name.
      subapplicant_name = if_else(
        .row %in% territory_rows |
          .row %in% subrecipient_match_rows |
          is.na(recipient),
        coalesce(subrecipient, recipient),
        recipient),
      ## Territory state always wins over a regex match on the name — a
      ## tribal-named entity sited in AS/GU/MP is still a territorial row
      ## for this project's partition.
      territory_flag = if_else(
        .row %in% territory_rows,
        "Territories",
        "Federally-recognized tribes and ANVs"),
      home_state = unname(v4_state_full_to_abbr[state]),
      program = program_area,
      ## what the project was for; same column name as FEMAGO in v4.
      project_type,
      # recipient_tribal_indicator,
      federal_share_amount = federal_share_obligated)

  ## many "FALSE" indicator values correspond to legitimate Indigenous communities
  # hma2 %>% 
  #   filter(recipient_tribal_indicator == FALSE) %>%
  #   count(subapplicant_name)
  
  ## Window the v4 branch on the same year_range the FEMAGO branch uses.
  ## Rows with neither an approval date nor a program_fy have no year to
  ## plot against and are dropped — surfaced because silent loss here would
  ## understate awards.
  hma_missing_year_n <- sum(is.na(hma2$year_submitted))
  if (hma_missing_year_n > 0) {
    message(
      "Figure 7 v4: dropping ", hma_missing_year_n,
      " Indigenous row(s) with no date_approved and no program_fy.") }

  hma3 <- hma2 |>
    filter(year_submitted %in% year_range)

  hma_summary <- hma3 |>
    summarize(
      .by = c(source, year_submitted, subapplicant_name, territory_flag, home_state, program, project_type),
      n_awards = n(),
      federal_share_amount = sum(federal_share_amount, na.rm = TRUE))

  ## -- 3. Combine -----------------------------------------------------------
  combined <- bind_rows(femago_summary, hma_summary) |>
    arrange(year_submitted, source, subapplicant_name, program) %>%
    mutate(program = if_else(str_detect(program, "Building"), "BRIC", program)) %>%
    ## HMA awards are reported per subapplicant (territorial rows are the AS/GU/MP
    ## governments), so territorial awards are scaled by the territory-wide
    ## Indigenous (NHPI alone-or-in-combination) population share, mirroring
    ## figure 1; tribal rows are unscaled.
    tidylog::left_join(
      get_territory_indigenous_share_by_state() %>% select(home_state, indigenous_share),
      by = "home_state",
      relationship = "many-to-one") %>%
    mutate(
      ## Unscaled total carried alongside for the territorial-scaling QC.
      federal_share_amount_unscaled = federal_share_amount,
      federal_share_amount = if_else(
        territory_flag == "Territories",
        federal_share_amount * coalesce(indigenous_share, 0),
        federal_share_amount)) %>%
    select(-indigenous_share) %>%
    ## Convert nominal dollars to 2025 USD, deflating each row from its
    ## plotted year (FEMAGO submission year; v4 approval year with
    ## program_fy fallback). 2026-dated rows are treated as already being
    ## in 2025 dollars. The unscaled QC column gets the same treatment.
    adjust_dollars_to_2025(
      year_variable = "year_submitted",
      dollar_variables = c("federal_share_amount", "federal_share_amount_unscaled"))

  message(
    "Figure 7 data prepared: ",
    nrow(femago_summary), " femago rows + ",
    nrow(hma_summary), " hma_projects_v4 rows = ",
    nrow(combined), " total. ",
    "HMA v4 identification — regex: ", length(regex_rows),
    ", territory: ", length(territory_rows), ".")

  combined
}
