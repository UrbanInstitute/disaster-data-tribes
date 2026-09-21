## ---------------------------------------------------------------------------
## 00_run_feature_1.R — feature-1 pipeline orchestrator (ENTRY POINT)
##
## run_feature_1() runs the eligibility pipeline end to end: it sources stages
## 01–03, pulls and classifies FEMA declarations, builds the Indigenous
## community universe, resolves each community to an eligibility tier, and
## writes the dated artifacts under data/fema/ that feature 2 and _feature1.qmd
## consume.
## ---------------------------------------------------------------------------
library(arrow)
library(tidyverse)
library(sf)
library(tigris)
library(janitor)
library(here)

options(tigris_use_cache = TRUE)

source(here::here("scripts", "feature_1", "01_prepare_fema_declarations.R"))
source(here::here("scripts", "feature_1", "02_get_indigenous_universe.R"))
source(here::here("scripts", "feature_1", "03_evaluate_indigenous_eligibility.R"))

#' End-to-end feature 1 pipeline
#'
#' Runs the full feature 1 pipeline: load FEMA declarations, classify,
#' extract tribal- and county-level subsets, build the Indigenous universe,
#' and resolve eligibility tiers. Writes a dated serializable parquet (list
#' columns dropped, counts preserved) plus an RDS file with the full
#' list-column detail.
#'
#' @param year_range Integer vector. Calendar years included when
#'   constructing `eligible_event`, filtered on the calendar year FEMA
#'   *issued* the declaration (`year(declaration_date)`), NOT the year
#'   the incident occurred. Passed to `classify_declarations()`,
#'   `get_tribal_declarations()`, `get_county_declarations()`, and
#'   `get_statewide_declarations()`. Default `2014:2026` — covers the
#'   widest window any downstream feature consumes. Program-specific
#'   sub-windows (EDA = 2023:2024, PA/HMGP/SBA = 2025:2026) are applied
#'   in `build_feature1_search()`, not here.
#' @param tigris_year Integer. TIGER vintage for `tigris::native_areas()`
#'   and `tigris::counties()`. Default `2024`.
#' @param refresh_fema Logical. If `TRUE`, pull a fresh FEMA snapshot via
#'   `refresh_fema_declarations()` before loading. Default `FALSE`.
#' @param out_dir Character. Directory for the written outputs. Created
#'   if missing. Default `here::here("data", "fema")`.
#'
#' @return The full resolved eligibility tibble (as returned by
#'   `resolve_eligibility()`), returned invisibly. See `resolve_eligibility()`
#'   for the column schema — including the list-columns `direct_declarations`,
#'   `intersecting_county_declarations`, and `statewide_declarations`.
#'
#'   Side effects — all written to `<out_dir>` with a `_YYYY_MM_DD` suffix:
#'
#'   \describe{
#'     \item{`feature_1_classified_declarations_*.parquet`}{Output of
#'       `classify_declarations()` — full DR table with eligibility/routing
#'       columns added.}
#'     \item{`feature_1_tribal_declarations_*.parquet`}{Output of
#'       `get_tribal_declarations()` — one row per tribal-direct declaration
#'       in `year_range`.}
#'     \item{`feature_1_county_declarations_*.parquet`}{Output of
#'       `get_county_declarations()` — one row per (county × disaster) in
#'       `year_range`.}
#'     \item{`feature_1_statewide_declarations_*.parquet`}{Output of
#'       `get_statewide_declarations()` — one row per (state × disaster)
#'       for statewide declarations (including AS/GU/MP territorial) in
#'       `year_range`.}
#'     \item{`feature_1_universe_*.parquet`}{Output of
#'       `get_indigenous_universe()` with geometry dropped and list-columns
#'       (`aliases`, `state_list`) collapsed to pipe-delimited strings.}
#'     \item{`feature_1_eligibility_*.parquet`}{Flat resolved eligibility
#'       table. List-columns replaced by integer counts (`n_direct` —
#'       distinct disaster numbers, robust to multi-state tribal
#'       declarations that ship one row per state; `n_intersect` and
#'       `n_statewide` — underlying declaration rows). Federal-subset match provenance is
#'       carried in `resolution` / `confidence` / `note` (from
#'       `build_indigenous_tribe_polygons()`); `direct_match_precision_max`
#'       (highest name-match precision across a community's tribal-direct FEMA
#'       declarations) is also retained.}
#'     \item{`feature_1_eligibility_detail_*.parquet`}{Full resolved tibble
#'       with list-columns preserved — use when you need the underlying
#'       declaration rows per community. Read via
#'       `arrow::read_parquet(path) %>% collect()` to materialize as a
#'       tibble; nested list-of-struct columns round-trip to list-columns
#'       of tibbles.}
#'     \item{`feature_1_universe_sf_*.rds`}{The `sf` universe returned by
#'       `get_indigenous_universe()` with geometry preserved — use for
#'       mapping and spatial inspection.}
#'     \item{`feature_1_counties_sf_*.rds`}{The `sf` county polygons fed to
#'       `resolve_eligibility()` (TIGER CB vintage `tigris_year`, reprojected
#'       to EPSG:4269).}
#'   }
run_feature_1 <- function(year_range = 2014:2026,
                          tigris_year = 2024,
                          refresh_fema = FALSE,
                          out_dir = here::here("data", "fema")) {

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  snapshot_date <- format(Sys.Date(), "%Y_%m_%d")
  dated_path <- function(stem) {
    file.path(out_dir, str_c("feature_1_", stem, "_", snapshot_date, ".parquet"))
  }
  collapse_list_col <- function(x) {
    purrr::map_chr(x, ~ {
      v <- unlist(.x)
      if (length(v) == 0) NA_character_ else str_c(v, collapse = "|")
    })
  }
  list_col_nrow <- function(x) {
    purrr::map_int(x, ~ if (is.null(.x)) 0L else nrow(.x))
  }
  ## Distinct disasters rather than raw rows: a multi-state tribal declaration
  ## (e.g., a Navajo-style disaster declared in several states) ships one row
  ## per state for the same disaster_number, which would inflate a row count.
  list_col_n_disasters <- function(x) {
    purrr::map_int(
      x, ~ if (is.null(.x)) 0L else n_distinct(.x$disaster_number))
  }

  declarations_raw <- get_fema_declarations(refresh = refresh_fema)
  classified <- classify_declarations(declarations_raw, year_range = year_range)
  tribal_declarations <- get_tribal_declarations(classified, year_range = year_range)
  county_declarations <- get_county_declarations(classified, year_range = year_range)
  statewide_declarations <- get_statewide_declarations(classified, year_range = year_range)

  universe <- get_indigenous_universe(year = tigris_year)

  counties_sf <- tigris::counties(year = tigris_year, cb = TRUE) %>%
    sf::st_transform(CRS_GEO)

  resolved <- resolve_eligibility(
    universe = universe,
    tribal_declarations = tribal_declarations,
    county_declarations = county_declarations,
    statewide_declarations = statewide_declarations,
    counties_sf = counties_sf)

  universe_flat <- universe %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    mutate(
      aliases = collapse_list_col(aliases),
      state_list = collapse_list_col(state_list))

  resolved_flat <- resolved %>%
    mutate(
      n_direct = list_col_n_disasters(direct_declarations),
      n_intersect = list_col_nrow(intersecting_county_declarations),
      n_statewide = list_col_nrow(statewide_declarations),
      aliases = collapse_list_col(aliases),
      state_list = collapse_list_col(state_list)) %>%
    select(
      -direct_declarations, -intersecting_county_declarations,
      -statewide_declarations)

  classified_path <- dated_path("classified_declarations")
  tribal_path <- dated_path("tribal_declarations")
  county_path <- dated_path("county_declarations")
  statewide_path <- dated_path("statewide_declarations")
  universe_path <- dated_path("universe")
  flat_path <- dated_path("eligibility")
  detail_path <- dated_path("eligibility_detail")
  universe_sf_path <- file.path(
    out_dir,
    str_c("feature_1_universe_sf_", snapshot_date, ".rds"))
  counties_sf_path <- file.path(
    out_dir,
    str_c("feature_1_counties_sf_", snapshot_date, ".rds"))

  arrow::write_parquet(classified, classified_path)
  arrow::write_parquet(tribal_declarations, tribal_path)
  arrow::write_parquet(county_declarations, county_path)
  arrow::write_parquet(statewide_declarations, statewide_path)
  arrow::write_parquet(universe_flat, universe_path)
  arrow::write_parquet(resolved_flat, flat_path)
  arrow::write_parquet(resolved, detail_path)
  saveRDS(universe, universe_sf_path)
  saveRDS(counties_sf, counties_sf_path)

  message("Wrote classified declarations to ", classified_path)
  message("Wrote tribal declarations to ", tribal_path)
  message("Wrote county declarations to ", county_path)
  message("Wrote statewide declarations to ", statewide_path)
  message("Wrote universe (flat) to ", universe_path)
  message("Wrote flat eligibility table to ", flat_path)
  message("Wrote full detail (with list-columns) to ", detail_path)
  message("Wrote universe (with geometry) to ", universe_sf_path)
  message("Wrote counties (with geometry) to ", counties_sf_path)

  invisible(resolved)
}
