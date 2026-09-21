## ---------------------------------------------------------------------------
## get_nri.R — FEMA National Risk Index data, mapped to communities
##
## get_nri(): loads two NRI v1.20 tables (each downloaded and cached as
## parquet under data/ when absent):
##   * the tract-level table, crosswalked to Indigenous communities via
##     get_tribal_crosswalk(source_geography = "tract"); and
##   * FEMA's published county-level table, used as-is for the all-US-county
##     baseline. County risk scores are percentile ranks computed among
##     counties (median ~50 by construction), which no aggregation of
##     tract-level percentile ranks reproduces.
## Puerto Rico and USVI are dropped from both tables (project policy: they
## are excluded from every universe, including the all-US-county comparison).
## Expected annual loss dollars are converted from the NRI release's
## 2024-dollar basis to 2025 USD. Sourced/called by
## scripts/feature_2/_feature2.qmd (figures 3-4).
## ---------------------------------------------------------------------------
library(tidyverse)
library(janitor)

source(here::here("scripts", "feature_2", "get_tribal_crosswalks.R"))

## Download an NRI table zip, extract its single Table CSV, and cache it as
## parquet at `parquet_path`. No-op when the parquet already exists. Called
## for both the tract- and county-level tables inside get_nri().
cache_nri_table = function(nri_url, parquet_path) {
  if (file.exists(parquet_path)) return(invisible(parquet_path))

  dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)

  nri_zip_tmp <- tempfile(fileext = ".zip")
  nri_unzip_tmp <- tempfile()
  dir.create(nri_unzip_tmp)

  download.file(nri_url, destfile = nri_zip_tmp, mode = "wb")
  unzip(nri_zip_tmp, exdir = nri_unzip_tmp)

  extracted_csv <- list.files(
    nri_unzip_tmp, pattern = "Table.*\\.csv$", full.names = TRUE, recursive = TRUE)

  if (length(extracted_csv) != 1) {
    stop("Expected exactly one CSV in the NRI zip; found ", length(extracted_csv), ".")
  }

  df_raw = read_csv(extracted_csv)

  arrow::write_parquet(df_raw, parquet_path)

  unlink(c(nri_zip_tmp, nri_unzip_tmp), recursive = TRUE)

  invisible(parquet_path)
}

get_nri = function() {

  ## for crosswalking data from tracts to Indigenous communities
  tribal_tract_crosswalk <- get_tribal_crosswalk(source_geography = "tract")

  nri_tract_path = here::here("data", "nri-tracts", "nri_tracts_fema_2026.parquet")
  nri_county_path = here::here("data", "nri-counties", "nri_counties_fema_2026.parquet")

  ## download the data from source if it's not available locally
  cache_nri_table(
    nri_url = "https://www.fema.gov/about/reports-and-data/openfema/nri/v120/NRI_Table_CensusTracts.zip",
    parquet_path = nri_tract_path)
  cache_nri_table(
    nri_url = "https://www.fema.gov/about/reports-and-data/openfema/nri/v120/NRI_Table_Counties.zip",
    parquet_path = nri_county_path)

  nri_tracts1 <- arrow::read_parquet(nri_tract_path) %>%
    clean_names()

  ##Prep NRI data
  nri_tract_clean <- nri_tracts1 %>%
    select(
      tract_geoid = tractfips,
      population,
      eal_all_hazards = eal_valt,
      risk_score,
      sovi_score,
      resl_score,
      matches("ealt$|evnts")) %>%
    ## Project policy: Puerto Rico (FIPS 72) and the US Virgin Islands (78)
    ## are excluded from this project everywhere, including the
    ## all-US-county comparison universe (matching figure 2's county
    ## baseline). The NRI table carries both, so drop them here.
    filter(!str_sub(tract_geoid, 1, 2) %in% c("72", "78"))

  ## Convert every expected-annual-loss dollar column to 2025 USD. The
  ## December 2025 NRI release reports EAL in 2024 dollars (project
  ## decision, July 2026), so all rows deflate from the single nominal year
  ## 2024. Event counts and the risk / SoVI / resilience scores are not
  ## dollar-denominated and are left unchanged.
  nri_eal_columns = nri_tract_clean %>%
    select(eal_all_hazards, matches("ealt$")) %>%
    colnames()

  nri_tract_clean2 = nri_tract_clean %>%
    mutate(nri_dollar_year = 2024) %>%
    adjust_dollars_to_2025(
      year_variable = "nri_dollar_year",
      dollar_variables = nri_eal_columns) %>%
    select(-nri_dollar_year)

  ## County baseline from FEMA's published county-level NRI table, NOT an
  ## aggregation of the tract table: county risk / SoVI / resilience scores
  ## are percentile ranks computed among counties, and a population-weighted
  ## mean of tract-level percentile ranks is not itself a percentile (it
  ## produced a county "median" risk score of ~69 rather than ~50 in earlier
  ## versions of this function). County EAL dollars get the same 2024->2025
  ## USD conversion as the tract table.
  county_nri = arrow::read_parquet(nri_county_path) %>%
    clean_names() %>%
    select(
      county_geoid = stcofips,
      population,
      eal_all_hazards = eal_valt,
      risk_score,
      sovi_score,
      resl_score) %>%
    ## Same PR/USVI exclusion as the tract table.
    filter(!str_sub(county_geoid, 1, 2) %in% c("72", "78")) %>%
    mutate(nri_dollar_year = 2024) %>%
    adjust_dollars_to_2025(
      year_variable = "nri_dollar_year",
      dollar_variables = "eal_all_hazards") %>%
    select(-nri_dollar_year) %>%
    mutate(eal_per_capita = eal_all_hazards / population)

  ## two tracts each from AS and CNMI do not join -- not ideal, but these represent less than
  ## 10% of all tracts in those territories, so the non-match does not appear to be systematic
  stopifnot(
    tribal_tract_crosswalk %>%
      anti_join(nri_tract_clean2, by = c("source_geoid" = "tract_geoid")) %>%
      nrow() < 5)

  ## `territory_flag` and `home_state` are carried through from the
  ## crosswalk so downstream code can partition / classify communities
  ## without re-deriving from `target_geography_name` (which is now a
  ## county-equivalent name for territorial rows).
  indigenous_nri <- tribal_tract_crosswalk %>%
    left_join(nri_tract_clean2, by = c("source_geoid" = "tract_geoid")) %>%
    summarize(
      .by = c(target_geoid, target_geography_name, territory_flag, home_state),
      ## Scores are averaged over the community's tracts, weighted by
      ## allocated population. Computed before
      ## the across() below, which overwrites `population` with its allocated
      ## sum (summarize() evaluates sequentially). Communities with no scored
      ## tracts return NA — notably all AS/GU/MP rows, which NRI does not
      ## score — rather than NaN.
      across(
        .cols = matches("score"),
        .fns = ~ {
          tract_weights <- population * allocation_factor_source_to_target
          scoreable <- !is.na(.x) & !is.na(tract_weights) & tract_weights > 0
          if (!any(scoreable)) NA_real_
          else weighted.mean(.x[scoreable], tract_weights[scoreable])
        }),
      ## EAL dollars, population, and hazard-event counts are all
      ## allocation-weighted sums across the community's tracts.
      across(
        .cols = matches("eal|population$|evnts"),
        .fns = ~ sum(.x * allocation_factor_source_to_target, na.rm = TRUE))) %>%
    ## this is an estimate of the population, reflecting crosswalking weights from
    ## tracts to target geographies
    rename(population_crosswalked = population) %>%
    mutate(eal_per_capita = eal_all_hazards / population_crosswalked)

  return(list(indigenous_nri, county_nri))
}
