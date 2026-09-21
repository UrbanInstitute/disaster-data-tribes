## ---------------------------------------------------------------------------
## figure_1_cumulative_damages.R — data prep for feature-2 figure 1
##
## prepare_figure_1_data(): cumulative SHELDUS hazard damages for Indigenous
## communities (territorial damages scaled by NHPI alone-or-in-combination
## share). Damages are converted from SHELDUS's 2024-dollar basis to 2025
## USD. Sourced/called by scripts/feature_2/_feature2.qmd; the chart is
## assembled and written to outputs/figure1*.png there.
## ---------------------------------------------------------------------------
library(arrow)
library(tidyverse)
library(sf)
library(janitor)
library(here)
library(climateapi)
library(urbnthemes)

source(here::here("scripts", "utilities", "utilities.R"))
source(here::here("scripts", "feature_2", "get_tribal_crosswalks.R"))

options(scipen = 9999)

prepare_figure_1_data = function() {

  ## SHELDUS 24.0 reports every dollar value in 2024 USD (the source file is
  ## "..._2024USD"), regardless of event year — so all rows deflate from the
  ## single nominal year 2024 to the project-wide 2025-USD basis.
  raw_sheldus = get_sheldus() %>%
    mutate(sheldus_dollar_year = 2024) %>%
    adjust_dollars_to_2025(
      year_variable = "sheldus_dollar_year",
      dollar_variables = c("damage_property", "damage_crop")) %>%
    select(-sheldus_dollar_year)

  ## Population-weighted county→AIANNH crosswalk, 2020 vintage to align with
  ## SHELDUS county GEOIDs. SDTSAs are excluded; AS/GU/MP county-equivalents
  ## are appended with 2020 Census total population from the Island Areas DP.
  county_to_tribal_crosswalk = get_tribal_crosswalk(
    source_geography = "county",
    source_year = 2020,
    target_year = 2020) %>%
    mutate(population_2020 = as.numeric(population_2020))

  ## Some interpolated AIANNH populations round below one person; floor at 1.
  warning("Some Indigenous areas have interpolated populations of zero or less than one.")
  indigenous_populations = county_to_tribal_crosswalk %>%
    summarize(
      .by = c(target_geoid, target_geography_name),
      population_2020 = sum(population_2020 * allocation_factor_source_to_target, na.rm = TRUE)) %>%
    mutate(population_2020 = if_else(population_2020 < 1, 1, population_2020))

  ## Standard Census regions/divisions, with two modifications: Alaska is its
  ## own region (division stays "Pacific"), and AS/GU/MP form a "Territories"
  ## region/division (no standard Census division applies).
  census_division_lookup = tribble(
    ~state_name,                ~division,            ~region,
    "Connecticut",              "New England",        "Northeast",
    "Maine",                    "New England",        "Northeast",
    "Massachusetts",            "New England",        "Northeast",
    "New Hampshire",            "New England",        "Northeast",
    "Rhode Island",             "New England",        "Northeast",
    "Vermont",                  "New England",        "Northeast",
    "New Jersey",               "Middle Atlantic",    "Northeast",
    "New York",                 "Middle Atlantic",    "Northeast",
    "Pennsylvania",             "Middle Atlantic",    "Northeast",
    "Illinois",                 "East North Central", "Midwest",
    "Indiana",                  "East North Central", "Midwest",
    "Michigan",                 "East North Central", "Midwest",
    "Ohio",                     "East North Central", "Midwest",
    "Wisconsin",                "East North Central", "Midwest",
    "Iowa",                     "West North Central", "Midwest",
    "Kansas",                   "West North Central", "Midwest",
    "Minnesota",                "West North Central", "Midwest",
    "Missouri",                 "West North Central", "Midwest",
    "Nebraska",                 "West North Central", "Midwest",
    "North Dakota",             "West North Central", "Midwest",
    "South Dakota",             "West North Central", "Midwest",
    "Delaware",                 "South Atlantic",     "South",
    "District of Columbia",     "South Atlantic",     "South",
    "Florida",                  "South Atlantic",     "South",
    "Georgia",                  "South Atlantic",     "South",
    "Maryland",                 "South Atlantic",     "South",
    "North Carolina",           "South Atlantic",     "South",
    "South Carolina",           "South Atlantic",     "South",
    "Virginia",                 "South Atlantic",     "South",
    "West Virginia",            "South Atlantic",     "South",
    "Alabama",                  "East South Central", "South",
    "Kentucky",                 "East South Central", "South",
    "Mississippi",              "East South Central", "South",
    "Tennessee",                "East South Central", "South",
    "Arkansas",                 "West South Central", "South",
    "Louisiana",                "West South Central", "South",
    "Oklahoma",                 "West South Central", "South",
    "Texas",                    "West South Central", "South",
    "Arizona",                  "Mountain",           "West",
    "Colorado",                 "Mountain",           "West",
    "Idaho",                    "Mountain",           "West",
    "Montana",                  "Mountain",           "West",
    "Nevada",                   "Mountain",           "West",
    "New Mexico",               "Mountain",           "West",
    "Utah",                     "Mountain",           "West",
    "Wyoming",                  "Mountain",           "West",
    "California",               "Pacific",            "West",
    "Hawaii",                   "Pacific",            "West",
    "Oregon",                   "Pacific",            "West",
    "Washington",               "Pacific",            "West",
    "Alaska",                   "Alaska",            "Alaska",
    "American Samoa",           "Territories",        "Territories",
    "Guam",                     "Territories",        "Territories",
    "Northern Mariana Islands", "Territories",        "Territories")

  state_fips_to_region = tidycensus::fips_codes %>%
    select(state_name, state_fips = state_code, home_state = state) %>%
    distinct() %>%
    tidylog::inner_join(census_division_lookup, by = "state_name", relationship = "one-to-one")

  ## Assign each community to a primary state (the source state holding the
  ## largest share of the community's population-weighted area); region,
  ## division, and `home_state` (2-letter abbreviation) follow from that
  ## primary state. `home_state` is used by downstream community-count QC
  ## to break out Alaskan / Hawaiian / territorial / contiguous tribes.
  community_primary_state = county_to_tribal_crosswalk %>%
    select(-home_state) %>%
    tidylog::left_join(state_fips_to_region, by = "state_fips", relationship = "many-to-one") %>%
    summarize(
      .by = c(target_geoid, target_geography_name, state_name, home_state, region, division),
      community_state_population = sum(
        population_2020 * allocation_factor_source_to_target, na.rm = TRUE)) %>%
    slice_max(
      community_state_population,
      by = c(target_geoid, target_geography_name),
      n = 1,
      with_ties = FALSE) %>%
    select(target_geoid, target_geography_name, state_name, home_state, region, division)

  ## Damages allocated from source counties to AIANNH targets via the
  ## population-weighted allocation factor, then summed to community-year-hazard.
  ## Territorial county-equivalents enter the crosswalk whole (allocation
  ## factor 1), so their damages are additionally scaled to each unit's
  ## Indigenous population share — Native Hawaiian and Other Pacific Islander
  ## alone-or-in-combination over total population (2020 Island Areas DP).
  ## Federally-recognized tribal rows are left unscaled: their
  ## `allocation_factor_source_to_target` already encodes the share of the
  ## source county's population living in the tribal area. A zero-population
  ## territorial county-equivalent (`indigenous_share` NA) contributes no
  ## damages.
  community_year_hazard_damages = raw_sheldus %>%
    select(source_geoid = GEOID, damage_property, damage_crop, year, hazard) %>%
    tidylog::left_join(
      county_to_tribal_crosswalk,
      by = "source_geoid",
      relationship = "many-to-many") %>%
    filter(!is.na(target_geoid)) %>%
    mutate(
      effective_weight = allocation_factor_source_to_target *
        if_else(
          territory_flag == "Territories",
          coalesce(indigenous_share, 0),
          1)) %>%
    summarize(
      .by = c(target_geoid, target_geography_name, year, hazard),
      ## Unscaled territorial total (allocation only, no Indigenous-share
      ## factor) carried alongside for the territorial-scaling QC; equals
      ## `damage_total` for federally-recognized tribal rows. Computed before
      ## the across() below, which overwrites `damage_property`/`damage_crop`
      ## with their scaled sums within this summarize().
      damage_total_unscaled = sum(
        (coalesce(damage_property, 0) + coalesce(damage_crop, 0)) *
          allocation_factor_source_to_target,
        na.rm = TRUE),
      across(
        .cols = c(damage_property, damage_crop),
        .fns = ~ sum(.x * effective_weight, na.rm = TRUE))) %>%
    mutate(damage_total = damage_property + damage_crop)

  result = community_year_hazard_damages %>%
    tidylog::left_join(
      community_primary_state,
      by = c("target_geoid", "target_geography_name"),
      relationship = "many-to-one") %>%
    tidylog::left_join(
      indigenous_populations,
      by = c("target_geoid", "target_geography_name"),
      relationship = "many-to-one")

  return(result)
}
