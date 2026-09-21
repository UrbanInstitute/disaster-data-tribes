## ---------------------------------------------------------------------------
## get_tribal_crosswalks.R — source-geography → Indigenous-community crosswalks
##
## get_tribal_crosswalk() maps a source geography (tract / ZCTA / county) to
## Indigenous communities (population-weighted, with AS/GU/MP territorial rows
## appended); the file also derives territorial population shares. Shared by the
## feature-2 figure-prep scripts (get_nri.R, figure_1/5/7) to allocate
## tract/ZCTA-level data down to communities.
## ---------------------------------------------------------------------------
library(tidyverse)
library(crosswalk)
library(sf)

## AIANNHCE -> tribe membership with proportional shares
## (get_aiannh_tribe_crosswalk()), used to collapse the raw `crosswalk` AIANNH
## targets into the Feature 1 consolidated-tribe universe; shared polygons are
## split equally across co-owning tribes.
source(here::here("scripts", "utilities", "get_aiannh_tribe_crosswalk.R"))
source(here::here("scripts", "utilities", "utilities.R"))  # CRS_GEO / CRS_PROJ

#' Fetch 2020 Census population and Indigenous share per territorial county-equivalent
#'
#' Pulls AS/GU/MP county-equivalent total population (variable `DP1_0001C` —
#' "Number!!SEX AND AGE!!Total population") and the Native Hawaiian and Other
#' Pacific Islander (NHPI) alone-or-in-combination population from the 2020
#' Demographic Profile of Island Areas, then computes the Indigenous
#' population share per county-equivalent. The NHPI alone-or-in-combination
#' race code differs by territory: `DP1_0100C` for American Samoa (`dpas`) and
#' `DP1_0104C` for Guam (`dpgu`) and the Northern Mariana Islands (`dpmp`).
#' The Island Areas live in their own per-territory DP summary files; we use DP
#' uniformly because tidycensus exposes DHC for AS/GU/VI but not CNMI.
#'
#' @return A tibble with one row per territorial county-equivalent:
#'   `source_geoid` (5-digit county-equivalent FIPS), `population_2020`
#'   (total), `population_nhpi_aoic` (NHPI alone-or-in-combination), and
#'   `indigenous_share` (`population_nhpi_aoic / population_2020`, or `NA`
#'   where total population is zero).
get_territory_populations_2020 = function() {
  ## Per-territory Island Areas DP sumfile plus the NHPI-alone-or-in-
  ## combination race code (the variable number differs by territory).
  ## `DP1_0001C` ("Total population") is the denominator in all three.
  territory_specs = tibble::tribble(
    ~state_abbr, ~sumfile, ~nhpi_variable,
    "AS",        "dpas",   "DP1_0100C",
    "GU",        "dpgu",   "DP1_0104C",
    "MP",        "dpmp",   "DP1_0104C")

  purrr::pmap(
      territory_specs,
      function(state_abbr, sumfile, nhpi_variable) {
        tidycensus::get_decennial(
          geography = "county",
          variables = c(
            population_2020 = "DP1_0001C",
            population_nhpi_aoic = nhpi_variable),
          year = 2020,
          sumfile = sumfile,
          state = state_abbr,
          output = "wide") %>%
          as_tibble() }) %>%
    bind_rows() %>%
    transmute(
      source_geoid = GEOID,
      population_2020 = as.numeric(population_2020),
      population_nhpi_aoic = as.numeric(population_nhpi_aoic),
      indigenous_share = if_else(
        population_2020 > 0,
        population_nhpi_aoic / population_2020,
        NA_real_))
}

#' Territory-wide Indigenous (NHPI alone-or-in-combination) population share
#'
#' Aggregates `get_territory_populations_2020()` from county-equivalents up to
#' the territory (state) level: the sum of NHPI alone-or-in-combination
#' population over the sum of total population across each territory's
#' county-equivalents. Used to scale territory-government-level dollar estimates
#' — e.g., Public Assistance and HMA awards, which are reported per territorial
#' government rather than per county-equivalent — to the Indigenous population,
#' mirroring figure 1's territorial scaling.
#'
#' @return A tibble with one row per territory: `home_state` (`"AS"`, `"GU"`,
#'   `"MP"`), `population_2020`, `population_nhpi_aoic`, and `indigenous_share`.
get_territory_indigenous_share_by_state = function() {
  get_territory_populations_2020() %>%
    mutate(state_fips = str_sub(source_geoid, 1, 2)) %>%
    summarize(
      .by = state_fips,
      population_2020 = sum(population_2020, na.rm = TRUE),
      population_nhpi_aoic = sum(population_nhpi_aoic, na.rm = TRUE)) %>%
    mutate(
      indigenous_share = if_else(
        population_2020 > 0, population_nhpi_aoic / population_2020, NA_real_),
      home_state = recode(state_fips, "60" = "AS", "66" = "GU", "69" = "MP")) %>%
    select(home_state, population_2020, population_nhpi_aoic, indigenous_share)
}

#' Build a source-geography-to-AIANNH crosswalk, including US territories
#'
#' Wraps `crosswalk::get_crosswalk()` for the population-weighted
#' source-geography-to-AIANNH mapping (federally-recognized tribes and
#' associated areas across the 50 states). State-designated tribal areas
#' (SDTSAs) are excluded. Territorial rows for Guam, American Samoa, and the
#' Commonwealth of the Northern Mariana Islands are then appended, with each
#' territorial **county-equivalent** as its own target (analogous to a
#' distinct tribe) — AS contributes 5 districts, GU contributes 1 county,
#' and MP contributes 4 municipalities, for 10 territorial target rows total.
#' Source rows are assigned to a single county-equivalent target with an
#' allocation factor of 1: tracts by their parent county FIPS, ZCTAs by a
#' point-in-polygon assignment of an internal point to the county-equivalent.
#' For `source_geography = "county"`, territorial rows carry 2020 Census
#' total populations from the Demographic Profile of Island Areas (variable
#' `DP1_0001C`); for `"tract"` and `"zcta"`, territorial rows carry `NA`
#' populations (Island Areas tract / ZCTA-level population is not fetched
#' here). Hawaiian Home Lands targets are dropped at the end.
#'
#' @param source_geography Character scalar. The source geography to map from.
#'   One of `"tract"` (default), `"zcta"`, or `"county"`.
#' @param source_year Integer. Vintage of the source geography, passed to
#'   `crosswalk::get_crosswalk()` and `tigris::*()`. Default `2024`.
#' @param target_year Integer. Vintage of the AIANNH target geography, passed
#'   to `crosswalk::get_crosswalk()`. Default `2024`.
#'
#' @return A tibble with one row per (source geography unit, AIANNH) pair from
#'   the 50 states, plus rows for each territorial source unit assigned to its
#'   parent county-equivalent. Columns match those returned by
#'   `crosswalk::get_crosswalk()` plus a `territory_flag` column with values
#'   `"Territories"` or `"Federally-recognized tribes and ANVs"` so downstream
#'   code does not need to re-derive the partition from a name list, plus
#'   `home_state` (2-letter state code) per target — the state holding the
#'   largest allocation-weighted share of the target's source rows.
#'   Territorial rows carry an `allocation_factor_source_to_target` of 1 and
#'   `NA` for `allocation_factor_target_to_source`. `population_2020` is real
#'   for territorial rows when `source_geography = "county"`; `NA` otherwise.
#'   Territorial rows also carry `population_nhpi_aoic` (Native Hawaiian and
#'   Other Pacific Islander alone-or-in-combination, 2020 Island Areas DP) and
#'   `indigenous_share` (`population_nhpi_aoic / population_2020` for the row's
#'   county-equivalent) for **every** `source_geography`, so downstream figures
#'   can scale territorial dollar/loss estimates to the Indigenous population
#'   share. Both are `NA` for federally-recognized tribal rows.
get_tribal_crosswalk = function(
    source_geography = "tract",
    source_year = 2024,
    target_year = 2024) {

  ## get_crosswalk() stops rather than creating its cache folder, so create it
  ## here on first use.
  crosswalk_cache_dir = here::here("data", "crosswalks")
  if (!dir.exists(crosswalk_cache_dir)) {
    dir.create(crosswalk_cache_dir, recursive = TRUE) }

  tribal_crosswalk_objects = get_crosswalk(
    source_geography = source_geography,
    source_year = source_year,
    target_geography = "aiannh",
    target_year = target_year,
    weight = "population",
    cache = crosswalk_cache_dir)
  
  ## Collapse the raw Census AIANNH targets into the Feature 1 consolidated
  ## universe via the canonical AIANNHCE -> single-tribe membership. The
  ## consolidation is defined/audited at the 2024 tigris vintage; AIANNHCE
  ## codes are stable across the 2020-2024 crosswalk vintages, so the 2024
  ## membership joins cleanly onto whichever `target_year` the crosswalk uses.
  ## The inner join is also the universe filter: SDTSAs, `tigris_no_tribe`
  ## polygons, and Hawaiian Home Lands are absent from the membership and so are
  ## dropped here (replacing the old name-substring SDTSA filter and the Iipay
  ## "3550" name patch).
  aiannh_tribe_membership = get_aiannh_tribe_crosswalk(year = 2024)

  ## geocorr's ZCTA→AIANNH crosswalk omits `state_fips` (ZCTAs span states),
  ## unlike the tract/county sources that carry it. Add it as NA when absent so
  ## the (state_fips, source, tribe) grouping and the downstream `home_state`
  ## derivation run unchanged; ZCTA-sourced federally-recognized rows then carry
  ## an NA `home_state`, and the four-way community-count QC falls back to its
  ## name regex. Territorial rows derive `state_fips` independently below.
  step_1_crosswalk = tribal_crosswalk_objects$crosswalks$step_1
  if (!"state_fips" %in% names(step_1_crosswalk)) {
    step_1_crosswalk = step_1_crosswalk %>% mutate(state_fips = NA_character_)
  }

  tribal_crosswalk = step_1_crosswalk %>%
    ## AIANNHCE codes are 4-digit zero-padded; pad defensively in case the
    ## crosswalk returns them as integers (which would drop the leading zero
    ## and break the join for codes like Pit River's "0215").
    mutate(target_geoid = str_pad(as.character(target_geoid), 4, pad = "0")) %>%
    ## many-to-many: a shared AIANNH polygon (co-owned OTSA / JUA) fans each
    ## source row out to every co-owning tribe, carrying that tribe's
    ## equal-shares fraction (`tribe_share`; 1 for unshared polygons).
    tidylog::inner_join(
      aiannh_tribe_membership %>%
        transmute(
          target_geoid = geoid_native,
          tribe_geoid = representative_geoid,
          tribe_name,
          tribe_share),
      by = "target_geoid",
      relationship = "many-to-many") %>%
    ## A single source geography can touch several of a tribe's component
    ## polygons (e.g. multiple Pit River rancherias, a shared OTSA plus a
    ## tribe's own reservation), so sum the source->target allocation within
    ## (source, tribe), weighting each polygon's allocation by the tribe's
    ## share of it so a co-owned polygon's population is divided across its
    ## co-owners (shares sum to 1 -> nothing is double-counted).
    ## `population_2020` is the SOURCE geography's population (constant
    ## across its target rows) -> first().
    summarize(
      .by = c(state_fips, source_geoid, source_geography_name,
              tribe_geoid, tribe_name),
      allocation_factor_source_to_target =
        sum(allocation_factor_source_to_target * tribe_share, na.rm = TRUE),
      population_2020 = first(population_2020)) %>%
    transmute(
      state_fips,
      source_geoid,
      target_geoid = tribe_geoid,
      source_geography_name,
      target_geography_name = tribe_name,
      allocation_factor_source_to_target,
      ## target->source share is undefined once several AIANNH polygons collapse
      ## into one tribe; downstream code uses only the source->target factor.
      allocation_factor_target_to_source = NA_real_,
      source_geography = source_geography,
      target_geography = "aiannh",
      weighting_factor = "population",
      population_2020,
      territory_flag = "Federally-recognized tribes and ANVs")

  # tribal_crosswalk %>%
  #   filter(str_detect(source_geography_name, " AK")) %>%
  #   summarize(.by = target_geography_name, population_2020_anvsa = sum(allocation_factor_source_to_target * as.numeric(population_2020))) %>%
  #   arrange(desc(population_2020_anvsa)) %>%
  #   filter(!str_detect(target_geography_name, "ANVSA"))
  #   print(n = Inf)

  ## The baseline crosswalk only covers federally-recognized tribes in the 50
  ## states. We append territorial rows for Guam, American Samoa, and the
  ## Commonwealth of the Northern Mariana Islands, with each territorial
  ## county-equivalent as its own target (analogous to a distinct tribe), per
  ## this project's convention. AS has 5 districts, GU has 1 county, and MP
  ## has 4 municipalities — 10 territorial target rows in total.
  territory_state_fips = c("60", "66", "69")

  territory_counties_sf = tigris::counties(year = source_year, cb = TRUE) %>%
    janitor::clean_names() %>%
    filter(statefp %in% territory_state_fips)

  territory_county_lookup = territory_counties_sf %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    transmute(
      county_geoid = geoid,
      county_name = namelsad)

  if (source_geography == "tract") {
    ## Each territorial tract is rolled up to its parent county-equivalent
    ## (target_geoid = 5-digit county FIPS). tigris::tracts() with cb = TRUE
    ## and no state arg returns territorial tracts alongside the 50 states.
    territorial_geographies = tigris::tracts(year = source_year, cb = TRUE) %>%
      janitor::clean_names() %>%
      filter(statefp %in% territory_state_fips) %>%
      sf::st_drop_geometry() %>%
      as_tibble() %>%
      mutate(target_geoid = str_sub(geoid, 1, 5)) %>%
      tidylog::left_join(
        territory_county_lookup,
        by = c("target_geoid" = "county_geoid"),
        relationship = "many-to-one") %>%
      transmute(
        state_fips = statefp,
        source_geography_name = namelsad,
        target_geography_name = county_name,
        source_geoid = geoid,
        target_geoid)
  } else if (source_geography == "zcta") {
    ## Territorial ZCTAs are assigned to a single county-equivalent by
    ## converting each ZCTA to a point on its surface and joining to the
    ## territorial county-equivalents. This keeps the allocation factor at 1,
    ## consistent with the rest of the territorial rows.
    territory_counties_for_join = territory_counties_sf %>%
      sf::st_transform(CRS_PROJ) %>%
      transmute(target_geoid = geoid, target_geography_name = namelsad)

    territorial_zctas_points = sf::st_filter(
      tigris::zctas(year = source_year) %>%
        janitor::clean_names() %>%
        sf::st_transform(CRS_PROJ) %>%
        sf::st_point_on_surface(),
      territory_counties_for_join)

    territorial_geographies = territorial_zctas_points %>%
      sf::st_join(territory_counties_for_join) %>%
      sf::st_drop_geometry() %>%
      as_tibble() %>%
      transmute(
        state_fips = str_sub(target_geoid, 1, 2),
        source_geography_name = NA_character_,
        target_geography_name,
        source_geoid = geoid20,
        target_geoid)
  } else if (source_geography == "county") {
    territorial_geographies = territory_counties_sf %>%
      sf::st_drop_geometry() %>%
      as_tibble() %>%
      transmute(
        state_fips = statefp,
        source_geography_name = namelsad,
        target_geography_name = namelsad,
        source_geoid = geoid,
        target_geoid = geoid)
  } else {
    stop("source_geography must be one of \"tract\", \"zcta\", \"county\".")
  }

  territories_crosswalk1 = territorial_geographies %>%
    transmute(
      across(everything(), ~ .x),
      allocation_factor_source_to_target = 1,
      allocation_factor_target_to_source = NA_real_,
      source_geography = source_geography,
      target_geography = "aiannh",
      weighting_factor = "identity",
      population_2020 = NA_real_,
      territory_flag = "Territories")

  ## Attach the Indigenous (NHPI alone-or-in-combination) population share to
  ## every territorial county-equivalent, keyed by its county-equivalent FIPS
  ## (`target_geoid`, which equals `source_geoid` when source = "county"). The
  ## share lets downstream dollar/loss figures scale territorial estimates to
  ## the Indigenous population, mirroring figure 1. The county-equivalent total
  ## `population_2020` is additionally attached for source = "county", where the
  ## source unit IS the county-equivalent; for tract / zcta sources it stays NA
  ## (Island Areas tract / ZCTA-level populations are not fetched here).
  territory_pops = get_territory_populations_2020()

  territories_crosswalk = territories_crosswalk1 %>%
    tidylog::left_join(
      territory_pops %>%
        transmute(target_geoid = source_geoid, population_nhpi_aoic, indigenous_share),
      by = "target_geoid",
      relationship = "many-to-one")

  if (source_geography == "county") {
    territories_crosswalk = territories_crosswalk %>%
      select(-population_2020) %>%
      tidylog::left_join(
        territory_pops %>% transmute(source_geoid, population_2020),
        by = "source_geoid",
        relationship = "one-to-one")

    n_missing_pop = sum(is.na(territories_crosswalk$population_2020))
    if (n_missing_pop > 0) {
      warning(
        n_missing_pop, " territorial county-equivalent(s) missing 2020 ",
        "population after join to Island Areas DHC.") }
  }

  ## all columns in our territories crosswalk should be identical to those in our primary tract-native crosswalk
  missing_columns = colnames(tribal_crosswalk)[!colnames(tribal_crosswalk) %in% colnames(territories_crosswalk)]
  stopifnot(length(missing_columns) == 0)

  result = bind_rows(
      tribal_crosswalk %>% mutate(across(matches("allocation_factor|population_2020"), as.numeric)),
      territories_crosswalk %>% mutate(across(matches("allocation_factor|population_2020"), as.numeric))) %>%
    ## Redundant safety: HHL targets are already absent from the tribe
    ## membership, so no Hawaiian rows survive the join above.
    filter(!str_detect(target_geography_name, "Hawaii"))

  ## Add `home_state` (2-letter state code) per target_geoid: the state
  ## holding the largest allocation-weighted share of the target's source
  ## rows. Territorial state_fips (60/66/69) map cleanly to AS/GU/MP via
  ## tidycensus::fips_codes. Feature-2 chunks use this to break out
  ## Alaskan from contiguous-US tribes for community-count QC.
  state_fips_to_abbr = tidycensus::fips_codes %>%
    distinct(state_fips = state_code, home_state = state)

  home_state_lookup = result %>%
    tidylog::left_join(state_fips_to_abbr, by = "state_fips", relationship = "many-to-one") %>%
    summarize(
      .by = c(target_geoid, home_state),
      weight = sum(allocation_factor_source_to_target, na.rm = TRUE)) %>%
    slice_max(weight, by = target_geoid, n = 1, with_ties = FALSE) %>%
    select(target_geoid, home_state)

  result1 = result %>%
    tidylog::left_join(home_state_lookup, by = "target_geoid", relationship = "many-to-one")

  return(result1)
}