## ---------------------------------------------------------------------------
## figure_2_distribution_declarations.R — data prep for feature-2 figure 2
##
## prepare_figure_2_data(): per-community counts of major disaster declarations
## (via named tribal-direct, areal county overlap, and statewide-when-contained
## paths). Sourced/called by _feature2.qmd, where the distribution is plotted.
## ---------------------------------------------------------------------------
library(arrow)
library(tidyverse)
library(sf)
library(janitor)
library(here)

source(here::here("scripts", "utilities", "utilities.R"))

#' Prepare per-community major disaster declaration counts (figure 2)
#'
#' Declaration counts reflect four mechanisms:
#'
#' \enumerate{
#'   \item **Named (tribal-direct)** — bidirectional substring match on
#'     normalized name keys between community name/aliases and FEMA
#'     `designated_area` for tribal-direct rows (those with
#'     `str_sub(county_fips, 3, 5) == "000"`). Substring matches are
#'     additionally filtered by `name_match_coverage_threshold`
#'     (`min(nchar) / max(nchar)`), which suppresses false positives where
#'     a short community key (e.g., `"EEK"`, `"EASTERN"`) accidentally
#'     appears inside a longer unrelated key (e.g., `"CREEK"`,
#'     `"EASTERNCHEROKEE"`).
#'   \item **Areal (county-level)** — for county-level declarations, a
#'     disaster counts for a community if the fraction of the community's
#'     area lying within the declared counties is at least
#'     `coverage_threshold` (default `0.5`).
#'   \item **Territory-direct** — AS/GU/MP territory community polygons in
#'     `universe_sf` extend well beyond the corresponding `tigris`
#'     county-equivalent polygon (they include territorial waters and
#'     extended bounds), so the areal path under-attributes them severely
#'     even at full county containment. For these communities the
#'     community_id encodes the 5-digit county fips
#'     (`territory_GU_66010`); county-level declarations are attributed
#'     directly via that membership instead of by areal overlap. Affects
#'     only `community_id` values matching `^territory_[A-Z]{2}_\\d{5}$`.
#'   \item **Statewide** — declarations with `designated_area` matching
#'     "statewide" case-insensitively. Note: FEMA's
#'     `DisasterDeclarationsSummaries` almost never uses this designation
#'     in practice — declarations are enumerated county-by-county even for
#'     territory-wide events. In a typical 10-year window this branch
#'     catches a handful of declarations at most (often only DR-4669-AS).
#'     A statewide declaration counts for a community when the community's
#'     `state_list` contains only the declared state. Multi-state tribes
#'     (Navajo, Cherokee, etc.) are intentionally not matched here; they
#'     rely on the named and areal paths.
#' }
#'
#' Time window is filtered on `year(incident_begin_date)`.
#'
#' @param universe_sf An `sf` tibble of Indigenous communities — the output
#'   of feature 1's `get_indigenous_universe()`. Must carry `state_list`.
#' @param counties_sf An `sf` of US counties, including AS/GU/MP
#'   county-equivalents (the default `tigris::counties()` call returns
#'   them).
#' @param year_range Integer vector. Calendar years included, filtered on
#'   `year(incident_begin_date)`. Default `2016:2025`.
#' @param coverage_threshold Numeric in `(0, 1]`. Minimum fraction of a
#'   community's area that must fall within declared counties for a
#'   county-level declaration to count for that community. Default `0.5`.
#' @param name_match_coverage_threshold Numeric in `(0, 1]`. Minimum
#'   `min(nchar) / max(nchar)` ratio between a community name key and a
#'   FEMA `designated_area` key for a substring match to be accepted in
#'   the named path. Default `0.5`.
#' @param declarations Optional tibble.
#'   When `NULL` (default), pulled from API.
#'
#' @return A long tibble with one row per unit × disaster declaration. The
#'   same shape is used for two `unit_type` values: \code{"community"}
#'   rows are Indigenous communities; \code{"county"} rows are US
#'   county-equivalents (excluding PR and VI) and are intended as a
#'   comparison baseline. Units with zero declarations in the time window
#'   are omitted. Columns:
#'   \describe{
#'     \item{unit_type}{Character. Either \code{"community"} or \code{"county"}.}
#'     \item{unit_id}{Character. `community_id` for communities;
#'       `county_fips` (5-digit GEOID) for counties.}
#'     \item{name}{Character. Canonical community name or county name.}
#'     \item{disaster_number}{Integer. FEMA disaster declaration number.}
#'     \item{incident_begin_date}{Date. Incident begin date.}
#'     \item{incident_type}{Character. Normalized incident type.}
#'     \item{year}{Integer. `year(incident_begin_date)`.}
#'     \item{route}{Character. Attribution path(s). For communities:
#'       \code{"named"}, \code{"areal"}, \code{"territory_direct"},
#'       \code{"statewide"}, or concatenated with \code{"+"} when multiple
#'       paths apply. For counties: \code{"county_level"},
#'       \code{"statewide"}, or \code{"county_level+statewide"}.}
#'   }
prepare_figure_2_data <- function(
  universe_sf,
  counties_sf,
  year_range = 2016:2025,
  coverage_threshold = 0.5,
  name_match_coverage_threshold = 0.5,
  declarations = NULL) {

  natural_hazards <- c(
    "Fire", "Flood", "Hurricane", "Severe Storm", "Winter Storm", "Tornado",
    "Snowstorm", "Earthquake", "Mud/Landslide", "Coastal Storm",
    "Severe Ice Storm", "Tropical Storm", "Typhoon", "Volcanic Eruption",
    "Tsunami", "Freezing", "Drought", "Tropical Depression",
    "Straight-Line Winds")

  ## -- 1. Pull / accept DR declarations ---------------------------------------
  if (is.null(declarations)) {
    declarations <- rfema::open_fema(
      data_set = "DisasterDeclarationsSummaries",
      filters = list(declarationType = "=DR"),
      ask_before_call = FALSE) %>%
      janitor::clean_names() %>%
      as_tibble()
  } else {
    declarations <- declarations %>%
      janitor::clean_names() %>%
      as_tibble()
  }

  ## -- 2. Normalize incident_type, filter window, classify route --------------
  ## Route assignment is literal: "statewide" when FEMA tags the area as
  ## statewide, "tribal_direct" when the county-fips suffix is "000", else
  ## "county_level". Territorial declarations (state AS/GU/MP) fall into
  ## these routes on their own merits — a district-specific declaration
  ## routes as county_level and matches only that district's polygon; a
  ## territory-wide "statewide" declaration matches every county-equivalent
  ## in the territory via state_list.
  declarations1 <- declarations %>%
    mutate(
      incident_type = case_when(
        str_detect(declaration_title, "STRAIGHT-LINE WINDS") ~ "Straight-Line Winds",
        str_detect(declaration_title, "WIND STORM") ~ "Straight-Line Winds",
        str_detect(declaration_title, "SEVERE WEATHER CONDITIONS") ~ "Severe Storm",
        TRUE ~ incident_type),
      incident_begin_date = as.Date(incident_begin_date),
      incident_year = year(incident_begin_date),
      county_fips = str_c(fips_state_code, fips_county_code),
      declaration_route = case_when(
        str_detect(designated_area, regex("statewide", ignore_case = TRUE)) ~ "statewide",
        str_sub(county_fips, 3, 5) == "000" ~ "tribal_direct",
        TRUE ~ "county_level")) %>%
    filter(
      incident_year %in% year_range,
      incident_type %in% natural_hazards,
      declaration_type == "DR")

  tribal_direct <- declarations1 %>%
    filter(declaration_route == "tribal_direct") %>%
    distinct(disaster_number, designated_area, state)

  county_level <- declarations1 %>%
    filter(declaration_route == "county_level") %>%
    distinct(disaster_number, county_fips, state)

  statewide <- declarations1 %>%
    filter(declaration_route == "statewide") %>%
    distinct(disaster_number, state)

  ## -- 3. Named matches (bidirectional substring) -----------------------------
  universe1 <- universe_sf %>%
    sf::st_as_sf() %>%
    mutate(row_id = row_number())

  universe_keys <- universe1 %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    select(row_id, community_id, name, aliases) %>%
    mutate(
      name_keys = purrr::map2(
        aliases, name,
        ~ unique(c(.y, unlist(.x))) %>%
          discard(is.na) %>%
          normalize_name_key() %>%
          keep(~ nchar(.x) >= 3)))

  tribal_keys <- tribal_direct %>%
    mutate(designated_area_key = normalize_name_key(designated_area)) %>%
    filter(!is.na(designated_area_key), nchar(designated_area_key) >= 3)

  named_disasters_for <- function(keys) {
    if (length(keys) == 0 || nrow(tribal_keys) == 0) return(integer(0))
    hits <- purrr::map_lgl(
      tribal_keys$designated_area_key,
      function(da_key) {
        substr_hits <- bidirectional_substring_match(da_key, keys)
        if (!any(substr_hits)) return(FALSE)
        coverages <- name_key_coverage(da_key, keys[substr_hits])
        any(coverages >= name_match_coverage_threshold, na.rm = TRUE)
      })
    unique(tribal_keys$disaster_number[hits]) }

  named <- universe_keys %>%
    mutate(named_disasters = purrr::map(name_keys, named_disasters_for)) %>%
    select(row_id, community_id, named_disasters)

  ## -- 4. Areal matches (>= coverage_threshold of community area) -------------
  ## Pre-filter counties to just those implicated by a county-level declaration
  ## in-window; keeps st_intersection tractable.
  declared_county_fips <- unique(county_level$county_fips)

  counties1 <- counties_sf %>%
    janitor::clean_names() %>%
    sf::st_transform(CRS_PROJ) %>%
    sf::st_make_valid() %>%
    transmute(county_fips = geoid) %>%
    filter(county_fips %in% declared_county_fips)

  universe_proj <- universe1 %>%
    sf::st_transform(CRS_PROJ) %>%
    sf::st_make_valid() %>%
    mutate(community_area = as.numeric(sf::st_area(geometry))) %>%
    filter(community_area > 0)

  areal <- if (nrow(counties1) == 0 || nrow(universe_proj) == 0) {
    tibble(row_id = integer(0), areal_disasters = list())
  } else {
    overlaps <- sf::st_intersection(universe_proj, counties1) %>%
      mutate(overlap_area = as.numeric(sf::st_area(geometry))) %>%
      sf::st_drop_geometry() %>%
      as_tibble() %>%
      mutate(fraction = overlap_area / community_area) %>%
      select(row_id, community_id, county_fips, fraction)

    overlaps %>%
      inner_join(county_level, by = "county_fips", relationship = "many-to-many") %>%
      summarize(
        .by = c(row_id, disaster_number),
        total_fraction = sum(fraction)) %>%
      filter(total_fraction >= coverage_threshold) %>%
      summarize(
        .by = row_id,
        areal_disasters = list(unique(disaster_number)))
  }

  ## -- 4b. Territory-direct matches (AS/GU/MP county-equivalents) ------------
  ## Territory community polygons in universe_sf extend beyond their county-
  ## equivalent and fail the areal coverage threshold even at full county
  ## containment. The community_id encodes the 5-digit county fips
  ## (e.g., territory_GU_66010), so attribute county-level declarations by
  ## direct fips membership for these communities.
  territory_pattern <- "^territory_[A-Z]{2}_\\d{5}$"

  territory_fips <- universe1 %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    filter(str_detect(community_id, territory_pattern)) %>%
    transmute(
      row_id,
      county_fips = str_extract(community_id, "\\d{5}$"))

  territory_direct <- if (nrow(territory_fips) == 0) {
    tibble(row_id = integer(0), territory_direct_disasters = list())
  } else {
    territory_fips %>%
      inner_join(county_level, by = "county_fips", relationship = "many-to-many") %>%
      summarize(
        .by = row_id,
        territory_direct_disasters = list(unique(disaster_number)))
  }

  ## -- 4c. Statewide matches (community solely within declared state) --------
  ## Match a statewide declaration to a community iff the community's
  ## state_list is exactly {declared_state}. This picks up every tribe whose
  ## territory lies entirely within one state, plus the AS/GU/MP territory
  ## communities (whose state_list is a single-element list of the territory
  ## abbreviation). Multi-state tribes are intentionally not matched here
  ## — they rely on named / areal routes.
  statewide_by_state <- statewide %>%
    summarize(.by = state, disasters = list(unique(disaster_number)))

  state_lookup <- set_names(
    statewide_by_state$disasters,
    statewide_by_state$state)

  statewide_matches <- universe1 %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    select(row_id, state_list) %>%
    mutate(
      statewide_disasters = purrr::map(state_list, function(sl) {
        states_uniq <- unique(unlist(sl))
        if (length(states_uniq) != 1) return(integer(0))
        hits <- state_lookup[[states_uniq]]
        if (is.null(hits)) integer(0) else hits
      })) %>%
    select(row_id, statewide_disasters)

  ## -- 5. Combine into long format (one row per community × disaster) ---------
  combined_wide <- universe1 %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    select(row_id, community_id, name) %>%
    left_join(
      named %>% select(row_id, named_disasters),
      by = "row_id", relationship = "one-to-one") %>%
    left_join(areal, by = "row_id", relationship = "one-to-one") %>%
    left_join(territory_direct, by = "row_id", relationship = "one-to-one") %>%
    left_join(statewide_matches, by = "row_id", relationship = "one-to-one") %>%
    mutate(
      named_disasters            = purrr::map(named_disasters,            ~ if (is.null(.x)) integer(0) else .x),
      areal_disasters            = purrr::map(areal_disasters,            ~ if (is.null(.x)) integer(0) else .x),
      territory_direct_disasters = purrr::map(territory_direct_disasters, ~ if (is.null(.x)) integer(0) else .x),
      statewide_disasters        = purrr::map(statewide_disasters,        ~ if (is.null(.x)) integer(0) else .x),
      all_disasters = purrr::pmap(
        list(named_disasters, areal_disasters, territory_direct_disasters, statewide_disasters),
        ~ unique(c(..1, ..2, ..3, ..4))))

  ## One row per disaster per community; disaster_number is unique within
  ## declarations1, so incident_begin_date and incident_type are one-to-one.
  declarations_dates <- declarations1 %>%
    distinct(disaster_number, incident_begin_date, incident_type) %>%
    slice(1, .by = disaster_number)

  community_long <- combined_wide %>%
    select(community_id, name, named_disasters, areal_disasters,
           territory_direct_disasters, statewide_disasters, all_disasters) %>%
    tidyr::unnest(cols = all_disasters) %>%
    rename(disaster_number = all_disasters) %>%
    mutate(
      route = purrr::pmap_chr(
        list(disaster_number, named_disasters, areal_disasters,
             territory_direct_disasters, statewide_disasters),
        function(dn, nd, ad, td, sd) {
          routes <- c(
            if (dn %in% nd) "named"            else NULL,
            if (dn %in% ad) "areal"            else NULL,
            if (dn %in% td) "territory_direct" else NULL,
            if (dn %in% sd) "statewide"        else NULL)
          if (length(routes) == 0) "unknown" else paste(routes, collapse = "+")
        })) %>%
    select(community_id, name, disaster_number, route) %>%
    left_join(declarations_dates, by = "disaster_number", relationship = "many-to-one") %>%
    transmute(
      unit_type = "community",
      unit_id = community_id,
      name,
      disaster_number,
      incident_begin_date,
      incident_type,
      year = lubridate::year(incident_begin_date),
      route)

  ## -- 6. Comparable county-level attribution -------------------------------
  ## Build a county-equivalent baseline using the same time window, hazard
  ## filter, and declaration_type as the community attribution above.
  ## Tribal-direct declarations don't apply to counties and are excluded.
  ## PR (FIPS 72) and VI (FIPS 78) are excluded per project policy; AS, GU,
  ## and MP county-equivalents are retained.
  counties_clean <- counties_sf %>%
    janitor::clean_names() %>%
    sf::st_drop_geometry() %>%
    as_tibble()

  county_name_col <- if ("namelsad" %in% names(counties_clean)) "namelsad" else "name"

  counties_universe <- counties_clean %>%
    transmute(
      county_fips = geoid,
      county_name = .data[[county_name_col]],
      state = stusps) %>%
    filter(!state %in% c("PR", "VI"))

  county_cl_attrib <- county_level %>%
    filter(county_fips %in% counties_universe$county_fips) %>%
    distinct(disaster_number, county_fips) %>%
    mutate(route_cl = "county_level")

  county_sw_attrib <- statewide %>%
    inner_join(
      counties_universe %>% select(county_fips, state),
      by = "state",
      relationship = "many-to-many") %>%
    distinct(disaster_number, county_fips) %>%
    mutate(route_sw = "statewide")

  county_long <- county_cl_attrib %>%
    full_join(
      county_sw_attrib,
      by = c("disaster_number", "county_fips"),
      relationship = "one-to-one") %>%
    mutate(
      route = case_when(
        !is.na(route_cl) & !is.na(route_sw) ~ "county_level+statewide",
        !is.na(route_cl) ~ "county_level",
        !is.na(route_sw) ~ "statewide")) %>%
    select(disaster_number, county_fips, route) %>%
    inner_join(
      counties_universe %>% select(county_fips, county_name),
      by = "county_fips",
      relationship = "many-to-one") %>%
    inner_join(declarations_dates, by = "disaster_number", relationship = "many-to-one") %>%
    transmute(
      unit_type = "county",
      unit_id = county_fips,
      name = county_name,
      disaster_number,
      incident_begin_date,
      incident_type,
      year = lubridate::year(incident_begin_date),
      route)

  combined <- bind_rows(community_long, county_long)

  n_communities <- n_distinct(community_long$unit_id)
  median_decl   <- median(community_long %>% count(unit_id) %>% pull(n))
  max_decl      <- max(community_long %>% count(unit_id) %>% pull(n))
  n_counties_impacted <- n_distinct(county_long$unit_id)
  n_counties_total <- nrow(counties_universe)

  message(
    "Figure 2 data prepared: ", n_communities, " communities with ≥1 declaration; ",
    nrow(community_long), " community-declaration rows; ",
    "median declarations per community = ", median_decl,
    ", max = ", max_decl, ". ",
    n_counties_impacted, " of ", n_counties_total,
    " counties (excl. PR/VI) with ≥1 declaration.")

  combined
}

#' @title Plot figure 2 — distribution of disasters per Indigenous community
#'
#' @description Histogram of disasters-per-community, with optional dashed
#' vertical reference lines showing the mean and median for a comparison
#' universe (e.g., all US counties). Bin width is set via the
#' Freedman–Diaconis rule (`2 * IQR / n^(1/3)`) floored to at least `1`
#' (the data are integer counts).
#'
#' @param community_counts Tibble with one row per community and an
#'   `n_disasters` column: the integer count of distinct disasters in the
#'   time window, where declarations sharing the same incident begin date
#'   and incident type are collapsed to one disaster (FEMA can issue
#'   several DR numbers for one event — a state DR plus tribal DRs).
#'   Typically built in the .qmd from the output of
#'   `prepare_figure_2_data()`.
#' @param county_mean,community_mean Optional numeric scalars to draw as
#'   vertical reference lines on the histogram. Pass `NULL` (default) to
#'   omit a given line.
#'
#' @return A `ggplot` object. Not written to disk.
plot_figure_2_distribution_declarations <- function(
  community_counts,
  county_mean = NULL,
  community_mean = NULL) {

  counts <- community_counts$n_disasters
  iqr_val <- stats::IQR(counts, na.rm = TRUE)
  n_obs <- sum(!is.na(counts))
  fd_width <- if (iqr_val > 0 && n_obs > 0) 2 * iqr_val / n_obs^(1 / 3) else 1
  bin_width <- max(1L, as.integer(ceiling(fd_width)))

  x_max <- max(counts, na.rm = TRUE)
  x_breaks <- scales::breaks_extended(n = 7, only.loose = FALSE)(c(0, x_max)) %>%
    keep(~ .x == as.integer(.x))

  p1 <- community_counts %>%
    ggplot(aes(x = n_disasters)) +
      geom_histogram(
        binwidth = bin_width,
        fill = palette_urbn_main[1]) +
      scale_x_continuous(
        breaks = x_breaks,
        labels = scales::label_number(accuracy = 1)) +
      labs(
        x = "Number of major disasters",
        y = "Number of Indigenous communities") +
      urbnthemes::theme_urbn_print()

  reference_lines <- tibble::tibble(
    statistic = c("Average, all US counties", "Average, Indigenous communities"),
    value = c(county_mean %||% NA_real_, community_mean %||% NA_real_)) %>%
    dplyr::filter(!is.na(value))

  if (nrow(reference_lines) > 0) {
    p1 <- p1 +
      geom_vline(
        data = reference_lines,
        aes(xintercept = value, linetype = statistic),
        color = "grey30") +
      scale_linetype_manual(
        name = NULL,
        values = c(
          "Average, all US counties" = "dashed",
          "Average, Indigenous communities" = "dotted"))
  }

  p1
}
