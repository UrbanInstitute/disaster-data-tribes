## ---------------------------------------------------------------------------
## utilities.R — shared name-matching and validation primitives
##
## Small, dependency-light helpers used across feature_1, feature_2, and the
## background scripts. Sourced wherever name normalization or flag coercion is
## needed; defines functions only:
##   adjust_dollars_to_2025()        — convert dollar columns to 2025 USD
##                                     (may refresh the shared PCE index file
##                                     on Box from the FRED API; see its docs)
##   any_flag_true()                 — coerce mixed-type FEMA boolean columns
##   bidirectional_substring_match() — substring match in either direction
##   name_key_coverage()             — token-coverage score between two name keys
##   normalize_name_key()            — canonical key for tribe/place name matching
##   warn_if_multivalued()           — warn when a group maps to >1 value of a field
## ---------------------------------------------------------------------------
library(tidyverse)

## Coordinate reference systems used across the project's spatial steps.
## CRS_GEO  — EPSG:4269 (NAD83 geographic, lon/lat): the storage/return CRS for
##   community and county geometries (matches tigris defaults).
## CRS_PROJ — EPSG:6933 (WGS 84 / NSIDC EASE-Grid 2.0 Global, cylindrical
##   equal-area): used only for area and intersection-area computations, where
##   an equal-area projection matters. Global rather than CONUS Albers
##   (EPSG:5070) because the universe includes Alaska, Hawaii, and the Pacific
##   territories, where CONUS Albers areas are badly distorted.
CRS_GEO <- 4269
CRS_PROJ <- 6933

#' Convert dollar columns to 2025 USD using the annual PCE price index
#'
#' Wraps `climateapi::inflation_adjust()` with `base_year = 2025`, adding the
#' two guards that function needs to work for this project:
#'
#' \enumerate{
#'   \item `inflation_adjust()` reads its price index from one fixed CSV on
#'     Box (`utilities/pce_index_annual_fred_2025_03_27.csv`). When that file
#'     has no 2025 row, this function downloads the full annual series from
#'     the FRED API (series `DPCERG3A086NBEA`, the PCE price index; no API
#'     key required) and rewrites the file in place — same columns, same
#'     path — so `inflation_adjust()` can find the 2025 base year. The
#'     refresh runs at most once (afterward the file contains 2025).
#'   \item Record years outside the index range are clamped before joining.
#'     Years after 2025 (partial-year 2026 records) are treated as already
#'     being in 2025 dollars, and records with a missing year pass through
#'     unchanged; without this, both would turn into `NA` dollars.
#' }
#'
#' Adjusted values REPLACE the input columns — no suffix is added — so
#' downstream code and exported CSVs keep their existing column names.
#'
#' @param df A data frame.
#' @param year_variable Character scalar. Name of the column holding each
#'   record's nominal (as-reported) dollar year. Character or numeric.
#' @param dollar_variables Character vector of dollar column names to adjust.
#'
#' @return `df` with `dollar_variables` re-expressed in 2025 USD.
adjust_dollars_to_2025 <- function(df, year_variable, dollar_variables) {
  base_year <- 2025

  pce_path <- file.path(
    climateapi::get_box_path(), "utilities",
    "pce_index_annual_fred_2025_03_27.csv")
  pce_years <- readr::read_csv(pce_path, show_col_types = FALSE) %>%
    dplyr::pull(observation_date) %>%
    lubridate::year()

  if (!base_year %in% pce_years) {
    message(
      "adjust_dollars_to_2025: the PCE index file on Box has no ", base_year,
      " row; refreshing it from the FRED API (series DPCERG3A086NBEA).")
    fred_pce <- readr::read_csv(
      "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DPCERG3A086NBEA",
      show_col_types = FALSE) %>%
      dplyr::rename(observation_date = 1, DPCERG3A086NBEA = 2)
    if (!base_year %in% lubridate::year(fred_pce$observation_date)) {
      stop(
        "adjust_dollars_to_2025: FRED did not return a ", base_year,
        " annual PCE value; cannot adjust dollars to ", base_year, " USD.",
        call. = FALSE)
    }
    readr::write_csv(fred_pce, pce_path)
    pce_years <- lubridate::year(fred_pce$observation_date)
  }

  df %>%
    dplyr::mutate(
      ## Years after the base year (e.g. 2026 records) are clamped down to
      ## the base year: FRED's annual PCE index has no value for a year
      ## still in progress, so those records' dollars are carried at nominal
      ## value — overstating them by the base-year-to-date inflation
      ## (project decision, July 2026: accepted as-is rather than deflating
      ## from the monthly index).
      inflation_year_clamped_ = pmin(
        pmax(as.numeric(.data[[year_variable]]), min(pce_years)),
        base_year),
      ## A record with no usable year cannot be deflated; treat it as
      ## already being in base-year dollars (factor 1) rather than letting
      ## the join turn its dollar values into NA.
      inflation_year_clamped_ = dplyr::coalesce(
        inflation_year_clamped_, as.numeric(base_year))) %>%
    climateapi::inflation_adjust(
      year_variable = "inflation_year_clamped_",
      dollar_variables = dollar_variables,
      names_suffix = "",
      base_year = base_year) %>%
    dplyr::select(-inflation_year_clamped_)
}

#' Test whether a mixed-type flag vector contains any truthy value
#'
#' FEMA API responses surface boolean-like columns (e.g.,
#' `ia_program_declared`) with inconsistent types — some rows parse as logical
#' `TRUE`/`FALSE`, others as character `"true"`/`"false"` or `"1"`/`"0"`, and
#' occasionally as numeric `1`/`0`. This unifies truthy values. A value that
#' still fails logical coercion is an error: flags silently dropping to
#' not-true would corrupt eligibility downstream.
#'
#' @param x Vector of any type. Existing `NA` values are treated as not-true.
#'
#' @return Logical scalar. `TRUE` iff any element of `x` coerces to `TRUE`.
any_flag_true <- function(x) {
  if (length(x) == 0) return(FALSE)
  coerced <- if (is.logical(x)) {
    x
  } else if (is.numeric(x)) {
    as.logical(x)
  } else {
    ## as.logical() parses "TRUE"/"true"/"T" etc. but NOT "1"/"0" — map those
    ## explicitly so a stringified numeric flag can't silently drop to NA.
    chr <- str_trim(as.character(x))
    suppressWarnings(
      case_when(
        chr %in% c("1") ~ TRUE,
        chr %in% c("0") ~ FALSE,
        .default = as.logical(chr)))
  }
  failed <- is.na(coerced) & !is.na(x)
  if (any(failed)) {
    stop(
      "any_flag_true: ", sum(failed), " value(s) failed logical coercion: ",
      str_c(unique(x[failed]), collapse = ", "),
      call. = FALSE)
  }
  isTRUE(any(coerced, na.rm = TRUE))
}

#' Bidirectional substring match between a query key and candidate keys
#'
#' Shared matching primitive for name-to-name linking across FEMA, BIA, and
#' tigris sources. Returns `TRUE` at each candidate position where the query
#' key is a substring of the candidate OR the candidate is a substring of the
#' query. Both directions are needed because the normalized name-key strings
#' are not lexically comparable across sources — BIA often has the longer
#' canonical name (e.g., "AGUACALIENTECAHUILLA"), while tigris may carry a
#' shorter reservation label ("AGUACALIENTE"), and FEMA `designated_area`
#' strings can be either.
#'
#' @param query_key Character scalar. Normalized name key to match.
#' @param candidate_keys Character vector. Normalized name keys to compare
#'   against.
#'
#' @return Logical vector the same length as `candidate_keys`. `TRUE` at each
#'   position where the two keys match bidirectionally.
bidirectional_substring_match <- function(query_key, candidate_keys) {
  str_detect(candidate_keys, fixed(query_key)) |
    str_detect(rep(query_key, length(candidate_keys)), fixed(candidate_keys))
}

#' Structural coverage score for two normalized name keys
#'
#' Given two normalized name keys (produced by `normalize_name_key()`) where
#' one is a substring of the other (per `bidirectional_substring_match()`),
#' returns `min(nchar) / max(nchar)` — a value in `(0, 1]` capturing how much
#' of the longer key the shorter one covers. `1.0` when the keys are equal
#' length (and therefore identical, given the substring precondition). Short keys
#' that match inside much longer keys (e.g., `"UTE"` inside `"PAIUTE"`)
#' score low, flagging probable false positives for review.
#'
#' @param a,b Character vectors of normalized name keys.
#'
#' @return Numeric vector the length of `max(length(a), length(b))`.
#'
#' @examples
#' name_key_coverage("NAVAJO", "NAVAJO")          # 1.0
#' name_key_coverage("UTE", "PAIUTE")             # 0.5
#' name_key_coverage("NAVAJO", "NAVAJOMOUNTAIN")  # ~0.43
name_key_coverage <- function(a, b) {
  lens_a <- nchar(a)
  lens_b <- nchar(b)
  out <- pmin(lens_a, lens_b) / pmax(lens_a, lens_b)
  out[is.na(a) | is.na(b) | lens_a == 0 | lens_b == 0] <- NA_real_
  out
}

#' Normalize tribal / community names into a comparable key
#'
#' Produces a case-insensitive, punctuation-free, stopword-stripped
#' concatenation of tokens suitable for substring matching between FEMA
#' `designated_area` strings, BIA tribe names, and tigris polygon NAME values.
#'
#' Normalization steps, in order:
#' \enumerate{
#'   \item Uppercase (case-insensitive).
#'   \item `&` → `" AND "` so the conjunction becomes a droppable token.
#'   \item All non-alphanumeric characters (punctuation, apostrophes,
#'     parentheses, hyphens, periods) replaced with spaces. This strips
#'     parenthetical FEMA suffixes like `(TN)` / `(TG)` / `(R)` by turning
#'     their surrounding punctuation into whitespace.
#'   \item Whitespace squished.
#'   \item Tokenized on space; tokens appearing in the stopword list dropped.
#'   \item Remaining tokens concatenated (no separator) into one string.
#' }
#'
#' The stopword list targets generic tribal-boilerplate tokens that carry no
#' distinguishing information across entities: governmental form
#' (`TRIBE`/`NATION`/`BAND`/`COMMUNITY`), geographic form
#' (`RESERVATION`/`RANCHERIA`/`COLONY`/`PUEBLO`/`TOWN`/`VILLAGE`), generic
#' descriptors (`INDIAN`/`INDIANS`/`TRIBAL`/`NATIVE`/`PEOPLE`/`PEOPLES`),
#' and connectives (`OF`/`THE`/`AND`/`OR`). `COUNCIL` is intentionally NOT a
#' stopword — several Alaska tribes (e.g., Curyung Tribal Council, Native
#' Village of Council) would collapse to an empty key otherwise.
#'
#' @param x Character vector. Free-text names to normalize. `NA` input
#'   passes through as `NA`.
#'
#' @return Character vector of the same length as `x`. Each element is the
#'   stopword-stripped concatenation, or `NA` if the input was `NA`.
#'
#' @examples
#' normalize_name_key("Navajo Nation")
#' #> [1] "NAVAJO"
#' normalize_name_key("Agua Caliente Band of Cahuilla Indians (TN)")
#' #> [1] "AGUACALIENTECAHUILLA"
#' normalize_name_key("Native Village of Afognak")
#' #> [1] "AFOGNAK"
normalize_name_key <- function(x) {
  stopwords <- c(
    "TRIBE", "TRIBES", "NATION", "BAND", "BANDS", "COMMUNITY", "COMMUNITIES",
    "INDIAN", "INDIANS", "TRIBAL", "NATIVE",
    "RESERVATION", "RANCHERIA", "COLONY", "PUEBLO", "TOWN", "VILLAGE",
    "PEOPLES", "PEOPLE",
    "OF", "THE", "AND", "OR")

  x %>%
    str_to_upper() %>%
    str_replace_all("&", " AND ") %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>%
    str_squish() %>%
    str_split(" ") %>%
    purrr::map(~ discard(.x, ~ .x %in% stopwords)) %>%
    purrr::map_chr(~ str_c(.x, collapse = ""))
}

#' Warn when a column is not single-valued within groups that expect it to be
#'
#' For `summarize(.by = ...)` contexts where a non-key column should be
#' invariant within each group — e.g., `declaration_title` should be the same
#' across every row in one `(county_fips, disaster_number)` group — `first()`
#' silently drops variation when it occurs. This helper runs the variance
#' check once across all groups and emits a single warning (instead of one
#' warning per group), so the caller sees the issue without losing the
#' downstream aggregation.
#'
#' @param data A data frame.
#' @param group_cols Character vector of column names that define the
#'   summarize groups.
#' @param field Character scalar. Column expected to be single-valued within
#'   each group.
#' @param context Character scalar or `NULL`. Label prepended to the warning
#'   (typically the calling function name).
#'
#' @return `data` invisibly, unchanged.
warn_if_multivalued <- function(data, group_cols, field, context = NULL) {
  violations <- data %>%
    summarize(
      .by = all_of(group_cols),
      .n_distinct = n_distinct(.data[[field]], na.rm = TRUE)) %>%
    filter(.n_distinct > 1)

  if (nrow(violations) > 0) {
    warning(
      if (!is.null(context)) str_c(context, ": ") else "",
      nrow(violations), " group(s) have multiple distinct values of '", field,
      "' — first() aggregation will drop information. Grouping columns: ",
      str_c(group_cols, collapse = ", "),
      call. = FALSE)
  }
  invisible(data)
}
