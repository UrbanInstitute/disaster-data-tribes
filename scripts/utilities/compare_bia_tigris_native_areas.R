## ---------------------------------------------------------------------------
## compare_bia_tigris_native_areas.R — first-pass BIA <-> tigris name matcher
##
## Defines compare_bia_to_tigris_native_areas(): links each BIA federally
## recognized tribe to its best-matching tigris::native_areas() polygon by
## normalized-token Jaccard. Deliberately simple — its unmatched residuals and
## wrong matches are resolved downstream by the crosswalk + corrections in
## build_indigenous_tribe_polygons.R (the entry point that sources this file).
## verify_bia_tigris_native_areas.R is the standalone reconciliation check.
## ---------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(tigris)
library(janitor)
library(here)

source(here::here("scripts", "utilities", "get_bia_federally_recognized_tribes.R"))
source(here::here("scripts", "utilities", "utilities.R"))  # CRS_GEO / CRS_PROJ

options(tigris_use_cache = TRUE)

## Stopwords stripped before name matching: generic governance words and
## geographic-type qualifiers ("Tribe", "Nation", "Reservation", "Rancheria",
## etc.) plus the filler words BIA uses for former/aka annotations
## ("previously", "listed", "as", "see"). State NAMES are deliberately NOT
## removed: several tribes carry a state word as an integral part of their name
## (Delaware Nation, Iowa Tribe) and stripping it would collapse them to an
## empty key or merge tribes that differ only by state (Iowa Tribe of Oklahoma
## vs Iowa Tribe of Kansas and Nebraska).
native_name_stopwords <- c(
  "tribe", "tribes", "tribal", "nation", "nations", "band", "bands",
  "community", "communities", "village", "villages", "native", "rancheria",
  "rancherias", "colony", "colonies", "pueblo", "group", "town", "towns",
  "indians", "indian", "of", "the", "and", "peoples", "people", "confederated",
  "federated", "reservation", "trust", "land", "lands", "off", "at",
  "previously", "listed", "as", "formerly", "aka", "also", "known", "see",
  "members", "affiliated", "tdsa", "otsa", "anvsa")

#' Reduce a tribe/area name to a normalized token set (helper)
#'
#' Lowercases, expands "&" to "and", drops apostrophes, normalizes
#' "Saint"/"St." to "st", strips punctuation, and removes
#' `native_name_stopwords` and single-character tokens. Used for both the BIA
#' and tigris name vectors so the two are normalized identically.
#'
#' @param x Character vector of names.
#' @return A list of character vectors, one token set per input name.
normalize_to_tokens <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("'", "") %>%
    str_replace_all("\\bsaint\\b", "st") %>%
    str_replace_all("\\bst\\.", "st") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish() %>%
    str_split(" ") %>%
    map(~ .x[!.x %in% native_name_stopwords & nchar(.x) > 1])
}

#' Jaccard similarity between two token sets (helper)
#'
#' @param a,b Character vectors of tokens.
#' @return Numeric in `[0, 1]`: `|a ∩ b| / |a ∪ b|`, or `0` if either is empty.
jaccard_similarity <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(0)
  length(intersect(a, b)) / length(union(a, b))
}

#' Reconcile the BIA Federal Register tribe list against tigris::native_areas()
#'
#' Full-outer reconciliation between the 575 federally recognized Tribal
#' entities in `get_bia_federally_recognized_tribes()` and the federal,
#' non-Hawaiian areas in `tigris::native_areas()`. The goal is to confirm the
#' tigris polygon universe — the spatial basis for feature 1 and feature 2 —
#' both (a) covers every BIA-listed tribe and (b) contains no area that does
#' not correspond to a BIA-listed tribe. Every entity on either side gets a
#' row; `in_bia` / `in_tigris` flag which source(s) it appears in.
#'
#' @section tigris entity unit:
#'   `tigris::native_areas()` returns one row per area (Reservation,
#'   Off-Reservation Trust Land, OTSA, ANVSA, Colony, Rancheria, ...), so a
#'   single tribe can span several rows under the same `NAME`. This function
#'   collapses to distinct `NAME` (the tigris "entity"), carrying
#'   `n_tigris_polygons` and the concatenated `namelsad` values so no detail is
#'   lost. The federal/non-Hawaiian filter (`aiannhr == "F"`, `namelsad` not
#'   containing "Hawaii") mirrors `get_indigenous_universe()`.
#'
#' @section Matching (intentionally simple first pass):
#'   BIA and tigris names follow very different conventions (BIA: "Agua
#'   Caliente Band of Cahuilla Indians of the Agua Caliente Indian Reservation,
#'   California"; tigris: "Agua Caliente"). Names are normalized to token sets
#'   (`normalize_to_tokens()`) and each BIA entity is matched to its best
#'   tigris entity by three tiers, highest Jaccard winning within a tier:
#'   \enumerate{
#'     \item `matched_exact`  — identical normalized token sets.
#'     \item `matched_subset` — one token set fully contained in the other.
#'     \item `matched_fuzzy`  — Jaccard >= `fuzzy_threshold`.
#'   }
#'   Unmatched BIA entities are `bia_only`; tigris entities claimed by no BIA
#'   entity are `tigris_only`. This pass uses NO hand-curated crosswalk, so
#'   known name divergences (e.g. Oglala Sioux ↔ "Pine Ridge", shared Oklahoma
#'   OTSAs, Alaska villages governing a differently-named ANVSA) and tribes
#'   with no polygon at all (landless rancherias, newly recognized tribes) will
#'   surface as gaps for manual review — that is the intended output, not an
#'   error. For every unmatched row, `nearest_counterpart` / `match_similarity`
#'   record the closest candidate on the other side to aid that review.
#'
#' @param year Integer. TIGER vintage for `tigris::native_areas()`. Default
#'   `2024`, matching the vintage `get_indigenous_universe()` uses.
#' @param fuzzy_threshold Numeric in `[0, 1]`. Minimum Jaccard for a tier-3
#'   fuzzy match. Default `0.5`.
#' @param tigris_entities Optional prepared tibble of tigris entities to
#'   reconcile against — one row per distinct `tigris_name`, with columns
#'   `n_tigris_polygons`, `tigris_namelsad`, and `home_state`. When `NULL`
#'   (default), entities are built directly from `tigris::native_areas(year)`
#'   (federal, non-Hawaiian, one row per NAME). Pass the
#'   `get_indigenous_universe()`-prepared records (reservation +
#'   off-reservation trust land combined, JUAs distributed, Pit River / Fallon
#'   Paiute-Shoshone consolidated) to reconcile against that universe instead.
#'
#' @return A tibble with one row per entity in either source. Columns:
#'   \describe{
#'     \item{bia_name}{BIA name; `NA` for `tigris_only` rows.}
#'     \item{bia_region}{"Contiguous 48 states" / "Alaska"; `NA` for tigris_only.}
#'     \item{bia_alaska_native}{Logical; `NA` for tigris_only.}
#'     \item{bia_is_cross_reference}{Logical; `TRUE` for the BIA "(See ...)"
#'       cross-reference rows. `NA` for tigris_only.}
#'     \item{tigris_name}{tigris `NAME`; `NA` for `bia_only` rows.}
#'     \item{n_tigris_polygons}{Count of native_areas rows sharing that NAME.}
#'     \item{tigris_namelsad}{"; "-joined distinct `namelsad` values.}
#'     \item{home_state}{Two-letter USPS code of the state holding the
#'       majority or plurality of the tigris entity's land area, summed
#'       across polygons sharing the `NAME` (areas computed in the project equal-area CRS (`CRS_PROJ`, EPSG:6933)).
#'       `NA` for `bia_only` rows, which have no polygon.}
#'     \item{in_bia, in_tigris}{Logical source-membership flags.}
#'     \item{match_status}{`matched_exact`, `matched_subset`, `matched_fuzzy`,
#'       `bia_only`, or `tigris_only`.}
#'     \item{match_similarity}{Jaccard of the matched pair, or — for unmatched
#'       rows — Jaccard to `nearest_counterpart`.}
#'     \item{nearest_counterpart}{For unmatched rows, the closest name on the
#'       other side; `NA` for matched rows (the counterpart is in the row).}
#'   }
compare_bia_to_tigris_native_areas <- function(year = 2024,
                                                fuzzy_threshold = 0.5,
                                                tigris_entities = NULL) {

  ## -- BIA Federal Register list (active = 575 entities) ----------------------
  bia <- get_bia_federally_recognized_tribes() %>%
    transmute(
      bia_name = name,
      bia_region = region,
      bia_alaska_native = alaska_native,
      bia_is_cross_reference = is_cross_reference)

  ## -- tigris records ---------------------------------------------------------
  ## By default, build the entity table straight from tigris::native_areas()
  ## (federal, non-Hawaiian, one row per distinct NAME) with a plurality
  ## home_state. Callers that want the get_indigenous_universe() preparation —
  ## reservation + off-reservation trust land combined, joint-use areas
  ## distributed, Pit River and Fallon Paiute-Shoshone consolidated — pass a
  ## prepared `tigris_entities` (one row per `tigris_name`, with columns
  ## n_tigris_polygons, tigris_namelsad, home_state).
  if (is.null(tigris_entities)) {
    native_raw <- tigris::native_areas(year = year, cb = FALSE) %>%
      janitor::clean_names()

    tigris_entities <- native_raw %>%
      sf::st_drop_geometry() %>%
      as_tibble() %>%
      filter(aiannhr == "F", !str_detect(namelsad, "Hawaii")) %>%
      summarize(
        .by = name,
        n_tigris_polygons = n(),
        tigris_namelsad = str_c(sort(unique(namelsad)), collapse = "; ")) %>%
      rename(tigris_name = name)

    ## Plurality home state: intersect each federal, non-Hawaiian polygon with
    ## the state layer in an equal-area CRS (CRS_PROJ, EPSG:6933), sum intersection area
    ## by state across polygons sharing a NAME, and keep the largest share —
    ## the majority state when one exceeds 50%, the plurality state otherwise.
    states_sf <- tigris::states(year = year, cb = TRUE) %>%
      janitor::clean_names() %>%
      sf::st_transform(CRS_PROJ) %>%
      sf::st_make_valid() %>%
      select(home_state = stusps)

    tigris_polys_sf <- native_raw %>%
      filter(aiannhr == "F", !str_detect(namelsad, "Hawaii")) %>%
      sf::st_transform(CRS_PROJ) %>%
      sf::st_make_valid() %>%
      select(tigris_name = name)

    tigris_home_state <- suppressWarnings(
        sf::st_intersection(tigris_polys_sf, states_sf)) %>%
      sf::st_make_valid() %>%
      mutate(area_m2 = as.numeric(sf::st_area(geometry))) %>%
      sf::st_drop_geometry() %>%
      summarize(area_m2 = sum(area_m2), .by = c(tigris_name, home_state)) %>%
      slice_max(area_m2, n = 1, by = tigris_name, with_ties = FALSE) %>%
      select(tigris_name, home_state)

    tigris_entities <- tigris_entities %>%
      left_join(tigris_home_state, by = "tigris_name",
                relationship = "one-to-one")

    message(
      "tigris::native_areas(", year, "): ", nrow(native_raw), " areas total; ",
      nrow(tigris_entities), " federal non-Hawaiian entities (distinct NAME).")
  } else {
    ## Caller-supplied records (e.g. the get_indigenous_universe() universe).
    required_cols <- c(
      "tigris_name", "n_tigris_polygons", "tigris_namelsad", "home_state")
    missing_cols <- setdiff(required_cols, names(tigris_entities))
    if (length(missing_cols) > 0) {
      stop("compare_bia_to_tigris_native_areas: supplied `tigris_entities` is ",
           "missing column(s): ", str_c(missing_cols, collapse = ", "), ".",
           call. = FALSE)
    }

    tigris_entities <- tigris_entities %>%
      sf::st_drop_geometry() %>%
      as_tibble() %>%
      distinct(tigris_name, .keep_all = TRUE)

    message(
      "Using supplied tigris records: ", nrow(tigris_entities),
      " entities (distinct NAME).")
  }

  ## -- Normalize both name sets to token sets ---------------------------------
  bia_tokens <- normalize_to_tokens(bia$bia_name)
  tigris_tokens <- normalize_to_tokens(tigris_entities$tigris_name)
  bia_key <- map_chr(bia_tokens, ~ str_c(sort(.x), collapse = " "))
  tigris_key <- map_chr(tigris_tokens, ~ str_c(sort(.x), collapse = " "))

  ## -- Best tigris match for each BIA entity ----------------------------------
  match_one_bia <- function(i) {
    a <- bia_tokens[[i]]
    if (length(a) == 0) {
      return(tibble(j = NA_integer_, status = "bia_only", sim = 0))
    }
    sims <- map_dbl(tigris_tokens, ~ jaccard_similarity(a, .x))

    exact <- which(nchar(bia_key[i]) > 0 & tigris_key == bia_key[i])
    if (length(exact) > 0) {
      return(tibble(j = exact[which.max(sims[exact])],
                    status = "matched_exact", sim = 1))
    }
    contained <- which(map_lgl(
      tigris_tokens,
      ~ length(.x) > 0 && (all(.x %in% a) || all(a %in% .x))))
    if (length(contained) > 0) {
      j <- contained[which.max(sims[contained])]
      return(tibble(j = j, status = "matched_subset", sim = sims[j]))
    }
    j <- which.max(sims)
    if (sims[j] >= fuzzy_threshold) {
      return(tibble(j = j, status = "matched_fuzzy", sim = sims[j]))
    }
    tibble(j = j, status = "bia_only", sim = sims[j])
  }

  bia_match <- map(seq_len(nrow(bia)), match_one_bia) %>% bind_rows()

  bia_resolved <- bia %>%
    mutate(
      match_status = bia_match$status,
      match_similarity = round(bia_match$sim, 3),
      tigris_name = if_else(
        str_starts(match_status, "matched"),
        tigris_entities$tigris_name[bia_match$j], NA_character_),
      nearest_counterpart = if_else(
        match_status == "bia_only",
        tigris_entities$tigris_name[bia_match$j], NA_character_),
      in_bia = TRUE,
      in_tigris = str_starts(match_status, "matched")) %>%
    left_join(tigris_entities, by = "tigris_name", relationship = "many-to-one")

  ## -- tigris entities claimed by no BIA entity -------------------------------
  claimed_names <- bia_resolved %>%
    filter(in_tigris) %>%
    pull(tigris_name) %>%
    unique()

  tigris_only_idx <- which(!tigris_entities$tigris_name %in% claimed_names)
  nearest_bia <- map(tigris_only_idx, function(j) {
    sims <- map_dbl(bia_tokens, ~ jaccard_similarity(tigris_tokens[[j]], .x))
    k <- which.max(sims)
    tibble(nearest_counterpart = bia$bia_name[k],
           match_similarity = round(sims[k], 3))
  }) %>% bind_rows()

  tigris_only <- tigris_entities[tigris_only_idx, ] %>%
    bind_cols(nearest_bia) %>%
    mutate(match_status = "tigris_only", in_bia = FALSE, in_tigris = TRUE)

  ## -- Assemble the full-outer reconciliation ---------------------------------
  status_levels <- c(
    "bia_only", "tigris_only", "matched_fuzzy", "matched_subset",
    "matched_exact")

  reconciliation <- bind_rows(bia_resolved, tigris_only) %>%
    mutate(match_status = factor(match_status, levels = status_levels)) %>%
    arrange(match_status, bia_name, tigris_name) %>%
    mutate(match_status = as.character(match_status)) %>%
    select(
      bia_name, bia_region, bia_alaska_native, bia_is_cross_reference,
      tigris_name, n_tigris_polygons, tigris_namelsad, home_state,
      in_bia, in_tigris, match_status, match_similarity, nearest_counterpart)

  ## -- Summary + gap flags ----------------------------------------------------
  status_counts <- reconciliation %>% count(match_status)
  n_bia_only <- sum(reconciliation$match_status == "bia_only")
  n_tigris_only <- sum(reconciliation$match_status == "tigris_only")
  n_matched <- sum(str_starts(reconciliation$match_status, "matched"))

  message(
    "BIA-to-tigris reconciliation: ", n_matched, " matched (",
    str_c(status_counts$match_status, "=", status_counts$n, collapse = ", "),
    ").")

  if (n_bia_only > 0) {
    warning(
      n_bia_only, " BIA tribe(s) have no matching tigris::native_areas ",
      "entity (`bia_only`) — these are NOT reflected in the polygon ",
      "universe. Inspect `match_status == \"bia_only\"`; `nearest_counterpart`",
      " shows the closest tigris candidate.", call. = FALSE)
  }
  if (n_tigris_only > 0) {
    warning(
      n_tigris_only, " tigris::native_areas entit(ies) match no BIA tribe ",
      "(`tigris_only`) — these polygons may not correspond to a listed ",
      "tribe (joint-use OTSAs, name divergences, or non-BIA areas). Inspect ",
      "`match_status == \"tigris_only\"`.", call. = FALSE)
  }

  reconciliation
}
