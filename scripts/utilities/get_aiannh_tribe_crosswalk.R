## ---------------------------------------------------------------------------
## get_aiannh_tribe_crosswalk.R — AIANNHCE → consolidated-tribe membership
##
## Builds the crosswalk from Census AIANNHCE codes to the consolidated tribe(s)
## each belongs to in the feature-1 universe, so AIANNH-coded source data can
## be collapsed to that universe. Polygons co-owned by several tribes (shared
## OTSAs, JUAs) carry equal proportional shares per co-owner. Sourced by
## get_tribal_crosswalks.R.
## ---------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(tigris)
library(janitor)
library(here)

## Chains generate_indigenous_universe.R (the consolidation-table accessors and
## get_federal_tribe_rows()), compare_bia_tigris_native_areas.R, the BIA list,
## and assemble_indigenous_tribe_polygons().
source(here::here("scripts", "utilities", "build_indigenous_tribe_polygons.R"))

options(tigris_use_cache = TRUE)

## Session cache: each Feature 2 figure calls get_tribal_crosswalk() (county /
## tract / zcta), which calls get_aiannh_tribe_crosswalk(); the tribe-polygon
## assembly is expensive, so memoize the membership per year for the session.
.aiannh_tribe_membership_cache <- new.env(parent = emptyenv())

#' AIANNHCE -> federally-recognized tribe membership with proportional shares
#'
#' Builds the lookup that lets Feature 2's population / dollar-allocation figures
#' collapse the `crosswalk` package's raw Census AIANNH targets into the
#' Feature 1 consolidated-tribe universe, mirroring the three consolidation
#' rules Feature 1 applies geometrically:
#' \enumerate{
#'   \item Post-consolidation links from `assemble_indigenous_tribe_polygons()`
#'     (normal 1:1 matches, multi-component tribes, and shared OTSAs).
#'   \item Duplicate-name constituents — the raw rancheria/colony `aiannhce`
#'     codes that `get_duplicate_name_consolidation()` unions away (e.g. the
#'     seven Pit River rows), recovered from `tigris::native_areas()` and mapped
#'     to the tribe that owns the consolidated geoid.
#'   \item JUA polygons — the `get_jua_distribution()` geoids (dropped from the
#'     Feature 1 universe), mapped to their recipient tribes.
#' }
#' A handful of `aiannhce` codes are legitimately co-used by several federally
#' recognized tribes: the shared OTSAs (Creek 5620, Kiowa-Comanche-Apache-Fort
#' Sill Apache 5720, Caddo-Wichita-Delaware 5540, Citizen Potawatomi-Absentee
#' Shawnee 5600, Wind River 4610) and the joint-use-area (JUA) polygons that
#' Feature 1 distributes into multiple recipient tribes. Each such geoid is
#' retained once per co-owning tribe with `tribe_share = 1 / n_sharers`
#' (equal-shares proportional split), so a shared area's population / dollars
#' are divided across its co-owners rather than attributed wholesale to one
#' canonical tribe (which had left co-owners like the Eastern Shoshone Tribe
#' looking landless/unexposed) — and never double-counted, since the shares
#' sum to 1 per geoid. AIANNH polygons with no federally recognized tribe
#' (`tigris_no_tribe`), HHL, ANRC (not an AIANNH class), and SDTSAs are simply
#' absent from the result and so are dropped by the consuming join.
#'
#' @param year Integer. tigris vintage. Default `2024`.
#' @return A tibble, one row per (federal `aiannhce`, co-owning tribe), with
#'   columns `geoid_native` (the AIANNHCE / crosswalk `target_geoid`),
#'   `tribe_name` (canonical BIA tribe name), `tribe_share` (this tribe's
#'   equal-shares fraction of the geoid; 1 for unshared geoids and shares sum
#'   to 1 per geoid), `is_shared` (TRUE where the geoid has multiple
#'   co-owners), and `representative_geoid` (a stable unique per-tribe id: the
#'   smallest geoid assigned to the tribe, preferring unshared geoids, with an
#'   alphabetical-rank `-2`/`-3`... suffix where several tribes' only land is
#'   the same shared polygon).
get_aiannh_tribe_crosswalk <- function(year = 2024) {
  cache_key <- as.character(year)
  cached <- .aiannh_tribe_membership_cache[[cache_key]]
  if (!is.null(cached)) return(cached)

  ## -- (a) post-consolidation geoid -> tribe links -----------------------------
  tribes <- assemble_indigenous_tribe_polygons(year = year) %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    filter(
      record_type == "federally_recognized_tribe",
      has_polygon,
      !is.na(tigris_geoids))

  step_a <- tribes %>%
    transmute(tribe_name, geoid_native = str_split(tigris_geoids, "; ")) %>%
    unnest_longer(geoid_native) %>%
    distinct(geoid_native, tribe_name)

  ## -- raw federal native_areas attributes (one row per AIANNHCE) -------------
  native_raw <- tigris::native_areas(year = year, cb = FALSE) %>%
    janitor::clean_names() %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    filter(aiannhr == "F") %>%
    transmute(geoid_native = aiannhce, name, namelsad) %>%
    distinct(geoid_native, .keep_all = TRUE)

  ## -- (b) duplicate-name constituent geoids -> canonical tribe ---------------
  dnc <- get_duplicate_name_consolidation()
  step_b <- native_raw %>%
    mutate(target_geoid = map_chr(namelsad, function(nm) {
      hits <- which(str_detect(nm, dnc$namelsad_pattern))
      if (length(hits) == 1) dnc$target_geoid[hits] else NA_character_
    })) %>%
    filter(!is.na(target_geoid)) %>%
    left_join(step_a, by = c("target_geoid" = "geoid_native")) %>%
    filter(!is.na(tribe_name)) %>%
    distinct(geoid_native, tribe_name)

  ## -- (c) JUA geoids -> recipient tribes -------------------------------------
  ## Recipient names are raw tigris NAMEs; resolve to their geoid then to the
  ## owning tribe(s) via step_a (a recipient that is itself a shared OTSA, e.g.
  ## "Creek", fans out to its sharing tribes — the canonical collapse below
  ## reduces the JUA to one tribe anyway).
  jua <- get_jua_distribution()
  name_to_geoid <- native_raw %>% distinct(name, geoid_native)
  step_c <- jua %>%
    left_join(name_to_geoid, by = c("recipient_name" = "name")) %>%
    rename(recipient_geoid = geoid_native) %>%
    ## A recipient that is itself a shared OTSA (e.g. "Creek") owns one geoid but
    ## several tribes, so this is genuinely many-to-many; the canonical collapse
    ## below reduces each JUA to a single tribe.
    left_join(step_a, by = c("recipient_geoid" = "geoid_native"),
              relationship = "many-to-many") %>%
    filter(!is.na(tribe_name)) %>%
    transmute(geoid_native = jua_geoid, tribe_name) %>%
    distinct(geoid_native, tribe_name)

  membership_raw <- bind_rows(step_a, step_b, step_c) %>%
    distinct(geoid_native, tribe_name)

  ## -- proportional (equal-shares) split for shared geoids --------------------
  ## Each co-owner of a shared geoid keeps a row with tribe_share = 1/n_sharers,
  ## so shares sum to 1 per geoid and no geography is double-counted.
  shared_membership <- membership_raw %>%
    mutate(
      .by = geoid_native,
      n_sharers = n_distinct(tribe_name),
      tribe_share = 1 / n_sharers,
      is_shared = n_sharers > 1)

  ## Stable unique per-tribe id: the smallest geoid assigned to the tribe,
  ## preferring unshared geoids (a tribe's own land over a co-owned polygon).
  ## Where several tribes' only land is the same shared polygon (e.g. the
  ## Creek tribal towns within the Creek OTSA), disambiguate with an
  ## alphabetical-rank suffix so target_geoid never conflates two tribes.
  representative_lookup <- shared_membership %>%
    summarize(
      .by = tribe_name,
      representative_geoid = if (any(!is_shared)) {
        min(geoid_native[!is_shared])
      } else {
        min(geoid_native)
      }) %>%
    arrange(representative_geoid, tribe_name) %>%
    mutate(
      .by = representative_geoid,
      representative_geoid = if_else(
        row_number() == 1,
        representative_geoid,
        str_c(representative_geoid, "-", row_number())))

  membership <- shared_membership %>%
    select(geoid_native, tribe_name, tribe_share, is_shared) %>%
    left_join(representative_lookup, by = "tribe_name",
              relationship = "many-to-one")

  ## Invariants: one row per (geoid, tribe); shares sum to 1 per geoid; one
  ## unique representative id per tribe.
  if (anyDuplicated(membership[, c("geoid_native", "tribe_name")]) > 0) {
    stop("get_aiannh_tribe_crosswalk: duplicated (geoid_native, tribe_name) ",
         "row(s) after proportional split.", call. = FALSE)
  }
  share_check <- membership %>%
    summarize(.by = geoid_native, share_total = sum(tribe_share))
  if (any(abs(share_check$share_total - 1) > 1e-8)) {
    stop("get_aiannh_tribe_crosswalk: tribe_share does not sum to 1 for ",
         sum(abs(share_check$share_total - 1) > 1e-8), " geoid(s).",
         call. = FALSE)
  }
  if (anyDuplicated(distinct(membership, tribe_name, representative_geoid)$representative_geoid) > 0) {
    stop("get_aiannh_tribe_crosswalk: representative_geoid collides across ",
         "tribes.", call. = FALSE)
  }

  shared_assignments <- membership %>%
    filter(is_shared) %>%
    summarize(.by = geoid_native,
              sharers = str_c(sort(tribe_name), collapse = " | ")) %>%
    arrange(geoid_native)
  message(
    "get_aiannh_tribe_crosswalk(", year, "): ",
    n_distinct(membership$geoid_native), " AIANNHCE -> ",
    n_distinct(membership$tribe_name), " tribes; ",
    nrow(shared_assignments),
    " shared geoid(s) split equally across co-owners:")
  if (nrow(shared_assignments) > 0) {
    walk2(shared_assignments$geoid_native, shared_assignments$sharers,
          ~ message("  ", .x, " -> ", .y))
  }

  .aiannh_tribe_membership_cache[[cache_key]] <- membership
  membership
}
