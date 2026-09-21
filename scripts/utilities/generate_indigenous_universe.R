## ---------------------------------------------------------------------------
## generate_indigenous_universe.R — tigris federal-tribe rows + consolidation
##
## Builds the tigris::native_areas() side of the universe. get_federal_tribe_rows()
## returns one row per tigris federal entity with duplicate-name consolidation
## (§1) and joint-use-area distribution (§2 of universe-definition.md) applied.
## Sourced by build_indigenous_tribe_polygons.R (the entry point) and by
## get_aiannh_tribe_crosswalk.R. Defines builders/accessors only — no side
## effects beyond setting the tigris cache option.
## ---------------------------------------------------------------------------
library(tigris)
library(sf)
library(tidyverse)
library(janitor)
library(here)

options(tigris_use_cache = TRUE)

#' Duplicate-name consolidation rules (accessor)
#'
#' A handful of federally-recognized tribes appear as several
#' `tigris::native_areas()` rows under distinct names (separate
#' rancherias/colonies/trust lands). Each rule matches those rows by a
#' `namelsad` regex and replaces them with one canonical row (its geometry is
#' their union). Exposed as an accessor so both `get_federal_tribe_rows()` and
#' the AIANNHCE-to-tribe membership builder
#' (`get_aiannh_tribe_crosswalk()`) share a single source of truth — the
#' membership builder needs these patterns to recover the constituent
#' `aiannhce` codes that the consolidation collapses away.
#'
#' @return A tibble with columns `target_name`, `target_geoid`, `expected_n`
#'   (the exact number of `native_areas()` rows the pattern must match —
#'   enforced by `get_federal_tribe_rows()` so a renamed component surfaces
#'   as an error rather than a silently smaller union), `namelsad_pattern`,
#'   and `rule_id`.
get_duplicate_name_consolidation <- function() {
  tribble(
    ~target_name,             ~target_geoid, ~expected_n, ~namelsad_pattern,
    ## 6 rancherias + Pit River Trust Land (universe-definition.md section 1).
    "Pit River",              "2835",        7L,
      str_c("Big Bend Rancheria|Likely Rancheria|Lookout Rancheria|",
            "Montgomery Creek Rancheria|Roaring Creek Rancheria|",
            "XL Ranch Rancheria|Pit River Trust Land"),
    ## Fallon Reservation + Fallon Colony.
    "Fallon Paiute-Shoshone", "1075",        2L,          "Fallon Paiute-Shoshone") %>%
    mutate(rule_id = row_number())
}

#' Joint-use-area (JUA) distribution rules (accessor)
#'
#' Several JUA polygons (e.g. the Creek/Seminole OTSA) are co-used by two tribes
#' but are not the footprint of any single one. Each JUA's geometry is unioned
#' into every recipient row (matched by tigris name) and the standalone JUA row
#' is then dropped. Exposed as an accessor so the membership builder
#' (`get_aiannh_tribe_crosswalk()`) can map each JUA `aiannhce` to its recipient
#' tribes (the JUA rows no longer exist in the post-distribution
#' `federal_rows`).
#'
#' @return A long tibble (one row per JUA geoid x recipient name) with columns
#'   `jua_geoid` and `recipient_name`.
get_jua_distribution <- function() {
  tribble(
    ~jua_geoid, ~recipient_name,
    "5915",     c("Creek", "Seminole"),
    "5950",     c("Kaw", "Ponca"),
    "5970",     c("Miami", "Peoria"),
    "5955",     c("Kiowa-Comanche-Apache-Fort Sill Apache",
                  "Caddo-Wichita-Delaware"),
    "4910",     c("Kickapoo (KS)", "Sac and Fox Nation"),
    "4930",     c("San Felipe", "Santa Ana"),
    "4940",     c("San Felipe", "Santo Domingo")) %>%
    unnest_longer(recipient_name)
}

#' Federal tigris::native_areas rows, consolidated and JUA-distributed
#'
#' Builds the federal (non-Hawaiian) `tigris::native_areas()` polygon set that
#' underlies the BIA-to-tigris reconciliation: one row per `geoid_native`
#' (`aiannhce`), with two hand-curated rewrites applied first —
#' (1) duplicate-name consolidation (e.g. the Pit River rancherias + trust land
#' unioned into one "Pit River" row) and (2) joint-use-area distribution (each
#' JUA polygon unioned into its recipient rows, then the standalone JUA row
#' dropped). Both steps validate strictly so vintage drift surfaces as an error
#' rather than silent geometry loss.
#'
#' @param year Integer. TIGER vintage for `tigris::native_areas()` and
#'   `tigris::states()`. Default `2024`.
#' @return An `sf` tibble, one row per `geoid_native`, with columns
#'   `geoid_native`, `name`, `namelsad`, `home_state` (plurality state by
#'   intersection area), and `geometry`.
get_federal_tribe_rows <- function(year = 2024) {

  states <- tigris::states(cb = TRUE)

  ## -- tigris::native_areas: partition into federal / state / HHL -------------
  native1 <- tigris::native_areas(year = year, cb = FALSE) %>%
    clean_names() %>%
    filter(aiannhr == "F") %>%
    rename(geoid_native = aiannhce) %>%
    summarize(
      .by = geoid_native,
      name = first(name),
      namelsad = first(namelsad),
      geometry = st_union(geometry))

  native_state_intersection <- native1 %>%
    mutate(native_area = st_area(.) %>% as.numeric()) %>%
    st_intersection(states)

  native_state_intersection2 <- native_state_intersection %>%
    st_make_valid() %>%
    mutate(
      intersection_area = st_area(.) %>% as.numeric(),
      intersection_percent = intersection_area / native_area)

  native_intersection3 <- native_state_intersection2 %>%
    arrange(
      geoid_native, desc(intersection_percent)) %>%
    select(geoid_native, intersection_percent, home_state = STUSPS) %>%
    st_drop_geometry() %>%
    slice_head(by = geoid_native, n = 1) %>%
    as_tibble()

  native2 <- native1 %>%
    tidylog::left_join(native_intersection3, by = "geoid_native")

  hhl_rows <- native2 %>% filter(str_detect(namelsad, "Hawaii"))
  federal_rows_raw <- native2 %>%
    filter(!str_detect(namelsad, "Hawaii"))

  ## -- Hand-curated rewrites of tigris::native_areas federal rows ---------------
  ## Two adjustments precede any downstream BIA-to-tigris matching:
  ##
  ## (1) Duplicate-name consolidation. A handful of federally-recognized tribes
  ##     appear as several native_areas rows under distinct names (separate
  ##     rancherias/colonies/trust lands, or a tribe split across "Reservation"
  ##     and "Colony" records). Each rule matches those rows by a `namelsad`
  ##     regex and replaces them with one row whose geometry is their union and
  ##     whose name/geoid are the canonical values.
  ##
  ## (2) Joint-use area (JUA) distribution. Several JUA polygons (e.g. the
  ##     Creek/Seminole OTSA) are co-used by two tribes but are not the footprint
  ##     of any single tribe. Each JUA's geometry is unioned into every recipient
  ##     row (matched by name) and the standalone JUA row is then dropped.
  ##
  ## Both steps validate strictly (a rule matching no rows, a JUA geoid that is
  ## not unique, or a recipient name with no row all error) so vintage drift
  ## surfaces as a load-time failure rather than silent geometry loss.

  duplicate_name_consolidation <- get_duplicate_name_consolidation()

  jua_distribution <- get_jua_distribution()

  ## (1) Duplicate-name consolidation -------------------------------------------
  ## Tag each federal row with the consolidation rule (if any) whose namelsad
  ## regex it matches; error if a row matches more than one rule.
  federal_rows_tagged <- federal_rows_raw %>%
    mutate(rule_id = map_int(namelsad, function(namelsad_i) {
      hits <- which(str_detect(
        namelsad_i, duplicate_name_consolidation$namelsad_pattern))
      if (length(hits) > 1) {
        stop("duplicate_name_consolidation: \"", namelsad_i, "\" matches ",
             length(hits), " patterns (expected <= 1).", call. = FALSE)
      }
      if (length(hits) == 0) NA_integer_ else hits
    }))

  ## Check: every rule matched exactly its expected number of component rows.
  ## A shortfall (e.g. a TIGER vintage renaming one rancheria) would otherwise
  ## silently shrink the consolidated geometry.
  rule_match_counts <- duplicate_name_consolidation %>%
    left_join(
      federal_rows_tagged %>%
        st_drop_geometry() %>%
        count(rule_id, name = "n_matched"),
      by = "rule_id") %>%
    mutate(n_matched = coalesce(n_matched, 0L))

  rules_drifted <- rule_match_counts %>%
    filter(n_matched != expected_n)
  if (nrow(rules_drifted) > 0) {
    stop("duplicate_name_consolidation: rule(s) matched an unexpected row ",
         "count in tigris::native_areas(", year, "): ",
         str_c(rules_drifted$target_name, " (matched ", rules_drifted$n_matched,
               ", expected ", rules_drifted$expected_n, ")", collapse = "; "),
         ". Review the table for vintage drift.", call. = FALSE)
  }

  ## Union each rule's rows into one canonical row (sf unions geometry within
  ## summarize by default); carry the first component's home_state.
  consolidated_rows <- federal_rows_tagged %>%
    filter(!is.na(rule_id)) %>%
    group_by(rule_id) %>%
    summarize(home_state = first(home_state), .groups = "drop") %>%
    st_make_valid() %>%
    left_join(duplicate_name_consolidation, by = "rule_id") %>%
    mutate(
      geoid_native = target_geoid,
      name = target_name,
      namelsad = str_c(target_name, " (consolidated)")) %>%
    select(geoid_native, name, namelsad, home_state)

  walk(duplicate_name_consolidation$rule_id, ~ message(
    "duplicate-name consolidation: unioned ",
    sum(federal_rows_tagged$rule_id == .x, na.rm = TRUE),
    " rows into \"", duplicate_name_consolidation$target_name[.x],
    "\" (geoid ", duplicate_name_consolidation$target_geoid[.x], ")."))

  federal_rows_consolidated <- federal_rows_tagged %>%
    filter(is.na(rule_id)) %>%
    select(-rule_id) %>%
    bind_rows(consolidated_rows) %>%
    st_as_sf()

  ## (2) Joint-use area (JUA) distribution --------------------------------------
  ## Pull each JUA polygon's geometry from the consolidated set; require exactly
  ## one row per JUA geoid.
  jua_geoms <- federal_rows_consolidated %>%
    filter(geoid_native %in% jua_distribution$jua_geoid) %>%
    select(jua_geoid = geoid_native)

  jua_bad <- jua_distribution %>%
    distinct(jua_geoid) %>%
    left_join(count(st_drop_geometry(jua_geoms), jua_geoid), by = "jua_geoid") %>%
    filter(is.na(n) | n != 1)
  if (nrow(jua_bad) > 0) {
    stop("jua_distribution: JUA geoid(s) ", str_c(jua_bad$jua_geoid, collapse = ", "),
         " did not match exactly one row in tigris::native_areas(", year, ").",
         call. = FALSE)
  }

  ## Check: every recipient name exists among the federal rows.
  recipients_missing <- setdiff(
    jua_distribution$recipient_name, federal_rows_consolidated$name)
  if (length(recipients_missing) > 0) {
    stop("jua_distribution: recipient name(s) ",
         str_c(recipients_missing, collapse = ", "),
         " matched 0 rows in tigris::native_areas(", year, ").", call. = FALSE)
  }

  ## One unioned JUA geometry per recipient name (a recipient can co-use more
  ## than one JUA, e.g. San Felipe).
  recipient_jua_geom <- jua_distribution %>%
    left_join(jua_geoms, by = "jua_geoid") %>%
    st_as_sf() %>%
    group_by(recipient_name) %>%
    summarize(.groups = "drop") %>%
    st_make_valid()
  recipient_geom_by_name <- set_names(
    st_geometry(recipient_jua_geom), recipient_jua_geom$recipient_name)

  ## Drop the standalone JUA rows, then union each JUA into its recipients' rows.
  ## A recipient name can map to several rows (e.g. a tribe split across a
  ## reservation and a trust-land row); each receives the JUA geometry.
  target_crs <- st_crs(federal_rows_consolidated)
  federal_rows <- federal_rows_consolidated %>%
    filter(!geoid_native %in% jua_distribution$jua_geoid) %>%
    mutate(geometry = st_sfc(
      map2(geometry, name, function(geom_i, name_i) {
        if (!name_i %in% names(recipient_geom_by_name)) return(geom_i)
        ## Make both parts valid BEFORE unioning: an invalid recipient geometry
        ## (e.g. Kickapoo (KS), geoid 1770) otherwise unions to a zero-area
        ## sliver that the trailing st_make_valid() then collapses to empty,
        ## silently dropping the reservation. Validating first preserves it.
        st_union(st_make_valid(st_sfc(geom_i, recipient_geom_by_name[[name_i]],
                                      crs = target_crs)))[[1]]
      }),
      crs = target_crs)) %>%
    st_make_valid()

  walk(unique(jua_distribution$jua_geoid), function(geoid_i) {
    recipients_i <- jua_distribution$recipient_name[
      jua_distribution$jua_geoid == geoid_i]
    message("JUA distribution: geoid ", geoid_i, " unioned into ",
            length(recipients_i), " recipient name(s) (",
            str_c(recipients_i, collapse = ", "), "); JUA row dropped.")
  })

  ## Check: JUA rows are gone and entity ids remain unique.
  stopifnot(!any(jua_distribution$jua_geoid %in% federal_rows$geoid_native))
  if (anyDuplicated(federal_rows$geoid_native) > 0) {
    warning("federal_rows: duplicate geoid_native after JUA distribution.",
            call. = FALSE)
  }

  message(
    "tigris::native_areas(", year, "): ",
    nrow(federal_rows), " federal (non-HHL, post-consolidation/JUA), ",
    nrow(hhl_rows), " Hawaiian Home Lands.")

  federal_rows
}
