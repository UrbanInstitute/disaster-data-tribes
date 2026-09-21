## ---------------------------------------------------------------------------
## build_indigenous_tribe_polygons.R — canonical one-polygon-per-tribe builder
##
## ENTRY POINT for the federal-tribe universe. Sourced by
## scripts/feature_1/02_get_indigenous_universe.R. Produces exactly one row per
## federally recognized tribe by folding together three sources:
##   1. generate_indigenous_universe.R — tigris native_areas rows with
##      duplicate-name consolidation and joint-use-area distribution applied
##      (get_federal_tribe_rows()).
##   2. compare_bia_tigris_native_areas.R — the first-pass token matcher that
##      links BIA tribes to tigris polygons (compare_bia_to_tigris_native_areas()).
##   3. get_bia_tigris_native_areas_crosswalk() + bia_tigris_match_corrections
##      (defined below) — the hand-audited crosswalk and corrections that resolve
##      and fix the matches the first pass missed or got wrong.
## See universe-definition.md for the full catalog of edge cases handled here.
## ---------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(here)

source(here::here("scripts", "utilities", "get_bia_federally_recognized_tribes.R"))
source(here::here("scripts", "utilities", "generate_indigenous_universe.R"))
source(here::here("scripts", "utilities", "compare_bia_tigris_native_areas.R"))

## ===========================================================================
## Manual corrections to the first-pass BIA <-> tigris reconciliation
## ===========================================================================
## `compare_bia_to_tigris_native_areas()` matches every BIA tribe to its best
## tigris::native_areas() entity by normalized-token Jaccard. That first pass is
## deliberately simple, and the crosswalk built here resolves the 50 `bia_only`
## and 70 `tigris_only` records it leaves behind (see
## verify_bia_tigris_native_areas.R for the standalone reconciliation check).
## But a token matcher also produces WRONG matches among the rows it *did*
## match — usually when a short, generic
## tigris NAME (a single token) is contained in an unrelated, longer BIA name.
## Auditing all 525 matched pairs surfaced the cases below. Each is the
## authoritative resolution for a tribe whose first-pass (and, where noted,
## crosswalk) link was wrong or incomplete.
##
## How these compose with the crosswalk (see `build_indigenous_tribe_polygons`):
## a tribe that appears in EITHER this table OR the crosswalk is resolved
## ENTIRELY from those two sources (its first-pass match is ignored), and the
## two sources are UNIONED. That is why the "component completion" rows exist:
## for a few multi-area tribes the crosswalk lists most components but omits the
## one the first pass happened to catch; without re-adding it here that real
## component would be dropped when the parent is resolved from the crosswalk.
##
## Maintenance: this table is hand-curated. Edit it (and/or
## get_bia_tigris_native_areas_crosswalk()) when the BIA Federal Register list
## or the tigris vintage changes, or when a low-confidence match is hand-reviewed
## and overturned. universe-definition.md (§3–§9) catalogs every case resolved
## here and is the narrative companion to keep in sync.
##
## Columns:
##   bia_name      Exact BIA Federal Register name (matches get_bia...$name).
##   tigris_name   Exact tigris NAME to link to, or NA when `tigris_geoid` is
##                 given instead, or NA for `no_polygon`.
##   tigris_geoid  tigris `aiannhce` (geoid_native) to link to directly. Used
##                 only to disambiguate a NAME shared by two different tribes
##                 (Santa Rosa) or to pin a specific ANVSA; NA otherwise.
##   relationship  "same_entity" | "component_area" | "shared_otsa" |
##                 "no_polygon" (tribe has no distinct federal AIANNH polygon).
##   confidence    Reviewer confidence (high/medium).
##   note          One-line justification.
bia_tigris_match_corrections <- tibble::tribble(
  ~bia_name, ~tigris_name, ~tigris_geoid, ~relationship, ~confidence, ~note,

  ## -- Name collision: two different tribes share the tigris NAME "Santa Rosa".
  ## The universe summarizes polygons by NAME, so it would otherwise merge the
  ## Tachi Yokut Rancheria and the Cahuilla Reservation; pin each to its geoid.
  "Santa Rosa Indian Community of the Santa Rosa Rancheria, California", NA, "3520", "same_entity", "high",
    "Santa Rosa Rancheria (Tachi Yokut, Kings Co., geoid 3520); separated from the Cahuilla reservation that shares the NAME.",
  "Santa Rosa Band of Cahuilla Indians, California", NA, "3525", "same_entity", "high",
    "Santa Rosa Reservation (Cahuilla, Riverside Co., geoid 3525); separated from the Tachi Yokut rancheria that shares the NAME.",

  ## -- Wrong first-pass match, tribe has no distinct federal AIANNH polygon ----
  "Central Council of the Tlingit & Haida Indian Tribes", NA, NA, "no_polygon", "high",
    "Regional SE-Alaska tribe; first pass grabbed the unrelated Council ANVSA (Native Village of Council). No single ANVSA of its own.",
  "Inupiat Community of the Arctic Slope", NA, NA, "no_polygon", "high",
    "Regional North-Slope tribe; first pass grabbed the unrelated Arctic Village ANVSA (a Gwich'in village). No ANVSA of its own.",
  "United Keetoowah Band of Cherokee Indians in Oklahoma", NA, NA, "no_polygon", "medium",
    "First pass grabbed the Cherokee OTSA (= Cherokee Nation). UKB's parcels fall within that OTSA but it has no distinct federal AIANNH polygon.",
  "Delaware Tribe of Indians", NA, NA, "no_polygon", "medium",
    "First pass grabbed the Caddo-Wichita-Delaware OTSA (= Delaware Nation, a different tribe). Operates within the Cherokee Nation OTSA; no distinct federal AIANNH polygon.",

  ## -- Wrong first-pass match, reassign to the correct area ---------------------
  "Iqugmiut Traditional Council", "Russian Mission", NA, "same_entity", "high",
    "Iqugmiut Traditional Council governs Russian Mission (Yukon); first pass grabbed the Council ANVSA. Russian Mission ANVSA freed once Chuathbaluk took its own ANVSA.",
  "Absentee-Shawnee Tribe of Indians of Oklahoma", "Citizen Potawatomi Nation-Absentee Shawnee", NA, "shared_otsa", "high",
    "Absentee-Shawnee shares the Citizen Potawatomi Nation-Absentee Shawnee OTSA; first pass grabbed the Shawnee Trust Land (= the Shawnee Tribe).",

  ## -- Cross-reference row whose first pass missed its own ANVSA ----------------
  "Arctic Village (See Native Village of Venetie Tribal Government)", NA, "6140", "component_area", "high",
    "Arctic Village ANVSA (geoid 6140) is administered by the Native Village of Venetie Tribal Government; first pass grabbed the Venetie ANVSA. Folds into the Venetie govt tribe alongside the Venetie ANVSA.",

  ## -- Component completion: real components the crosswalk omitted --------------
  ## The parent is resolved from the crosswalk, so the one component the first
  ## pass caught (and the crosswalk did not relist) must be re-added here.
  "Minnesota Chippewa Tribe, Minnesota (Six component reservations: Bois Forte Band (Nett Lake); Fond du Lac Band; Grand Portage Band; Leech Lake Band; Mille Lacs Band; White Earth Band)", "Fond du Lac", NA, "component_area", "high",
    "Fond du Lac is a statutory MCT component reservation omitted from the crosswalk's six-name MCT list; re-added so it is not dropped.",
  "Washoe Tribe of Nevada & California (Carson Colony, Dresslerville Colony, Woodfords Community, Stewart Community, & Washoe Ranches)", "Washoe Ranches", NA, "component_area", "high",
    "Washoe Ranches is a constituent Washoe parcel (named in the BIA title) not listed in the crosswalk's Washoe components; re-added.",
  "Te-Moak Tribe of Western Shoshone Indians of Nevada (Four constituent bands: Battle Mountain Band; Elko Band; South Fork Band; and Wells Band)", "Battle Mountain", NA, "component_area", "high",
    "Battle Mountain is the fourth Te-Moak constituent band (named in the BIA title) not listed in the crosswalk's Te-Moak components; re-added.",
  "Capitan Grande Band of Diegueno Mission Indians of California (Barona Group of Capitan Grande Band of Mission Indians of the Barona Reservation, California; Viejas (Baron Long) Group of Capitan Grande Band of Mission Indians of the Viejas Reservation, California)", "Capitan Grande", NA, "component_area", "high",
    "The Capitan Grande reservation itself (held jointly by the Barona and Viejas groups) sits alongside the separately-occupied Barona and Viejas parcels in the crosswalk; re-added.",
  "Passamaquoddy Tribe", "Passamaquoddy", NA, "component_area", "high",
    "Passamaquoddy off-reservation trust land sits alongside the Indian Township and Pleasant Point reservations in the crosswalk; re-added.",
  "Yerington Paiute Tribe of the Yerington Colony & Campbell Ranch, Nevada", "Campbell", NA, "component_area", "high",
    "Campbell Ranch is the Yerington Paiute Tribe's second parcel alongside the Yerington Colony in the crosswalk; re-added.")


#' Resolve tigris NAME(s) to geoid_native, via a name->geoid lookup (helper)
#'
#' @param names Character vector of tigris NAMEs.
#' @param name_to_geoid Two-column tibble (`name`, `geoid_native`).
#' @return Character vector of matching `geoid_native` values (deduplicated).
geoids_for_names <- function(names, name_to_geoid) {
  name_to_geoid$geoid_native[name_to_geoid$name %in% names] %>% unique()
}

#' One polygon per federally recognized tribe, from the tigris area universe
#'
#' Collapses the BIA-to-tigris reconciliation, the hand-curated crosswalk, and
#' the audited match corrections into a single sf object in which every
#' federally recognized Tribal entity (Tribes and Alaska Native villages) gets
#' exactly one row carrying the union of every tigris::native_areas() polygon
#' that is owned by, held in trust for, or otherwise federally recognized as
#' belonging to that tribe. Tribes with no federal AIANNH polygon (landless,
#' relocated, state-reservation-only, or jurisdiction-within-another-reservation)
#' get a row with empty geometry and `has_polygon = FALSE`. tigris polygons that
#' correspond to no federally recognized tribe are retained as extra rows with
#' `record_type == "tigris_no_tribe"` so nothing is silently dropped.
#'
#' @section Resolution priority:
#'   For each BIA entity, its polygon set is determined as:
#'   \enumerate{
#'     \item If the entity appears in `corrections` or `crosswalk`, its polygons
#'       are the UNION of all links from those two sources (its first-pass match
#'       is ignored). A `no_polygon` correction or `bia_no_polygon` crosswalk row
#'       yields the empty set.
#'     \item Otherwise the entity keeps its first-pass `matched_*` tigris entity.
#'   }
#'   Cross-reference BIA rows (the "(See ...)" redirects) are folded into their
#'   `cross_reference_target` so, e.g., the St. Paul and St. George ANVSAs both
#'   land on the single "Pribilof Islands Aleut Communities ..." tribe. The
#'   resulting roster is the BIA notice's 573 substantive entities.
#'
#'   A polygon may be assigned to more than one tribe by design — shared OTSAs
#'   (e.g. the Creek, Caddo-Wichita-Delaware, and Citizen Potawatomi-Absentee
#'   Shawnee OTSAs) and shared reservations (Wind River) are duplicated into
#'   every sharing tribe. These are reported, not flagged as errors.
#'
#' @param reconciliation Output of `compare_bia_to_tigris_native_areas()` run
#'   against the `get_indigenous_universe()` records (i.e. `federal_rows`).
#' @param federal_rows The sf object left in scope by
#'   `generate_indigenous_universe.R`: one row per `geoid_native` (consolidations
#'   applied, joint-use areas distributed), with `name`, `namelsad`,
#'   `home_state`, and geometry.
#' @param crosswalk The hand-curated `bia_tigris_crosswalk`.
#' @param corrections The audited `bia_tigris_match_corrections` (default the
#'   table defined in this file).
#' @param bia The BIA list (default `get_bia_federally_recognized_tribes()`),
#'   used for region and cross-reference targets.
#' @return An sf tibble with one row per federally recognized tribe plus the
#'   `tigris_no_tribe` polygons. Columns: `tribe_name`, `bia_region`,
#'   `bia_alaska_native`, `record_type`, `has_polygon`, `n_tigris_polygons`,
#'   `tigris_geoids`, `tigris_namelsad`, `resolution`, `confidence`, `note`,
#'   and `geometry`.
build_indigenous_tribe_polygons <- function(reconciliation,
                                            federal_rows,
                                            crosswalk,
                                            corrections = bia_tigris_match_corrections,
                                            bia = get_bia_federally_recognized_tribes()) {

  fr <- sf::st_make_valid(federal_rows)
  fr_attr <- fr %>% sf::st_drop_geometry() %>% as_tibble()
  name_to_geoid <- fr_attr %>% distinct(name, geoid_native)
  target_crs <- sf::st_crs(fr)

  ## -- 1. Canonical tribe identity (fold cross-reference rows into targets) ----
  bia_identity <- bia %>%
    transmute(
      bia_name = name,
      bia_region = region,
      bia_alaska_native = alaska_native,
      tribe_name = if_else(is_cross_reference, cross_reference_target, name))

  ## -- 2. Pre-resolve each source to bia_name -> geoid links -------------------
  ## match() takes the FIRST hit, so a tigris NAME shared by unrelated entities
  ## (e.g. the two "Santa Rosa" polygons) would silently link a tribe to
  ## whichever geoid sorts first. Any ambiguous name must instead be pinned to
  ## an explicit tigris_geoid; crosswalk rows have no geoid column, so an
  ## ambiguous crosswalk link must move to `corrections` with a `tigris_geoid`.
  ambiguous_names <- name_to_geoid %>%
    count(name) %>%
    filter(n > 1) %>%
    pull(name)

  corr_ambiguous <- corrections %>%
    filter(
      relationship != "no_polygon",
      is.na(tigris_geoid),
      tigris_name %in% ambiguous_names)
  xwalk_ambiguous <- crosswalk %>%
    filter(
      !is.na(bia_name), !is.na(tigris_name),
      tigris_name %in% ambiguous_names)
  if (nrow(corr_ambiguous) > 0 || nrow(xwalk_ambiguous) > 0) {
    stop(
      "build_indigenous_tribe_polygons: tigris_name(s) matching more than one ",
      "federal_rows geoid must be pinned to an explicit tigris_geoid in ",
      "`corrections`:\n",
      str_c(
        c(str_c("  correction: ", corr_ambiguous$bia_name, " -> ",
                corr_ambiguous$tigris_name),
          str_c("  crosswalk: ", xwalk_ambiguous$bia_name, " -> ",
                xwalk_ambiguous$tigris_name)),
        collapse = "\n"),
      call. = FALSE)
  }

  corr_links <- corrections %>%
    filter(relationship != "no_polygon") %>%
    mutate(geoid_native = coalesce(
      tigris_geoid,
      name_to_geoid$geoid_native[match(tigris_name, name_to_geoid$name)])) %>%
    select(bia_name, geoid_native, confidence, note)
  corr_nopoly <- corrections %>%
    filter(relationship == "no_polygon")

  xwalk_links <- crosswalk %>%
    filter(!is.na(bia_name), !is.na(tigris_name)) %>%
    mutate(geoid_native = name_to_geoid$geoid_native[
      match(tigris_name, name_to_geoid$name)]) %>%
    select(bia_name, geoid_native, confidence, note)
  xwalk_nopoly <- crosswalk %>%
    filter(relationship == "bia_no_polygon")

  ## Validate that every named link resolved to a real geoid (catches typos /
  ## vintage drift in the crosswalk or corrections).
  bad_corr <- corr_links %>% filter(is.na(geoid_native))
  bad_xwalk <- xwalk_links %>% filter(is.na(geoid_native))
  if (nrow(bad_corr) > 0 || nrow(bad_xwalk) > 0) {
    stop("build_indigenous_tribe_polygons: link(s) did not resolve to a ",
         "federal_rows geoid:\n",
         str_c("  correction: ", bad_corr$bia_name, collapse = "\n"),
         str_c("  crosswalk: ", bad_xwalk$bia_name, collapse = "\n"),
         call. = FALSE)
  }

  manual_links <- bind_rows(corr_links, xwalk_links)
  manual_bia <- unique(manual_links$bia_name)
  nopoly_bia <- unique(c(corr_nopoly$bia_name, xwalk_nopoly$bia_name))

  ## -- 3. Resolve every BIA entity to its set of geoids ------------------------
  ## Priority: a tribe in corrections/crosswalk is resolved entirely from those
  ## (unioned); otherwise its first-pass matched entity is used.
  firstpass <- reconciliation %>%
    filter(str_starts(match_status, "matched"), !is.na(tigris_name)) %>%
    select(bia_name, tigris_name)

  resolve_one <- function(nm) {
    if (nm %in% nopoly_bia) {
      return(tibble(geoid_native = character(0)))
    }
    if (nm %in% manual_bia) {
      return(tibble(geoid_native = manual_links$geoid_native[
        manual_links$bia_name == nm] %>% unique()))
    }
    tn <- firstpass$tigris_name[firstpass$bia_name == nm]
    tibble(geoid_native = geoids_for_names(tn, name_to_geoid))
  }

  links <- bia_identity %>%
    mutate(resolved = map(bia_name, resolve_one)) %>%
    unnest(resolved)

  tribe_geoids <- links %>%
    filter(!is.na(geoid_native)) %>%
    distinct(tribe_name, geoid_native)

  ## -- 4. Per-tribe attributes & resolution provenance -------------------------
  ## Per-tribe note/confidence: concatenate any crosswalk/correction notes that
  ## fed this tribe (via any of its BIA rows); these are the manual adjustments.
  adjust_provenance <- links %>%
    distinct(tribe_name, bia_name) %>%
    left_join(
      manual_links %>% distinct(bia_name, confidence, note),
      by = "bia_name", relationship = "many-to-many") %>%
    left_join(
      bind_rows(corr_nopoly, xwalk_nopoly) %>%
        transmute(bia_name, confidence, note, nopoly = TRUE),
      by = "bia_name", suffix = c("", "_np")) %>%
    mutate(
      confidence = coalesce(confidence, confidence_np),
      note = coalesce(note, note_np)) %>%
    filter(!is.na(note)) %>%
    summarize(
      .by = tribe_name,
      ## Worst-case confidence across the tribe's manual links; min() on the
      ## raw character values would sort alphabetically ("high" < "low" <
      ## "medium") and report the opposite of the conservative floor.
      confidence = if (all(is.na(confidence))) NA_character_
        else as.character(min(
          factor(confidence, levels = c("low", "medium", "high"), ordered = TRUE),
          na.rm = TRUE)),
      note = str_c(unique(note), collapse = " | "))

  tribe_meta <- tribe_geoids %>%
    left_join(fr_attr %>% select(geoid_native, name, namelsad),
              by = "geoid_native") %>%
    summarize(
      .by = tribe_name,
      n_tigris_polygons = n_distinct(geoid_native),
      tigris_geoids = str_c(sort(unique(geoid_native)), collapse = "; "),
      tigris_names = str_c(sort(unique(name)), collapse = "; "),
      tigris_namelsad = str_c(sort(unique(namelsad)), collapse = "; "))

  tribe_attr <- bia_identity %>%
    summarize(
      .by = tribe_name,
      bia_region = first(bia_region),
      bia_alaska_native = first(bia_alaska_native)) %>%
    left_join(tribe_meta, by = "tribe_name") %>%
    left_join(adjust_provenance, by = "tribe_name") %>%
    mutate(
      n_tigris_polygons = coalesce(n_tigris_polygons, 0L),
      has_polygon = n_tigris_polygons > 0,
      record_type = "federally_recognized_tribe",
      resolution = case_when(
        !has_polygon & tribe_name %in% nopoly_bia ~ "no_polygon (audited)",
        !has_polygon ~ "no_polygon",
        tribe_name %in% manual_bia | !is.na(note) ~ "crosswalk/correction",
        TRUE ~ "matched_firstpass"))

  ## -- 5. Union polygons per tribe; add landless tribes with empty geometry ----
  ## group_by()/summarise() (rather than `.by`) so sf unions geometry within
  ## each tribe automatically; `.by` would require an explicit st_union arg.
  tribe_polys <- fr %>%
    select(geoid_native) %>%
    inner_join(tribe_geoids, by = "geoid_native", relationship = "many-to-many") %>%
    group_by(tribe_name) %>%
    summarize(.groups = "drop")

  landless_names <- setdiff(tribe_attr$tribe_name, tribe_polys$tribe_name)
  landless_sf <- sf::st_sf(
    tribe_name = landless_names,
    geometry = sf::st_sfc(
      rep(list(sf::st_multipolygon()), length(landless_names)), crs = target_crs))

  tribes_sf <- bind_rows(tribe_polys, landless_sf) %>%
    left_join(tribe_attr, by = "tribe_name")

  ## -- 6. tigris polygons with no federally recognized tribe -------------------
  no_tribe_sf <- fr %>%
    filter(!geoid_native %in% tribe_geoids$geoid_native) %>%
    transmute(
      tribe_name = NA_character_,
      bia_region = NA_character_,
      bia_alaska_native = NA,
      record_type = "tigris_no_tribe",
      has_polygon = NA,
      n_tigris_polygons = 1L,
      tigris_geoids = geoid_native,
      tigris_names = name,
      tigris_namelsad = namelsad,
      resolution = "tigris_no_tribe",
      confidence = NA_character_,
      note = NA_character_)

  result <- bind_rows(tribes_sf, no_tribe_sf) %>%
    sf::st_make_valid() %>%
    select(
      tribe_name, bia_region, bia_alaska_native, record_type, has_polygon,
      n_tigris_polygons, tigris_geoids, tigris_names, tigris_namelsad,
      resolution, confidence, note, geometry)

  ## -- 7. Validate coverage & report -------------------------------------------
  n_tribes <- sum(result$record_type == "federally_recognized_tribe")
  n_with <- sum(result$record_type == "federally_recognized_tribe" &
                  result$has_polygon, na.rm = TRUE)
  n_landless <- n_tribes - n_with
  n_no_tribe <- sum(result$record_type == "tigris_no_tribe")

  ## The 2026 BIA list resolves to exactly 573 substantive Tribal entities;
  ## any other count means the roster or the cross-reference folding drifted.
  if (n_tribes != 573) {
    stop("build_indigenous_tribe_polygons: resolved ", n_tribes,
         " federally recognized tribes; expected 573 (BIA 2026 list).",
         call. = FALSE)
  }

  ## No federal_rows polygon should be lost: each geoid is either assigned to a
  ## tribe or surfaced as tigris_no_tribe.
  assigned_geoids <- unique(tribe_geoids$geoid_native)
  no_tribe_geoids <- no_tribe_sf$tigris_geoids
  lost <- setdiff(fr_attr$geoid_native, c(assigned_geoids, no_tribe_geoids))
  if (length(lost) > 0) {
    warning("build_indigenous_tribe_polygons: ", length(lost),
            " federal_rows geoid(s) neither assigned nor flagged: ",
            str_c(lost, collapse = ", "), call. = FALSE)
  }

  ## Shared polygons (assigned to >1 tribe) are expected (shared OTSAs / Wind
  ## River); report them so they can be inspected.
  shared <- tribe_geoids %>%
    left_join(name_to_geoid, by = "geoid_native") %>%
    summarize(.by = c(geoid_native, name),
              tribes = str_c(sort(unique(tribe_name)), collapse = ", "),
              n = n_distinct(tribe_name)) %>%
    filter(n > 1) %>%
    arrange(desc(n))

  message(
    "build_indigenous_tribe_polygons: ", n_tribes, " federally recognized ",
    "tribes (", n_with, " with a polygon, ", n_landless, " landless); ",
    n_no_tribe, " tigris_no_tribe polygon(s); ",
    nrow(shared), " polygon(s) shared across multiple tribes.")
  if (nrow(shared) > 0) {
    message("  shared polygons: ",
            str_c(shared$name, " (", shared$n, ")", collapse = "; "))
  }

  result
}


#' Hand-curated BIA <-> tigris::native_areas crosswalk
#'
#' Resolves the `bia_only` / `tigris_only` records the token-Jaccard first pass
#' in `compare_bia_to_tigris_native_areas()` leaves unmatched. Every `bia_only`
#' BIA Federal Register name appears as a `bia_name`; every `tigris_only`
#' Census `native_areas()` NAME appears as a `tigris_name`. See
#' `bia_tigris_match_corrections` (above) for the audited fixes to WRONG matches
#' the first pass *did* make. Promoted here (from the test harness) so the
#' Feature 1 universe can source it.
#'
#' Columns: `bia_name`, `tigris_name`, `relationship`
#' (`same_entity` | `component_area` | `shared_otsa` | `bia_no_polygon` |
#' `tigris_no_tribe`), `has_polygon`, `confidence`, `note`.
get_bia_tigris_native_areas_crosswalk <- function() {
  tibble::tribble(
    ~bia_name, ~tigris_name, ~relationship, ~has_polygon, ~confidence, ~note,
    "Aleut Community of St. Paul Island (See Pribilof Islands Aleut Communities of St. Paul & St. George Islands) (previously listed as Saint Paul Island ( See Pribilof Islands Aleut Communities of St. Paul & St. George Islands))", "St. Paul", "same_entity", TRUE, "high", "St. Paul ANVSA (Pribilofs) = Aleut Community of St. Paul Island; the BIA entry fuzzy-matched the St. George ANVSA instead, leaving St. Paul unclaimed.",
    "Asa'carsarmiut Tribe", "Mountain Village", "same_entity", TRUE, "high", "Asa'carsarmiut Tribe is the federally recognized government of Mountain Village (lower Yukon); Mountain Village ANVSA is its area.",
    "Cheesh-Na Tribe", "Chistochina", "same_entity", TRUE, "high", "Cheesh-Na Tribe (formerly Native Village of Chistochina) governs the Chistochina ANVSA.",
    "Curyung Tribal Council", "Dillingham", "same_entity", TRUE, "high", "Dillingham ANVSA is governed by the Curyung Tribal Council.",
    "Flandreau Santee Sioux Tribe of South Dakota", "Flandreau", "same_entity", TRUE, "high", "Flandreau Reservation = Flandreau Santee Sioux Tribe (SD).",
    "Iowa Tribe of Kansas and Nebraska", "Iowa (KS-NE)", "same_entity", TRUE, "high", "Iowa (KS-NE) Reservation = Iowa Tribe of Kansas and Nebraska.",
    "Keweenaw Bay Indian Community, Michigan", "L'Anse", "same_entity", TRUE, "high", "Keweenaw Bay Indian Community's principal land base is the L'Anse Reservation (MI). (Also governs Ontonagon - see component_area.)",
    "Kickapoo Traditional Tribe of Texas", "Kickapoo (TX)", "same_entity", TRUE, "high", "Kickapoo (TX) Reservation = Kickapoo Traditional Tribe of Texas.",
    "Kickapoo Tribe of Indians of the Kickapoo Reservation in Kansas", "Kickapoo (KS)", "same_entity", TRUE, "high", "Kickapoo (KS) Reservation = Kickapoo Tribe of Indians of the Kickapoo Reservation in Kansas.",
    "Lower Brule Sioux Tribe of the Lower Brule Reservation, South Dakota", "Lower Brule", "same_entity", TRUE, "high", "Lower Brule Reservation = Lower Brule Sioux Tribe (SD).",
    "Manchester Band of Pomo Indians of the Manchester Rancheria, California", "Manchester-Point Arena", "same_entity", TRUE, "high", "Manchester Band of Pomo = Manchester-Point Arena Rancheria (Mendocino Co.); BIA was formerly '...Manchester-Point Arena Rancheria'.",
    "Mentasta Traditional Council", "Mentasta Lake", "same_entity", TRUE, "high", "Mentasta Lake ANVSA is governed by the Mentasta Traditional Council.",
    "Mi'kmaq Nation (previously listed as Aroostook Band of Micmacs)", "Aroostook Band of Micmac", "same_entity", TRUE, "high", "Mi'kmaq Nation is the renamed Aroostook Band of Micmacs; its trust land is the Aroostook Band of Micmac Trust Land (ME). Census keeps the former name.",
    "Native Village of Chuathbaluk (Russian Mission, Kuskokwim)", "Chuathbaluk", "same_entity", TRUE, "high", "Chuathbaluk ANVSA = Native Village of Chuathbaluk (Russian Mission, Kuskokwim).",
    "Northfork Rancheria of Mono Indians of California", "North Fork", "same_entity", TRUE, "high", "Same entity; BIA spells 'Northfork', Census 'North Fork' Rancheria (Madera Co.).",
    "Nunakauyarmiut Tribe", "Toksook Bay", "same_entity", TRUE, "high", "Nunakauyarmiut Tribe (Nunakauyak Traditional Council) governs Toksook Bay, Nelson Island.",
    "Oglala Sioux Tribe", "Pine Ridge", "same_entity", TRUE, "high", "Oglala Sioux Tribe governs the Pine Ridge Reservation (SD).",
    "Oneida Nation", "Oneida (WI)", "same_entity", TRUE, "high", "Oneida (WI) Reservation is the Oneida Nation (Wisconsin). (Distinct from the NY 'Oneida Indian Nation'.)",
    "Orutsararmiut Traditional Native Council", "Bethel", "same_entity", TRUE, "high", "Bethel ANVSA is governed by the Orutsararmiut Traditional Native Council (tribe matched elsewhere, leaving this polygon unclaimed).",
    "Paiute Indian Tribe of Utah (Cedar Band of Paiutes, Kanosh Band of Paiutes, Koosharem Band of Paiutes, Indian Peaks Band of Paiutes, and Shivwits Band of Paiutes)", "Paiute (UT)", "same_entity", TRUE, "high", "Paiute Indian Tribe of Utah governs the Paiute (UT) Reservation (its five bands' scattered parcels).",
    "Ponca Tribe of Nebraska", "Ponca (NE)", "same_entity", TRUE, "high", "Ponca (NE) Trust Land = Ponca Tribe of Nebraska.",
    "PuliklaTribe of Yurok People (previously listed as Resighini Rancheria, California)", "Resighini", "same_entity", TRUE, "high", "Resighini Rancheria (Del Norte Co.) was renamed the Pulikla Tribe of Yurok People (2024). NB: BIA string has a typo ('PuliklaTribe', no space).",
    "Ramona Band of Cahuilla, California", "Ramona", "same_entity", TRUE, "high", "Ramona Village (Riverside Co.) is governed by the Ramona Band of Cahuilla.",
    "Red Cliff Band of Lake Superior Chippewa Indians of Wisconsin", "Red Cliff", "same_entity", TRUE, "high", "Red Cliff Reservation = Red Cliff Band of Lake Superior Chippewa (WI).",
    "Sac & Fox Nation, Oklahoma", "Sac and Fox", "same_entity", TRUE, "high", "Sac and Fox OTSA (OK) = Sac & Fox Nation, Oklahoma.",
    "Sac & Fox Tribe of the Mississippi in Iowa", "Sac and Fox/Meskwaki", "same_entity", TRUE, "high", "Sac and Fox/Meskwaki Settlement (IA) = Sac & Fox Tribe of the Mississippi in Iowa (Meskwaki Nation).",
    "Saginaw Chippewa Indian Tribe of Michigan", "Isabella", "same_entity", TRUE, "high", "Saginaw Chippewa Indian Tribe governs the Isabella Reservation (MI).",
    "Tangirnaq Native Village", "Lesnoi", "same_entity", TRUE, "high", "Tangirnaq Native Village (formerly Lesnoi Village / Woody Island) governs the Leisnoi/Lesnoi ANVSA; village now depopulated but the area persists.",
    "Timbisha Shoshone Tribe", "Timbi-Sha Shoshone", "same_entity", TRUE, "high", "Same federally recognized entity (Death Valley); BIA 'Timbisha', Census 'Timbi-Sha' Shoshone.",
    "Tolowa Dee-ni' Nation", "Smith River", "same_entity", TRUE, "high", "Tolowa Dee-ni' Nation was formerly the Smith River Rancheria (Del Norte Co.).",
    "Wiyot Tribe, California", "Table Bluff", "same_entity", TRUE, "high", "Wiyot Tribe is the Table Bluff Reservation (Humboldt Co.); BIA was 'previously listed as Table Bluff Reservation - Wiyot Tribe'.",
    "Yocha Dehe Wintun Nation, California", "Rumsey", "same_entity", TRUE, "high", "Yocha Dehe Wintun Nation was renamed from the Rumsey Indian Rancheria (Yolo Co.) in 2009.",
    "Yupiit of Andreafski", "Andreafsky", "same_entity", TRUE, "high", "Yupiit of Andreafski (HQ St. Mary's) is the tribe for the historic Andreafsky settlement; Andreafsky ANVSA is its area.",
    "Capitan Grande Band of Diegueno Mission Indians of California (Barona Group of Capitan Grande Band of Mission Indians of the Barona Reservation, California; Viejas (Baron Long) Group of Capitan Grande Band of Mission Indians of the Viejas Reservation, California)", "Barona", "component_area", TRUE, "high", "Barona Reservation is governed by the Barona Group of the Capitan Grande Band of Diegueno Mission Indians (one BIA entry carries both reservation names).",
    "Capitan Grande Band of Diegueno Mission Indians of California (Barona Group of Capitan Grande Band of Mission Indians of the Barona Reservation, California; Viejas (Baron Long) Group of Capitan Grande Band of Mission Indians of the Viejas Reservation, California)", "Viejas", "component_area", TRUE, "high", "Viejas Reservation is governed by the Viejas Group of the Capitan Grande Band of Diegueno Mission Indians (one BIA entry carries both reservation names).",
    "Keweenaw Bay Indian Community, Michigan", "Ontonagon", "component_area", TRUE, "high", "Ontonagon Reservation is the Keweenaw Bay Indian Community's second land base (KBIC formed from the L'Anse and Ontonagon bands).",
    "Minnesota Chippewa Tribe, Minnesota (Six component reservations: Bois Forte Band (Nett Lake); Fond du Lac Band; Grand Portage Band; Leech Lake Band; Mille Lacs Band; White Earth Band)", "Bois Forte", "component_area", TRUE, "high", "Bois Forte is a component reservation of the Minnesota Chippewa Tribe; these bands are federally recognized only as MCT components, not independently.",
    "Minnesota Chippewa Tribe, Minnesota (Six component reservations: Bois Forte Band (Nett Lake); Fond du Lac Band; Grand Portage Band; Leech Lake Band; Mille Lacs Band; White Earth Band)", "Grand Portage", "component_area", TRUE, "high", "Grand Portage is a component reservation of the Minnesota Chippewa Tribe; these bands are federally recognized only as MCT components, not independently.",
    "Minnesota Chippewa Tribe, Minnesota (Six component reservations: Bois Forte Band (Nett Lake); Fond du Lac Band; Grand Portage Band; Leech Lake Band; Mille Lacs Band; White Earth Band)", "Leech Lake", "component_area", TRUE, "high", "Leech Lake is a component reservation of the Minnesota Chippewa Tribe; these bands are federally recognized only as MCT components, not independently.",
    "Minnesota Chippewa Tribe, Minnesota (Six component reservations: Bois Forte Band (Nett Lake); Fond du Lac Band; Grand Portage Band; Leech Lake Band; Mille Lacs Band; White Earth Band)", "Mille Lacs", "component_area", TRUE, "high", "Mille Lacs is a component reservation of the Minnesota Chippewa Tribe; these bands are federally recognized only as MCT components, not independently.",
    "Minnesota Chippewa Tribe, Minnesota (Six component reservations: Bois Forte Band (Nett Lake); Fond du Lac Band; Grand Portage Band; Leech Lake Band; Mille Lacs Band; White Earth Band)", "Minnesota Chippewa", "component_area", TRUE, "high", "Minnesota Chippewa is a component reservation of the Minnesota Chippewa Tribe; these bands are federally recognized only as MCT components, not independently.",
    "Minnesota Chippewa Tribe, Minnesota (Six component reservations: Bois Forte Band (Nett Lake); Fond du Lac Band; Grand Portage Band; Leech Lake Band; Mille Lacs Band; White Earth Band)", "White Earth", "component_area", TRUE, "high", "White Earth is a component reservation of the Minnesota Chippewa Tribe; these bands are federally recognized only as MCT components, not independently.",
    "Passamaquoddy Tribe", "Indian Township", "component_area", TRUE, "high", "Indian Township Reservation (ME) is one of the two Passamaquoddy Tribe reservations.",
    "Passamaquoddy Tribe", "Pleasant Point", "component_area", TRUE, "high", "Pleasant Point Reservation (ME) is one of the two Passamaquoddy Tribe reservations.",
    "Seminole Tribe of Florida", "Big Cypress", "component_area", TRUE, "high", "Big Cypress is one of the Seminole Tribe of Florida reservations/trust lands (the Miccosukee Tribe's areas are separate).",
    "Seminole Tribe of Florida", "Brighton", "component_area", TRUE, "high", "Brighton is one of the Seminole Tribe of Florida reservations/trust lands (the Miccosukee Tribe's areas are separate).",
    "Seminole Tribe of Florida", "Coconut Creek", "component_area", TRUE, "high", "Coconut Creek is one of the Seminole Tribe of Florida reservations/trust lands (the Miccosukee Tribe's areas are separate).",
    "Seminole Tribe of Florida", "Fort Pierce", "component_area", TRUE, "high", "Fort Pierce is one of the Seminole Tribe of Florida reservations/trust lands (the Miccosukee Tribe's areas are separate).",
    "Seminole Tribe of Florida", "Hollywood", "component_area", TRUE, "high", "Hollywood is one of the Seminole Tribe of Florida reservations/trust lands (the Miccosukee Tribe's areas are separate).",
    "Seminole Tribe of Florida", "Immokalee", "component_area", TRUE, "high", "Immokalee is one of the Seminole Tribe of Florida reservations/trust lands (the Miccosukee Tribe's areas are separate).",
    "Seminole Tribe of Florida", "Seminole (FL)", "component_area", TRUE, "high", "Seminole (FL) is one of the Seminole Tribe of Florida reservations/trust lands (the Miccosukee Tribe's areas are separate).",
    "Seminole Tribe of Florida", "Tampa", "component_area", TRUE, "high", "Tampa is one of the Seminole Tribe of Florida reservations/trust lands (the Miccosukee Tribe's areas are separate).",
    "Seneca Nation of Indians", "Allegany", "component_area", TRUE, "high", "Allegany Reservation (NY) is governed by the Seneca Nation of Indians; the BIA entry fuzzy-matched the unrelated 'Seneca-Cayuga' OTSA, leaving these unclaimed. (Oil Springs historically joint with the Tonawanda Band.)",
    "Seneca Nation of Indians", "Cattaraugus", "component_area", TRUE, "high", "Cattaraugus Reservation (NY) is governed by the Seneca Nation of Indians; the BIA entry fuzzy-matched the unrelated 'Seneca-Cayuga' OTSA, leaving these unclaimed. (Oil Springs historically joint with the Tonawanda Band.)",
    "Seneca Nation of Indians", "Oil Springs", "component_area", TRUE, "high", "Oil Springs Reservation (NY) is governed by the Seneca Nation of Indians; the BIA entry fuzzy-matched the unrelated 'Seneca-Cayuga' OTSA, leaving these unclaimed. (Oil Springs historically joint with the Tonawanda Band.)",
    "Te-Moak Tribe of Western Shoshone Indians of Nevada (Four constituent bands: Battle Mountain Band; Elko Band; South Fork Band; and Wells Band)", "Elko", "component_area", TRUE, "high", "Elko is one of the four constituent bands of the Te-Moak Tribe of Western Shoshone (named in the BIA title).",
    "Te-Moak Tribe of Western Shoshone Indians of Nevada (Four constituent bands: Battle Mountain Band; Elko Band; South Fork Band; and Wells Band)", "South Fork", "component_area", TRUE, "high", "South Fork is one of the four constituent bands of the Te-Moak Tribe of Western Shoshone (named in the BIA title).",
    "Te-Moak Tribe of Western Shoshone Indians of Nevada (Four constituent bands: Battle Mountain Band; Elko Band; South Fork Band; and Wells Band)", "Wells", "component_area", TRUE, "high", "Wells is one of the four constituent bands of the Te-Moak Tribe of Western Shoshone (named in the BIA title).",
    "Washoe Tribe of Nevada & California (Carson Colony, Dresslerville Colony, Woodfords Community, Stewart Community, & Washoe Ranches)", "Carson", "component_area", TRUE, "high", "Carson is a constituent colony/community of the Washoe Tribe of Nevada & California (named in the BIA title).",
    "Washoe Tribe of Nevada & California (Carson Colony, Dresslerville Colony, Woodfords Community, Stewart Community, & Washoe Ranches)", "Dresslerville", "component_area", TRUE, "high", "Dresslerville is a constituent colony/community of the Washoe Tribe of Nevada & California (named in the BIA title).",
    "Washoe Tribe of Nevada & California (Carson Colony, Dresslerville Colony, Woodfords Community, Stewart Community, & Washoe Ranches)", "Stewart", "component_area", TRUE, "high", "Stewart is a constituent colony/community of the Washoe Tribe of Nevada & California (named in the BIA title).",
    "Washoe Tribe of Nevada & California (Carson Colony, Dresslerville Colony, Woodfords Community, Stewart Community, & Washoe Ranches)", "Woodfords", "component_area", TRUE, "high", "Woodfords is a constituent colony/community of the Washoe Tribe of Nevada & California (named in the BIA title).",
    "Yerington Paiute Tribe of the Yerington Colony & Campbell Ranch, Nevada", "Yerington", "component_area", TRUE, "high", "Yerington Colony is a component of the Yerington Paiute Tribe (its Campbell Ranch parcel matched separately).",
    "Alabama-Quassarte Tribal Town", "Creek", "shared_otsa", TRUE, "high", "Oklahoma Creek tribal town located within the Muscogee (Creek) Nation OTSA ('Creek'); has no separate OTSA polygon of its own.",
    "Apache Tribe of Oklahoma", "Kiowa-Comanche-Apache-Fort Sill Apache", "shared_otsa", TRUE, "high", "Shares the Kiowa-Comanche-Apache-Fort Sill Apache OTSA (with the Kiowa, Comanche, Apache Tribe of OK, and Fort Sill Apache).",
    "Caddo Nation of Oklahoma", "Caddo-Wichita-Delaware", "shared_otsa", TRUE, "high", "Shares the Caddo-Wichita-Delaware OTSA (with the Caddo Nation, Wichita and Affiliated Tribes, and Delaware Nation). NB: the polygon fuzzy-matched 'Delaware Tribe of Indians', a different tribe.",
    "Citizen Potawatomi Nation, Oklahoma", "Citizen Potawatomi Nation-Absentee Shawnee", "shared_otsa", TRUE, "high", "Shares the Citizen Potawatomi Nation-Absentee Shawnee OTSA with the Absentee-Shawnee Tribe.",
    "Comanche Nation, Oklahoma", "Kiowa-Comanche-Apache-Fort Sill Apache", "shared_otsa", TRUE, "high", "Shares the Kiowa-Comanche-Apache-Fort Sill Apache OTSA (with the Kiowa, Comanche, Apache Tribe of OK, and Fort Sill Apache).",
    "Delaware Nation, Oklahoma", "Caddo-Wichita-Delaware", "shared_otsa", TRUE, "high", "Shares the Caddo-Wichita-Delaware OTSA (with the Caddo Nation, Wichita and Affiliated Tribes, and Delaware Nation). NB: the polygon fuzzy-matched 'Delaware Tribe of Indians', a different tribe.",
    "Kialegee Tribal Town", "Creek", "shared_otsa", TRUE, "high", "Oklahoma Creek tribal town located within the Muscogee (Creek) Nation OTSA ('Creek'); has no separate OTSA polygon of its own.",
    "Kiowa Tribe (previously listed as Kiowa Indian Tribe of Oklahoma)", "Kiowa-Comanche-Apache-Fort Sill Apache", "shared_otsa", TRUE, "high", "Shares the Kiowa-Comanche-Apache-Fort Sill Apache OTSA (with the Kiowa, Comanche, Apache Tribe of OK, and Fort Sill Apache).",
    "Thlopthlocco Tribal Town", "Creek", "shared_otsa", TRUE, "high", "Oklahoma Creek tribal town located within the Muscogee (Creek) Nation OTSA ('Creek'); has no separate OTSA polygon of its own.",
    "Wichita and Affiliated Tribes (Wichita, Keechi, Waco, & Tawakonie), Oklahoma", "Caddo-Wichita-Delaware", "shared_otsa", TRUE, "high", "Shares the Caddo-Wichita-Delaware OTSA (with the Caddo Nation, Wichita and Affiliated Tribes, and Delaware Nation). NB: the polygon fuzzy-matched 'Delaware Tribe of Indians', a different tribe.",
    "Buena Vista Rancheria of Me-Wuk Indians of California", NA_character_, "bia_no_polygon", FALSE, "medium", "Hardwick-restored Amador Co. rancheria with modern trust land, but no federal AIANNH polygon in the tigris(2024) universe.",
    "California Valley Miwok Tribe, California", NA_character_, "bia_no_polygon", FALSE, "medium", "Formerly Sheep Ranch Rancheria; presently landless with no trust land/polygon.",
    "Cloverdale Rancheria of Pomo Indians of California", NA_character_, "bia_no_polygon", FALSE, "high", "Hardwick-restored (1983) but landless (fee land still in fee-to-trust); no polygon.",
    "Federated Indians of Graton Rancheria, California", NA_character_, "bia_no_polygon", FALSE, "medium", "Restored 2000 with a casino trust parcel; a Census AIANNH code exists but no polygon appears in the tigris(2024) federal non-Hawaiian universe.",
    "Kaguyak Village", NA_character_, "bia_no_polygon", FALSE, "high", "Kaguyak (Kodiak I.) destroyed by the 1964 tsunami and never resettled; tribe HQ Akhiok, no ANVSA of its own.",
    "King Island Native Community", NA_character_, "bia_no_polygon", FALSE, "high", "King Islanders relocated to Nome by ~1970; tribe operates from Nome with no separate ANVSA.",
    "Koi Nation of Northern California", NA_character_, "bia_no_polygon", FALSE, "high", "Landless; its sole trust acquisition (Shiloh/Sonoma) was vacated and removed from trust in 2026; no polygon.",
    "Little Shell Tribe of Chippewa Indians of Montana", NA_character_, "bia_no_polygon", FALSE, "high", "Federally recognized Dec 2019 and landless at the time; no Census AIANNH polygon in this vintage.",
    "Lumbee Tribe of North Carolina (See Supplementary Information supra, noting conditions on the Tribe's eligibility for Federal services)", NA_character_, "bia_no_polygon", FALSE, "high", "Recognized with conditions on federal services and holds no federal trust land; no federal AIANNH polygon.",
    "Monacan Indian Nation", NA_character_, "bia_no_polygon", FALSE, "high", "Recognized via the 2018 Thomasina Jordan Act with no reservation; land-into-trust only authorized; no polygon.",
    "Nansemond Indian Nation", NA_character_, "bia_no_polygon", FALSE, "high", "Recognized via the 2018 Thomasina Jordan Act; no reservation (federal or state); no polygon.",
    "Native Village of Afognak", NA_character_, "bia_no_polygon", FALSE, "high", "Afognak destroyed in the 1964 quake; survivors founded Port Lions (separate tribe/ANVSA); Afognak has no ANVSA.",
    "Native Village of Kanatak", NA_character_, "bia_no_polygon", FALSE, "high", "Kanatak abandoned mid-20th c.; landless tribe HQ Wasilla, no ANVSA.",
    "Native Village of Unga", NA_character_, "bia_no_polygon", FALSE, "medium", "Unga (Popof/Unga I.) vacated by 1969 (to Sand Point); an uninhabited Unga ANVSA exists but is absent from this tigris universe.",
    "Pamunkey Indian Tribe", NA_character_, "bia_no_polygon", FALSE, "high", "Federally recognized 2016; its land base is a Virginia STATE reservation (Census aiannhr=='S'), excluded from this federal-only universe.",
    "Pauloff Harbor Village", NA_character_, "bia_no_polygon", FALSE, "medium", "Pauloff Harbor (Sanak I.) is a ghost town (members at Sand Point); an uninhabited ANVSA exists but is absent from this tigris universe.",
    "Potter Valley Tribe, California", NA_character_, "bia_no_polygon", FALSE, "high", "Hardwick-restored; the historic rancheria was terminated and the tribe remained landless; no polygon.",
    "San Juan Southern Paiute Tribe of Arizona", NA_character_, "bia_no_polygon", FALSE, "medium", "Its ~5,400-ac reservation within the Navajo Nation was only just established (Homelands Act); not yet a separate AIANNH polygon.",
    "Scotts Valley Band of Pomo Indians of California", NA_character_, "bia_no_polygon", FALSE, "high", "Restored 1991/92 but landless; only trust decision (160 ac, Vallejo) is Jan 2025, post-dating the tigris(2024) universe.",
    "Shinnecock Indian Nation", NA_character_, "bia_no_polygon", FALSE, "high", "Federally recognized 2010; its Southampton NY land base is a STATE reservation (aiannhr=='S'), excluded from this federal-only universe.",
    "Tejon Indian Tribe", NA_character_, "bia_no_polygon", FALSE, "high", "Reaffirmed 2012 after ~150 years landless; no AIANNH polygon in the tigris(2024) universe.",
    "Umkumiut Native Village", NA_character_, "bia_no_polygon", FALSE, "medium", "Umkumiut is a seasonal Nelson I. camp counted within Toksook Bay/Nightmute; no separate ANVSA.",
    "Upper Mattaponi Tribe", NA_character_, "bia_no_polygon", FALSE, "high", "Recognized via the 2018 Thomasina Jordan Act; holds no Virginia state reservation (unlike Pamunkey/Mattaponi); no polygon.",
    "Wilton Rancheria, California", NA_character_, "bia_no_polygon", FALSE, "medium", "Restored 2009 with a small Elk Grove trust parcel, but no polygon appears in the tigris(2024) universe.",
    NA_character_, "Canyon Village", "tigris_no_tribe", NA, "medium", "Canyon Village (Gwich'in, Yukon Flats) is not on the federally recognized list; ANVSA has no corresponding tribe.",
    NA_character_, "Celilo", "tigris_no_tribe", NA, "high", "Celilo Village is a multi-tribal Columbia R. treaty fishing village (Warm Springs/Yakama/Umatilla/Nez Perce residents); no single governing tribe.",
    NA_character_, "Lake Minchumina", "tigris_no_tribe", NA, "medium", "Minchumina Natives Inc. (ANCSA) was denied Native-village status; ANVSA has no federally recognized tribe.",
    NA_character_, "Port Alsworth", "tigris_no_tribe", NA, "medium", "Port Alsworth (Lake Clark) is a predominantly non-Native community; ANVSA has no corresponding tribe.",
    NA_character_, "Uyak", "tigris_no_tribe", NA, "low", "Uyak (Uyak Bay, Kodiak) maps to the Uyak Natives ANCSA corporation; the nearest tribe, Native Village of Larsen Bay, governs the separate Larsen Bay ANVSA - no distinct tribe for Uyak.")
}


#' Assemble the one-polygon-per-tribe universe end to end
#'
#' Convenience orchestrator that chains the three steps the test harness wires
#' together: build the federal tigris rows, reconcile them against the BIA
#' Federal Register list, and fold in the crosswalk + audited corrections. This
#' is the single entry point Feature 1 (and the test harness) call to obtain the
#' federally-recognized-tribe polygon universe.
#'
#' @param year Integer. TIGER vintage. Default `2024`.
#' @return The `sf` tibble returned by `build_indigenous_tribe_polygons()`:
#'   one row per federally recognized tribe plus the `tigris_no_tribe` polygons.
#'   `tribe_name` is the verbatim BIA Federal Register string, including the
#'   FR typo "PuliklaTribe of Yurok People ..." (no space). Every name-keyed
#'   match downstream keys on that verbatim string; the display fix to
#'   "Pulikla Tribe" is applied once in `get_indigenous_universe()`, after
#'   the last of those matches.
assemble_indigenous_tribe_polygons <- function(year = 2024) {
  federal_rows <- get_federal_tribe_rows(year = year)

  tigris_entities_universe <- federal_rows %>%
    sf::st_drop_geometry() %>%
    as_tibble() %>%
    summarize(
      .by = name,
      n_tigris_polygons = n(),
      tigris_namelsad = str_c(sort(unique(namelsad)), collapse = "; "),
      home_state = first(home_state)) %>%
    rename(tigris_name = name)

  reconciliation <- compare_bia_to_tigris_native_areas(
    tigris_entities = tigris_entities_universe)

  build_indigenous_tribe_polygons(
    reconciliation = reconciliation,
    federal_rows = federal_rows,
    crosswalk = get_bia_tigris_native_areas_crosswalk())
}

