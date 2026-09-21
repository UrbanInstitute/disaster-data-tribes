## ---------------------------------------------------------------------------
## format_disaster_name.R — display formatting for FEMA declaration titles
##
## format_disaster_name(): turns FEMA's upper-case `declaration_title` into a
## display string (lower case except proper nouns; named storms title-cased),
## resolving non-storm proper nouns via the curated override table below.
## Sourced by 05_build_feature1_search.R and
## 06_build_feature1_data_visualization.R.
## ---------------------------------------------------------------------------
library(tidyverse)

## Curated proper-noun overrides --------------------------------------------
## FEMA `declaration_title` values are stored in upper case. The heuristic in
## `format_disaster_name()` title-cases named storms by pattern and lowercases
## everything else, but it cannot infer non-storm proper nouns (named fires,
## volcanoes, places, events). Those are resolved exactly via this table, keyed
## on the squished, trailing-punctuation-stripped raw title (matched
## case-insensitively). Source characters are reproduced verbatim — e.g. FEMA
## prints "EL NINO" without the tilde, so the display keeps "El Nino".
disaster_name_overrides <- tibble::tribble(
  ~raw, ~display,
  "ASPEN FIRE", "Aspen Fire",
  "BLACK FOREST WILDFIRE", "Black Forest Wildfire",
  "COALINGA EARTHQUAKE", "Coalinga Earthquake",
  "FIRE (CITY OF CHELSEA)", "fire (city of Chelsea)",
  "FIRE (LOS ANGELES COUNTY)", "fire (Los Angeles County)",
  "FREEDOM AND NOBLE WILDFIRES", "Freedom and Noble Wildfires",
  "HIGH PARK AND WALDO CANYON WILDFIRES", "High Park and Waldo Canyon Wildfires",
  "KILAUEA VOLCANIC ERUPTION AND EARTHQUAKES", "Kilauea Volcanic Eruption and earthquakes",
  "LAVA FLOW, KILAUEA VOLCANO", "lava flow, Kilauea Volcano",
  "LOMA PRIETA EARTHQUAKE", "Loma Prieta Earthquake",
  "NORTHRIDGE EARTHQUAKE", "Northridge Earthquake",
  "OAKLAND HILLS FIRE", "Oakland Hills Fire",
  "OLD GULCH & FOUNTAIN FIRES", "Old Gulch & Fountain Fires",
  "PU'U O'O VOLCANIC ERUPTION AND LAVA FLOW", "Pu'u O'o Volcanic Eruption and lava flow",
  "RICHARD SPRING FIRE", "Richard Spring Fire",
  "RIM FIRE", "Rim Fire",
  "ROYAL GORGE FIRE", "Royal Gorge Fire",
  "SAN FERNANDO EARTHQUAKE", "San Fernando Earthquake",
  "SOUTH FORK FIRE, SALT FIRE, AND FLOODING", "South Fork Fire, Salt Fire, and flooding",
  "VALLEY FIRE AND BUTTE FIRE", "Valley Fire and Butte Fire",
  "VOLCANIC ERUPTION, MT. ST. HELENS", "volcanic eruption, Mt. St. Helens",
  "WATCH FIRE", "Watch Fire",
  "EXPLOSION AT FEDERAL COURTHOUSE IN OKLAHOMA CITY", "explosion at federal courthouse in Oklahoma City",
  "EXPLOSION AT WORLD TRADE CENTER", "explosion at World Trade Center",
  "THE EL NINO (THE SALMON INDUSTRY)", "the El Nino (the salmon industry)")

#' Human-readable display formatting for FEMA declaration titles
#'
#' FEMA `declaration_title` values are stored in upper case (e.g.
#' `"HURRICANE HELENE"`, `"SEVERE STORMS, FLOODING, AND MUDSLIDES"`). This
#' renders them for display under the rule "lowercase everything except proper
#' nouns":
#' * **Named storms** (`Hurricane <Name>`, `Typhoon <Name>`,
#'   `Tropical Storm <Name>`, `Tropical Depression <Name>`, `Super Typhoon
#'   <Name>`, `Post-Tropical Storm <Name>`, `(Potential) Tropical Cyclone
#'   <Name>`) are title-cased by pattern, including inside constructions like
#'   `"remnants of Typhoon Halong"`. A bare type word with no following name
#'   (e.g. `"HURRICANE & FLOOD"`) stays lower case.
#' * **Other proper nouns** (named fires, volcanoes, places, events) are
#'   resolved exactly through `disaster_name_overrides`.
#' * **Everything else** is lower-cased.
#'
#' Trailing whitespace and a trailing period are stripped; internal whitespace
#' is squished. Source characters are reproduced verbatim — the function never
#' introduces or removes non-ASCII (apostrophes, accents, em dashes, etc.).
#'
#' @param x Character vector of raw FEMA declaration titles, any case. `NA` in
#'   yields `NA` out.
#' @return Character vector of display-formatted titles.
format_disaster_name <- function(x) {
  storm_pat <- stringr::regex(stringr::str_c(
    "\\b(super typhoon|post-tropical storm|potential tropical cyclone|",
    "tropical cyclone|tropical storm|tropical depression|hurricane|typhoon)",
    "\\b\\s+([a-z][a-z'-]+)"))

  raw_norm <- x %>% str_squish() %>% str_remove("[.\\s]+$") %>% str_squish()
  override <- disaster_name_overrides$display[
    match(str_to_upper(raw_norm), str_to_upper(disaster_name_overrides$raw))]

  ## Title-case each "<type> <name>" storm construct found in the lower-cased
  ## title; leave the rest lower case.
  cased <- purrr::map_chr(str_to_lower(raw_norm), function(s) {
    if (is.na(s)) return(NA_character_)
    matches <- str_match_all(s, storm_pat)[[1]]
    for (i in seq_len(nrow(matches))) {
      name_cased <- matches[i, 3] %>%
        str_to_title() %>%
        str_replace_all("'([A-Z])", function(m) str_to_lower(m))
      s <- str_replace(
        s, fixed(matches[i, 1]),
        str_c(str_to_title(matches[i, 2]), " ", name_cased))
    }
    s
  })

  coalesce(override, cased)
}
