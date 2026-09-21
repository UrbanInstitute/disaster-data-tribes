## ---------------------------------------------------------------------------
## 08_get_feature1_data_visualization_codebook.R — codebook for the data-viz deliverable
##
## get_feature1_data_visualization_codebook() returns the variable/definition
## table for the dataset built by 06_build_feature1_data_visualization.R;
## _feature1.qmd uses it to verify that exported columns match their docs.
## ---------------------------------------------------------------------------
library(tidyverse)

#' Codebook for the feature-1 data-visualization dataframe
#'
#' Returns a two-column tibble describing every variable produced by
#' `build_feature1_data_visualization()`. Column order matches the
#' canonical order in `_feature1.qmd::final_columns_data_visualization`.
#'
#' @return A tibble with columns `variable` and `definition`.
get_feature1_data_visualization_codebook <- function() {
  tibble::tribble(
    ~variable, ~definition,
    "community_id", "Unique identifier for the Indigenous community.",
    "community_name", "Name of the community. Alaska Native Regional Corporation rows carry the suffix \"Alaska Native Regional Corporation\" (e.g., \"Ahtna Alaska Native Regional Corporation\").",
    "community_name_short", "Shortened community name (nine words or fewer where possible) for narrow/mobile displays; equals `community_name` when it is already short enough. Alaska Native Regional Corporation rows keep the bare corporation name here (no entity-type suffix).",
    "disaster_id", "FEMA disaster number.",
    "disaster_title", "FEMA declaration title, display-formatted (lower case except proper nouns; named storms title-cased).",
    "disaster_year_declared", "Calendar year the disaster was declared.",
    "disaster_type", "FEMA incident type, lower-cased (e.g., 'hurricane', 'wildfire'; 'Mud/Landslide' rendered as 'mudslide or landslide').",
    "disaster_designated_area", "Area covered by the declaration relative to the community: 'Statewide declaration' for statewide-tier matches; the FEMA tribal designated area with ' (Tribal area)' appended (parenthetical qualifiers removed) for direct-tier matches; or up to three intersecting county-equivalents joined with '; ', each carrying its full legal name and descriptor per TIGER ('Terrebonne Parish, LA', 'Denali Borough, AK', 'Oglala Lakota County, SD'; independent cities' lowercase 'city' is capitalized, e.g. 'Bedford City, VA'). Names reflect the current (2024) TIGER vintage, not declaration-era county names.",
    "disaster_lead_applicant", "Entity that filed the declaration: the community name when tribally-filed; 'State of <name>' for the 50 states; else the bare territory name (e.g., 'Guam').")
}
