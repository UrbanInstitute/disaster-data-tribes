## ---------------------------------------------------------------------------
## 07_get_feature1_search_codebook.R — codebook for the search deliverable
##
## get_feature1_search_codebook() returns the variable/definition table for the
## dataset built by 05_build_feature1_search.R; _feature1.qmd uses it to verify
## that the exported columns match their documentation.
## ---------------------------------------------------------------------------
library(tidyverse)

#' Codebook for the feature-1 search dataframe
#'
#' Returns a two-column tibble describing every variable produced by
#' `build_feature1_search()`. Column order matches the canonical order
#' in `_feature1.qmd::final_columns_search`.
#'
#' @return A tibble with columns `variable` and `definition`.
get_feature1_search_codebook <- function() {
  tibble::tribble(
    ~variable, ~definition,
    "community_id", "Unique identifier for the Indigenous community.",
    "community_name",  "Name of the community. Alaska Native Regional Corporation rows carry the suffix \"Alaska Native Regional Corporation\" (e.g., \"Ahtna Alaska Native Regional Corporation\").",
    "community_name_short", "Shortened community name (nine words or fewer where possible) for narrow/mobile displays; equals `community_name` when it is already short enough. Alaska Native Regional Corporation rows keep the bare corporation name here (no entity-type suffix).",
    "community_state", "Primary state in which the community is located (postal abbreviation).",
    "community_edr", "EDA Economic Development Representative (EDR) for the state (semicolon-joined when a state has multiple).",
    "community_edr_email", "Email address of the EDA Economic Development Representative (semicolon-joined when a state has multiple; order matches `community_edr`).",
    "eda_factor_eligibility", "EDA disaster supplemental eligibility: 'yes' if at least one FEMA major disaster (DR) declaration dated calendar year 2023 or 2024 reaches the community by any path (named tribal-direct, county spatial overlap, or statewide); 'no' otherwise. The 2023-2024 window is specific to the EDA disaster supplemental; the HMGP and SBA columns apply their own windows.",
    "eda_declaration_name_1", "Name of the most-recent declaration in the 2023-2024 EDA window that reaches the community (slot 1).",
    "eda_declaration_name_2", "Name of the second most-recent declaration in the 2023-2024 EDA window (slot 2).",
    "eda_declaration_name_3", "Name of the third most-recent declaration in the 2023-2024 EDA window (slot 3).",
    "eda_declaration_year_1", "Calendar year of the slot-1 declaration.",
    "eda_declaration_year_2", "Calendar year of the slot-2 declaration.",
    "eda_declaration_year_3", "Calendar year of the slot-3 declaration.",
    "eda_declaration_disaster_number_1", "FEMA disaster number of the slot-1 declaration.",
    "eda_declaration_disaster_number_2", "FEMA disaster number of the slot-2 declaration.",
    "eda_declaration_disaster_number_3", "FEMA disaster number of the slot-3 declaration.",
    "eda_spatial_intersection_counties_1", "Up to three counties named in the slot-1 declaration that intersect the community, formatted 'County, ST' and joined with ';'.",
    "eda_spatial_intersection_counties_2", "Up to three counties named in the slot-2 declaration that intersect the community, formatted 'County, ST' and joined with ';'.",
    "eda_spatial_intersection_counties_3", "Up to three counties named in the slot-3 declaration that intersect the community, formatted 'County, ST' and joined with ';'.",
    "fema_hmgp_binary", "1 if potentially eligible for FEMA's Hazard Mitigation Grant Program (HMGP): a declaration dated calendar year 2025 or 2026, carrying the hazard-mitigation authorization flag, reaches the community by any path; else 0. A value of 99 flags a source value that was neither 'yes' nor 'no' (unexpected or missing upstream flag; currently unreachable).",
    "fema_hmgp_lead_applicant_declaration_binary", "1 if potentially eligible for HMGP (per fema_hmgp_binary's 2025-2026 window) and the community was the applicant for the disaster declaration, else 0. A value of 99 flags a source value that was neither 'yes' nor 'no' (unexpected or missing upstream flag; currently unreachable).",
    "fema_hmgp_lead_applicant_state_year", "Most recent calendar year of an intersecting-county or statewide HMGP-flagged declaration in the 2025-2026 window (NA if none).",
    "sba_binary", "1 if potentially eligible for Small Business Administration (SBA) Disaster Loans: a declaration reaching the community has an incident end date in calendar year 2025 or 2026; else 0. A value of 99 flags a source value that was neither 'yes' nor 'no' (unexpected or missing upstream flag; currently unreachable).")
}
