## ---------------------------------------------------------------------------
## figure_6_sba_stacked_bar.R — data prep for feature-2 figure 6
##
## prepare_figure_6_data(): SBA disaster-loan dollars to Indigenous communities,
## allocated via the ZCTA→Indigenous-community crosswalk. Loan dollars are
## converted to 2025 USD, deflating each row from its approval fiscal year.
## Sourced/called by scripts/feature_2/_feature2.qmd; cross-community
## aggregation for the stacked bar happens there. Output rendered to
## outputs/figure6*.png.
## ---------------------------------------------------------------------------
library(climateapi)
library(tidyverse)

source(here::here("scripts", "feature_2", "get_tribal_crosswalks.R"))
source(here::here("scripts", "utilities", "utilities.R"))  # adjust_dollars_to_2025()

prepare_figure_6_data = function() {
  sba_raw = climateapi::get_sba_loans()

  sba_raw2 = sba_raw %>%
    select(
      fiscal_year,
      disaster_number = disaster_number_fema,
      zip_code = damaged_property_zip_code,
      loan_type,
      matches("approved_amount"),
      matches("state")) %>%
    ## get_sba_loans() returns fiscal_year as character (a str_extract
    ## product); cast to integer so year filters compare numerically and the
    ## figure-6 x-axis renders continuous like figures 5 and 7.
    mutate(fiscal_year = as.integer(fiscal_year))

  ## `territory_flag` is carried directly from the crosswalk; for territorial
  ## rows, `target_geography_name` is now a county-equivalent name, not the
  ## territory name, so we can no longer derive the partition from it.
  zcta_tribal_crosswalk = get_tribal_crosswalk(source_geography = "zcta")

  ## I'm unsure how to QC this join because there's no expectation that SBA has made
  ## awards to every indigenous community, so we would anticipate non-joining records from
  ## both sides of the join...
  ## `home_state` (2-letter code) is carried from the crosswalk so the
  ## .qmd can break out Alaskan / Hawaiian / territorial / contiguous
  ## tribes for community-count QC.
  sba_tribal_crosswalk_join = tidylog::left_join(
      zcta_tribal_crosswalk,
      sba_raw2,
      by = c("source_geoid" = "zip_code"),
      relationship = "many-to-many") %>%
    ## Territorial rows are scaled to the county-equivalent's Indigenous (NHPI
    ## alone-or-in-combination) population share, mirroring figure 1; tribal rows
    ## are unscaled (indigenous_share is NA for them, so the if_else returns 1).
    summarize(
      .by = c(target_geoid, target_geography_name, territory_flag, home_state, loan_type, fiscal_year),
      ## Unscaled total (allocation only) carried alongside for the
      ## territorial-scaling QC; equals the scaled total for tribal rows.
      ## Computed first: the scaled sum below reuses the `approved_amount_total`
      ## name, and summarize() evaluates sequentially, so anything after it
      ## would see the scaled scalar rather than the raw column.
      approved_amount_total_unscaled = sum(
        approved_amount_total * allocation_factor_source_to_target, na.rm = TRUE),
      approved_amount_total = sum(
        approved_amount_total * allocation_factor_source_to_target *
          if_else(territory_flag == "Territories", coalesce(indigenous_share, 0), 1),
        na.rm = TRUE))

  ## Convert nominal loan dollars to 2025 USD, deflating each row from its
  ## approval fiscal year. Crosswalk rows SBA never matched carry an NA
  ## fiscal year and zero dollars; adjust_dollars_to_2025() passes those
  ## through unchanged.
  sba_tribal_crosswalk_join2 = sba_tribal_crosswalk_join %>%
    adjust_dollars_to_2025(
      year_variable = "fiscal_year",
      dollar_variables = c("approved_amount_total", "approved_amount_total_unscaled"))

  return(sba_tribal_crosswalk_join2)
}
