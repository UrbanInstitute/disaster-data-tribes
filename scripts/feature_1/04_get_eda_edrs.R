## ---------------------------------------------------------------------------
## 04_get_eda_edrs.R — EDA Economic Development Representative lookup
##
## Reference table mapping each state/territory to its EDA regional contact,
## sourced by 05_build_feature1_search.R to attach EDA contacts to the
## per-community search deliverable.
## ---------------------------------------------------------------------------
library(tidyverse)

#' Economic Development Administration regional contacts
#'
#' Returns the state-by-state mapping of EDA Economic Development
#' Representatives (EDRs) responsible for each U.S. state, territory, and
#' Indigenous community we treat as analogous to a state in this project
#' (AS, GU, MP). Transcribed from the EDA regional-office contact pages
#' (https://www.eda.gov/about/contact — per-region EDR staff listings, as
#' posted spring 2026) into the tribble below. Puerto Rico, the U.S.
#' Virgin Islands, and the Pacific freely-associated states (FSM, MH, PW)
#' are omitted because they are out of scope for this project.
#'
#' Some states are covered by more than one EDR (e.g. Texas is split
#' across four Austin-region EDRs by sub-region; Missouri is split between
#' two Denver-region EDRs). Each (state, EDR) pair is its own row;
#' downstream consumers should collapse on `state` if a single value per
#' state is required.
#'
#' @return A tibble with one row per (state, EDR) pair and the columns
#'   `eda_region`, `state` (two-letter postal abbreviation),
#'   `edr_name`, `edr_title`, `edr_phone`, `edr_email`.
get_eda_edrs <- function() {
  tibble::tribble(
    ~eda_region,    ~state, ~edr_name,             ~edr_title,                            ~edr_phone,        ~edr_email,
    ## Seattle Regional Office
    "Seattle",      "AK",   "Shirley Kelly",       "Economic Development Representative", "(206) 909-1805",  "skelly2@eda.gov",
    "Seattle",      "AS",   "Keoki Noji",          "Economic Development Representative", "(808) 354-5339",  "knoji@eda.gov",
    "Seattle",      "GU",   "Keoki Noji",          "Economic Development Representative", "(808) 354-5339",  "knoji@eda.gov",
    "Seattle",      "HI",   "Keoki Noji",          "Economic Development Representative", "(808) 354-5339",  "knoji@eda.gov",
    "Seattle",      "MP",   "Keoki Noji",          "Economic Development Representative", "(808) 354-5339",  "knoji@eda.gov",
    "Seattle",      "AZ",   "Jeff Hays",           "Economic Development Representative", "(206) 999-2079",  "jhays@eda.gov",
    "Seattle",      "CA",   "Jeff Hays",           "Economic Development Representative", "(206) 999-2079",  "jhays@eda.gov",
    "Seattle",      "ID",   "J. Wesley Cochran",   "Economic Development Representative", "(206) 561-6646",  "jcochran@eda.gov",
    "Seattle",      "OR",   "J. Wesley Cochran",   "Economic Development Representative", "(206) 561-6646",  "jcochran@eda.gov",
    "Seattle",      "WA",   "J. Wesley Cochran",   "Economic Development Representative", "(206) 561-6646",  "jcochran@eda.gov",
    "Seattle",      "NV",   "La Tarche Collins",   "Economic Development Representative", "(206) 379-8682",  "lcollins@eda.gov",
    ## Denver Regional Office
    "Denver",       "CO",   "Trent Thompson",      "Economic Development Representative", "(303) 319-6940",  "tthompson@eda.gov",
    "Denver",       "UT",   "Trent Thompson",      "Economic Development Representative", "(303) 319-6940",  "tthompson@eda.gov",
    "Denver",       "IA",   "Ali DeMersseman",     "Economic Development Representative", "(720) 237-6079",  "ademersseman@eda.gov",
    "Denver",       "ND",   "Ali DeMersseman",     "Economic Development Representative", "(720) 237-6079",  "ademersseman@eda.gov",
    "Denver",       "SD",   "Ali DeMersseman",     "Economic Development Representative", "(720) 237-6079",  "ademersseman@eda.gov",
    "Denver",       "KS",   "Mark Werthmann",      "Economic Development Representative", "(720) 626-6192",  "mwerthmann@eda.gov",
    "Denver",       "MO",   "Mark Werthmann",      "Economic Development Representative", "(720) 626-6192",  "mwerthmann@eda.gov",
    "Denver",       "NE",   "Mark Werthmann",      "Economic Development Representative", "(720) 626-6192",  "mwerthmann@eda.gov",
    "Denver",       "MO",   "Chad Eggen",          "Economic Development Representative", "(573) 606-0528",  "ceggen@eda.gov",
    "Denver",       "MT",   "Aaron Pratt",         "Economic Development Representative", "(406) 599-9795",  "apratt@eda.gov",
    "Denver",       "WY",   "Aaron Pratt",         "Economic Development Representative", "(406) 599-9795",  "apratt@eda.gov",
    ## Chicago Regional Office
    "Chicago",      "IL",   "Tom Baron",           "Economic Development Representative", "(224) 229-9154",  "tbaron@eda.gov",
    "Chicago",      "WI",   "Tom Baron",           "Economic Development Representative", "(224) 229-9154",  "tbaron@eda.gov",
    "Chicago",      "IN",   "Darrin Fleener",      "Economic Development Representative", "(312) 405-8521",  "dfleener@eda.gov",
    "Chicago",      "MN",   "Darrin Fleener",      "Economic Development Representative", "(312) 405-8521",  "dfleener@eda.gov",
    "Chicago",      "MI",   "Lee J. Shirey",       "Economic Development Representative", "(312) 720-0076",  "lshirey@eda.gov",
    "Chicago",      "OH",   "Lee J. Shirey",       "Economic Development Representative", "(312) 720-0076",  "lshirey@eda.gov",
    ## Austin Regional Office
    "Austin",       "AR",   "April Campbell",      "Economic Development Representative", "(512) 667-0496",  "ACampbell@eda.gov",
    "Austin",       "LA",   "Robert Peche",        "Economic Development Representative", "(512) 568-7732",  "rpeche1@eda.gov",
    "Austin",       "TX",   "Robert Peche",        "Economic Development Representative (Texas — South, West)", "(512) 568-7732", "rpeche1@eda.gov",
    "Austin",       "NM",   "Trisha Korbas",       "Economic Development Representative", "(720) 626-1499",  "tkorbas@eda.gov",
    "Austin",       "TX",   "Trisha Korbas",       "Economic Development Representative (Texas — Panhandle)",   "(720) 626-1499", "tkorbas@eda.gov",
    "Austin",       "OK",   "Stacey Webb",         "Economic Development Representative", "(737) 704-4707",  "swebb@eda.gov",
    "Austin",       "TX",   "Stacey Webb",         "Economic Development Representative (Texas — North)",       "(737) 704-4707", "swebb@eda.gov",
    "Austin",       "TX",   "Angela Bonner",       "Economic Development Representative (Texas — East)",        "(512) 809-6625", "abonner@eda.gov",
    ## Atlanta Regional Office
    "Atlanta",      "AL",   "Lucas Z. Blankenship","Economic Development Representative", "(615) 736-1423",  "lblankenship@eda.gov",
    "Atlanta",      "MS",   "Lucas Z. Blankenship","Economic Development Representative", "(615) 736-1423",  "lblankenship@eda.gov",
    "Atlanta",      "TN",   "Lucas Z. Blankenship","Economic Development Representative", "(615) 736-1423",  "lblankenship@eda.gov",
    "Atlanta",      "FL",   "Greg Vaday",          "Economic Development Representative", "(772) 521-4371",  "gvaday@eda.gov",
    "Atlanta",      "GA",   "Jonathan Corso",      "Economic Development Representative", "(404) 809-7094",  "jcorso@eda.gov",
    "Atlanta",      "KY",   "Emily Hathcock",      "Economic Development Representative", "(202) 424-0424",  "EHathcock@eda.gov",
    "Atlanta",      "NC",   "Hillary Sherman",     "Economic Development Representative", "(828) 707-2748",  "hsherman@eda.gov",
    "Atlanta",      "SC",   "Hillary Sherman",     "Economic Development Representative", "(828) 707-2748",  "hsherman@eda.gov",
    ## Philadelphia Regional Office
    "Philadelphia", "CT",   "Debra Beavin",        "Economic Development Representative", "(267) 559-3385",  "dbeavin@eda.gov",
    "Philadelphia", "MA",   "Debra Beavin",        "Economic Development Representative", "(267) 559-3385",  "dbeavin@eda.gov",
    "Philadelphia", "RI",   "Debra Beavin",        "Economic Development Representative", "(267) 559-3385",  "dbeavin@eda.gov",
    "Philadelphia", "DE",   "Lauren Stuhldreher",  "Economic Development Representative", "(215) 764-0427",  "lstuhldreher@eda.gov",
    "Philadelphia", "DC",   "Lauren Stuhldreher",  "Economic Development Representative", "(215) 764-0427",  "lstuhldreher@eda.gov",
    "Philadelphia", "VA",   "Lauren Stuhldreher",  "Economic Development Representative", "(215) 764-0427",  "lstuhldreher@eda.gov",
    "Philadelphia", "ME",   "Katherine Trapani",   "Economic Development Representative", "(215) 514-6572",  "ktrapani@eda.gov",
    "Philadelphia", "NH",   "Katherine Trapani",   "Economic Development Representative", "(215) 514-6572",  "ktrapani@eda.gov",
    "Philadelphia", "VT",   "Katherine Trapani",   "Economic Development Representative", "(215) 514-6572",  "ktrapani@eda.gov",
    "Philadelphia", "MD",   "Bob Gittler",         "Administrative Director",             "(267) 713-0326",  "bgittler@eda.gov",
    "Philadelphia", "NJ",   "Lucas Martin",        "Economic Development Representative", "(267) 314-3476",  "lmartin2@eda.gov",
    "Philadelphia", "NY",   "Lucas Martin",        "Economic Development Representative", "(267) 314-3476",  "lmartin2@eda.gov",
    "Philadelphia", "PA",   "Christopher Casper",  "Economic Development Specialist",     "(267) 634-7575",  "CCasper1@eda.gov",
    "Philadelphia", "WV",   "Ellen Heinz",         "Economic Development Representative", "(202) 209-4295",  "eheinz1@eda.gov")
}
