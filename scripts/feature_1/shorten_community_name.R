## ---------------------------------------------------------------------------
## shorten_community_name.R — compact display names for Indigenous communities
##
## shorten_community_name(): trims long community names to nine words or fewer
## (for narrow/mobile displays) by stripping non-identifying clauses. Sourced by
## 05_build_feature1_search.R and 06_build_feature1_data_visualization.R.
## ---------------------------------------------------------------------------
library(tidyverse)

#' Shorten an Indigenous community name to nine words or fewer where possible
#'
#' Produces a compact display name intended to fit narrow (mobile) screens.
#' Names of nine words or fewer are returned unchanged (only whitespace is
#' squished); longer names are trimmed by removing the descriptive clauses that
#' carry no identifying value:
#' * parenthetical content (`"(previously listed as ...)"`, `"(See ...)"`,
#'   `"(includes ...)"`, etc.), repeated to catch nested / multiple groups;
#' * a sub-group enumeration following a colon;
#' * the trailing geographic clause `"of [the] <place>
#'   <Reservation|Rancheria|Colony|...>"` (the reservation/rancheria name and
#'   anything after it), matching only the final such clause so an intervening
#'   `"of the Lake Superior Tribe of Chippewa Indians"` is preserved;
#' * a trailing run of state names (`"of California"`, `", South Dakota"`,
#'   `"Nevada and Oregon"`, etc.).
#'
#' Trimming is applied ONLY to names longer than nine words, so short names —
#' including `"Confederated Tribes of the <X> Reservation"`, `"Iowa Tribe of
#' Oklahoma"`, and the Hawaiian Home Lands residential/agricultural splits —
#' are left intact and never collide. A handful of names whose length is itself
#' their identity (e.g. `"Bad River Band of the Lake Superior Tribe of Chippewa
#' Indians"`) remain above nine words; this is intentional.
#'
#' @param x Character vector of community names. `NA` in yields `NA` out.
#' @return Character vector of shortened names.
shorten_community_name <- function(x) {
  states <- c(state.name, "District of Columbia")
  state_alt <- str_c(states, collapse = "|")
  word_count <- str_count(str_squish(x), "\\S+")

  trimmed <- str_squish(x)
  for (i in 1:6) {
    trimmed <- str_replace_all(trimmed, "\\s*\\([^()]*\\)", "")
  }
  trimmed <- trimmed %>%
    str_replace(":.*$", "") %>%
    str_replace(
      str_c("\\s+of\\s+(the\\s+)?((?:(?!\\s+of\\s+).)*?)\\s*",
            "\\b(Reservation|Reserve|Rancheria|Rancherias|Colony|Ranches|Ranch)\\b.*$"),
      "") %>%
    str_replace(str_c("(\\s*(,|&|and|of)\\s*(", state_alt, "))+\\s*$"), "") %>%
    str_replace_all("\\s+,", ",") %>%
    str_squish() %>%
    str_replace("[,;]+$", "") %>%
    str_squish()

  if_else(word_count > 9, trimmed, str_squish(x))
}
