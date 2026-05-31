#' @importFrom dplyr %>% group_by ungroup arrange mutate summarise filter select
#'   rename lag all_of
#' @importFrom tidyr nest unnest unnest_wider
#' @importFrom purrr map
#' @importFrom janitor tabyl
#' @importFrom glue glue
utils::globalVariables(c(
  "clus", "clusVec", "cluster", "dwell", "fo",
  "n", "tag", "target", "tot", ".x[[i]]"
))
