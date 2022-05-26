#' Fractional occupancy'
#' @export

nest_fo <- function(tbl, vars, foVar){

  require(dplyr)
  require(tidyr)
  require(janitor)

  vars_ <- dplyr::syms(vars)

  nested_fo <- tbl %>%
    dplyr::rename('clus' = all_of(foVar)) %>%
    dplyr::group_by(!!!vars_) %>%
    dplyr::summarise(clusVec = list(clus)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fo = purrr::map(clusVec,
                                  janitor::tabyl)) %>%
    tidyr::unnest(fo) %>%
    dplyr::rename('cluster' = `.x[[i]]`,
                  'perc' = 'percent') %>%
    dplyr::mutate(cluster = as.character(cluster)) %>%
    dplyr::select(-c(n,
                     clusVec)) %>%
    dplyr::group_by(cluster) %>%
    tidyr::nest()

  return(nested_fo)

}
