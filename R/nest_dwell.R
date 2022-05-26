#' Dwelling time'
#' @export

nest_dwell <- function(tbl, vars, foVar, sortBy){

  require(dplyr)
  require(tidyr)
  require(janitor)

  vars_ <- dplyr::syms(vars)
  vars__ <- dplyr::syms(append(vars, 'cluster'))
  srt_ <- dplyr::syms(sortBy)

  tbl %>%
    dplyr::rename('clus' = all_of(foVar)) %>%
    dplyr::group_by(!!!vars_) %>%
    dplyr::arrange(!!!srt_) %>%
    dplyr::summarise(clus = list(clus)) %>%
    dplyr::mutate(cluster = purrr::map(clus, dwellCount)) %>%
    tidyr::unnest_wider(cluster) %>%
    dplyr::select(-clus) %>%
    tidyr::unnest(c(cluster, dwell)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(dwell != 1) %>%
    dplyr::group_by(!!!vars__) %>%
    dplyr::summarise(mean_dwell = mean(dwell)) %>%
    dplyr::mutate(cluster = as.character(cluster)) %>%
    dplyr::group_by(cluster) %>%
    tidyr::nest()

}
