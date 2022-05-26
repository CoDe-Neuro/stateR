#' This function reports the likelihood of transition between different states.
#' The analysis is performed in a Markovian manner, i.e., without memory and
#' all states have the same relevance.'
#'
#' @param tbl A tibble/dataframe featuring a list of states in time
#'  - and covariates.
#' @param vars Variables of interest in the tibble/dataframe.
#' @param cVar States variable name to obtain transition likelihood.
#' @param sortBy Time index variable.
#' @param groupBy Grouping variable, e.g., subject or class.
#' @param remIntra Logical indicating whether dwelling state transitions should be discarded.
#'
#' @export

clusters_markov <- function(tbl,
                            vars,
                            cVar,
                            sortBy,
                            groupBy,
                            remIntra = FALSE){

  require(dplyr)
  require(glue)
  require(tidyr)

  srt_ <- dplyr::syms(sortBy)
  grp_ <- dplyr::syms(groupBy)
  vars_ <- dplyr::syms(append(vars,
                              c('source',
                                'target',
                                'tag')))
  vars__ <- dplyr::syms(append(vars,
                               'source'))

  summ_ <- dplyr::syms(append(vars[vars != groupBy],
                              'tag'))

  cluster_mk <- tbl %>%
    dplyr::rename('clus' = all_of(cVar)) %>%
    dplyr::group_by(!!!grp_) %>%
    dplyr::arrange(!!!srt_) %>%
    dplyr::mutate(source = lag(clus, n = 1),
                  target = clus,
                  tag = paste0(source, '_', target)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(source != 'NA')

  nClus <- max(cluster_mk$clus)
  inward <- character(length(nClus))
  for (i in 0:nClus){inward[i + 1] <- glue::glue('{i}_{i}')}

  if (remIntra) {

    cluster_mk <- cluster_mk %>%
      dplyr::filter(!tag %in% inward)

  }

  cluster_mk %>%
    dplyr::group_by(!!!vars_) %>%
    dplyr::summarise(n = table(tag)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!vars__) %>%
    dplyr::mutate(tot = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nCount = n/tot) %>%
    dplyr::group_by(tag, source, target) %>%
    tidyr::nest()

}
