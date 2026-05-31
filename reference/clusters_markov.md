# This function reports the likelihood of transition between different states. The analysis is performed in a Markovian manner, i.e., without memory and all states have the same relevance.'

This function reports the likelihood of transition between different
states. The analysis is performed in a Markovian manner, i.e., without
memory and all states have the same relevance.'

## Usage

``` r
clusters_markov(tbl, vars, cVar, sortBy, groupBy, remIntra = FALSE)
```

## Arguments

- tbl:

  A tibble/dataframe featuring a list of states in time - and
  covariates.

- vars:

  Variables of interest in the tibble/dataframe.

- cVar:

  States variable name to obtain transition likelihood.

- sortBy:

  Time index variable.

- groupBy:

  Grouping variable, e.g., subject or class.

- remIntra:

  Logical indicating whether dwelling state transitions should be
  discarded.
