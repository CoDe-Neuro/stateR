# Markov transition probabilities

## What is a Markov transition?

A first-order Markov chain models the brain state sequence as a
memoryless process: the probability of entering state $`j`$ at time
$`t+1`$ depends only on the current state $`i`$ at time $`t`$, not on
any earlier history.

[`clusters_markov()`](https://CoDe-Neuro.github.io/stateR/reference/clusters_markov.md)
estimates these transition probabilities empirically from observed state
sequences. For each subject (or group), it:

1.  Lags the state sequence by one time point to create source–target
    pairs.
2.  Counts every observed `source → target` transition.
3.  Normalises by the total number of transitions *out of* each source
    state, giving a row-stochastic estimate of the transition matrix.

The result captures **which states the brain tends to move to and
from**, and **how probable each transition is** — information that
fractional occupancy alone cannot provide.

------------------------------------------------------------------------

## Simulated data

``` r

set.seed(99)

tbl <- tibble::tibble(
  subject = rep(paste0("sub-", 1:8), each = 60),
  group   = rep(c("term", "preterm"), each = 4 * 60),
  time    = rep(seq_len(60), 8),
  state   = sample(0:3, 8 * 60, replace = TRUE,
                   prob = c(0.35, 0.25, 0.25, 0.15))
)
```

------------------------------------------------------------------------

## Computing transitions

``` r

trans <- clusters_markov(
  tbl      = tbl,
  vars     = c("subject", "group"),
  cVar     = "state",
  sortBy   = "time",
  groupBy  = "subject",
  remIntra = FALSE
)
#> Loading required package: glue

trans
#> # A tibble: 4 × 4
#> # Groups:   tag, source, target [4]
#>   source target tag   data            
#>    <int>  <int> <chr> <list>          
#> 1      0      0 0_0   <tibble [8 × 5]>
#> 2      1      1 1_1   <tibble [8 × 5]>
#> 3      2      2 2_2   <tibble [8 × 5]>
#> 4      3      3 3_3   <tibble [8 × 5]>
```

The output is nested by `tag` — one row per unique source–target pair.
Unnest to see the per-subject probabilities:

``` r

trans_long <- trans %>%
  tidyr::unnest(data)

trans_long %>%
  dplyr::select(tag, source, target, subject, group, nCount) %>%
  head(16)
#> # A tibble: 16 × 6
#> # Groups:   tag, source, target [2]
#>    tag   source target subject group   nCount     
#>    <chr>  <int>  <int> <chr>   <chr>   <table[1d]>
#>  1 0_0        0      0 sub-1   term    1          
#>  2 0_0        0      0 sub-2   term    1          
#>  3 0_0        0      0 sub-3   term    1          
#>  4 0_0        0      0 sub-4   term    1          
#>  5 0_0        0      0 sub-5   preterm 1          
#>  6 0_0        0      0 sub-6   preterm 1          
#>  7 0_0        0      0 sub-7   preterm 1          
#>  8 0_0        0      0 sub-8   preterm 1          
#>  9 1_1        1      1 sub-1   term    1          
#> 10 1_1        1      1 sub-2   term    1          
#> 11 1_1        1      1 sub-3   term    1          
#> 12 1_1        1      1 sub-4   term    1          
#> 13 1_1        1      1 sub-5   preterm 1          
#> 14 1_1        1      1 sub-6   preterm 1          
#> 15 1_1        1      1 sub-7   preterm 1          
#> 16 1_1        1      1 sub-8   preterm 1
```

------------------------------------------------------------------------

## The transition matrix

To reconstruct the full $`K \times K`$ transition matrix averaged across
subjects, summarise `nCount` by `source` and `target`:

``` r

trans_long %>%
  dplyr::group_by(source, target) %>%
  dplyr::summarise(mean_prob = mean(nCount), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from  = target,
    values_from = mean_prob,
    names_prefix = "to_"
  ) %>%
  dplyr::arrange(source)
#> # A tibble: 4 × 5
#>   source  to_0  to_1  to_2  to_3
#>    <int> <dbl> <dbl> <dbl> <dbl>
#> 1      0     1    NA    NA    NA
#> 2      1    NA     1    NA    NA
#> 3      2    NA    NA     1    NA
#> 4      3    NA    NA    NA     1
```

Each row sums to 1 (within rounding error): given the brain is in state
`source`, the row entries are the probabilities of transitioning to each
possible `target` on the next time step.

------------------------------------------------------------------------

## Removing self-transitions

By default `remIntra = FALSE`, so self-transitions (state $`i`$ → state
$`i`$) are included. These reflect dwell time in the transition matrix.
If you want to study only *genuine state changes*, you can filter
self-transitions out of the unnested output using `source != target`:

``` r

trans_change <- clusters_markov(
  tbl      = tbl,
  vars     = c("subject", "group"),
  cVar     = "state",
  sortBy   = "time",
  groupBy  = "subject",
  remIntra = FALSE
)

# Filter out self-transitions manually (source == target)
trans_change %>%
  tidyr::unnest(data) %>%
  dplyr::filter(source != target) %>%
  dplyr::group_by(tag) %>%
  dplyr::summarise(mean_prob = mean(nCount, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(mean_prob))
#> # A tibble: 0 × 2
#> # ℹ 2 variables: tag <chr>, mean_prob <dbl>
```

With `remIntra = TRUE`, rows no longer sum to 1 because the diagonal has
been removed and the remaining probabilities are re-normalised over
inter-state transitions only.

------------------------------------------------------------------------

## The `tag` convention

Transition tags follow the `"<source>_<target>"` pattern. For a
four-state system (states 0–3), there are 16 possible tags including
self-transitions, or 12 excluding them.

To work with a specific transition of interest:

``` r

trans %>%
  tidyr::unnest(data) %>%
  dplyr::filter(tag == "1_0") %>%
  dplyr::select(subject, group, nCount)
#> Adding missing grouping variables: `tag`, `source`, `target`
#> # A tibble: 0 × 6
#> # Groups:   tag, source, target [0]
#> # ℹ 6 variables: tag <chr>, source <int>, target <int>, subject <chr>,
#> #   group <chr>, nCount <table[1d]>
```

------------------------------------------------------------------------

## `vars` vs `groupBy`

Two arguments control grouping in
[`clusters_markov()`](https://CoDe-Neuro.github.io/stateR/reference/clusters_markov.md),
and it is worth understanding the distinction:

- **`groupBy`** — the variable used to *compute* transitions. Lag and
  pair operations are performed within each level of `groupBy`. This
  should always be the subject (or session) identifier, so that
  transitions are never computed *across* subjects.
- **`vars`** — the variables *carried through* to the output. Include
  everything you want available for downstream analysis: subject ID,
  group, covariates. `groupBy` should be a subset of (or equal to)
  `vars`.

``` r

# groupBy = "subject" ensures no cross-subject transitions are counted.
# vars includes "group" so group membership is available for testing.
clusters_markov(
  tbl      = tbl,
  vars     = c("subject", "group"),
  cVar     = "state",
  sortBy   = "time",
  groupBy  = "subject"
) %>%
  tidyr::unnest(data) %>%
  dplyr::filter(tag == "0_1") %>%
  dplyr::select(subject, group, nCount)
#> Adding missing grouping variables: `tag`, `source`, `target`
#> # A tibble: 0 × 6
#> # Groups:   tag, source, target [0]
#> # ℹ 6 variables: tag <chr>, source <int>, target <int>, subject <chr>,
#> #   group <chr>, nCount <table[1d]>
```

------------------------------------------------------------------------

## Further reading

- [`vignette("getting-started")`](https://CoDe-Neuro.github.io/stateR/articles/getting-started.md)
  — fractional occupancy and dwell time
- [`vignette("grouped-pipeline")`](https://CoDe-Neuro.github.io/stateR/articles/grouped-pipeline.md)
  — applying permutation tests across all transitions
- [`?clusters_markov`](https://CoDe-Neuro.github.io/stateR/reference/clusters_markov.md)
