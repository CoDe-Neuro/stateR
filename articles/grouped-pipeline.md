# Grouped pipeline: testing across states

## Overview

In practice, brain state analyses involve not just computing metrics but
testing whether those metrics differ between groups or associate with
continuous variables — and doing so across every state simultaneously.
This vignette shows the full pipeline:

1.  Compute
    [`nest_fo()`](https://CoDe-Neuro.github.io/stateR/reference/nest_fo.md),
    [`nest_dwell()`](https://CoDe-Neuro.github.io/stateR/reference/nest_dwell.md),
    or
    [`clusters_markov()`](https://CoDe-Neuro.github.io/stateR/reference/clusters_markov.md)
2.  Unnest and filter to one state (or transition) at a time
3.  Apply a statistical model — demonstrated here with base R, and
    compatible with [`ptestR`](https://github.com/CoDe-Neuro/ptestR) for
    permutation tests

------------------------------------------------------------------------

## Simulated data

We simulate 20 subjects (10 term, 10 preterm), each with 80 time points
and five possible states (0–4). States are drawn with unequal
probabilities to create realistic differences:

``` r

set.seed(2024)

n_sub <- 20
n_tp  <- 80

# Preterm subjects spend more time in state 0, term in state 2
state_probs <- list(
  term    = c(0.15, 0.20, 0.35, 0.15, 0.15),
  preterm = c(0.35, 0.20, 0.15, 0.15, 0.15)
)

tbl <- purrr::map_dfr(seq_len(n_sub), function(i) {
  grp   <- ifelse(i <= 10, "term", "preterm")
  probs <- state_probs[[grp]]
  tibble::tibble(
    subject = sprintf("sub-%02d", i),
    group   = grp,
    pma     = rnorm(1, mean = ifelse(grp == "term", 40, 34), sd = 2),
    time    = seq_len(n_tp),
    state   = sample(0:4, n_tp, replace = TRUE, prob = probs)
  )
})

dplyr::count(tbl, group)
#> # A tibble: 2 × 2
#>   group       n
#>   <chr>   <int>
#> 1 preterm   800
#> 2 term      800
```

------------------------------------------------------------------------

## Step 1 — Compute fractional occupancy

``` r

fo <- nest_fo(
  tbl   = tbl,
  vars  = c("subject", "group", "pma"),
  foVar = "state"
)

fo
#> # A tibble: 5 × 2
#> # Groups:   cluster [5]
#>   cluster data             
#>   <chr>   <list>           
#> 1 0       <tibble [20 × 4]>
#> 2 1       <tibble [20 × 4]>
#> 3 2       <tibble [20 × 4]>
#> 4 3       <tibble [20 × 4]>
#> 5 4       <tibble [20 × 4]>
```

Each row is a state; the `data` list-column holds one row per subject
with their `perc` value and any covariates passed via `vars`.

------------------------------------------------------------------------

## Step 2 — Test each state

Unnesting gives a flat data frame per state. We can map a model across
all states using
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html). Here
we use a simple linear model (`lm`) to test whether fractional occupancy
differs by group — swap in `ptestR::grouped_perm_glm()` for a
permutation test:

``` r

fo_results <- fo %>%
  dplyr::mutate(
    model = purrr::map(data, ~ lm(perc ~ group + pma, data = .x)),
    tidy  = purrr::map(model, broom::tidy)
  ) %>%
  dplyr::select(cluster, tidy) %>%
  tidyr::unnest(tidy) %>%
  dplyr::filter(term == "groupterm") %>%
  dplyr::select(cluster, estimate, statistic, p.value) %>%
  dplyr::arrange(p.value)

fo_results
#> # A tibble: 5 × 4
#> # Groups:   cluster [5]
#>   cluster estimate statistic    p.value
#>   <chr>      <dbl>     <dbl>      <dbl>
#> 1 0       -0.177      -7.12  0.00000172
#> 2 2        0.172       4.88  0.000142  
#> 3 1       -0.0164     -0.590 0.563     
#> 4 3        0.0150      0.479 0.638     
#> 5 4        0.00651     0.229 0.822
```

Apply FDR correction across all states:

``` r

fo_results %>%
  dplyr::mutate(p.fdr = p.adjust(p.value, method = "fdr"))
#> # A tibble: 5 × 5
#> # Groups:   cluster [5]
#>   cluster estimate statistic    p.value      p.fdr
#>   <chr>      <dbl>     <dbl>      <dbl>      <dbl>
#> 1 0       -0.177      -7.12  0.00000172 0.00000172
#> 2 2        0.172       4.88  0.000142   0.000142  
#> 3 1       -0.0164     -0.590 0.563      0.563     
#> 4 3        0.0150      0.479 0.638      0.638     
#> 5 4        0.00651     0.229 0.822      0.822
```

------------------------------------------------------------------------

## Step 3 — Dwell time pipeline

The same pattern works for dwell time:

``` r

dwell <- nest_dwell(
  tbl    = tbl,
  vars   = c("subject", "group", "pma"),
  foVar  = "state",
  sortBy = "time"
)

dwell %>%
  dplyr::mutate(
    model = purrr::map(data, ~ lm(mean_dwell ~ group + pma, data = .x)),
    tidy  = purrr::map(model, broom::tidy)
  ) %>%
  dplyr::select(cluster, tidy) %>%
  tidyr::unnest(tidy) %>%
  dplyr::filter(term == "groupterm") %>%
  dplyr::select(cluster, estimate, statistic, p.value)
#> # A tibble: 5 × 4
#> # Groups:   cluster [5]
#>   cluster estimate statistic p.value
#>   <chr>      <dbl>     <dbl>   <dbl>
#> 1 0         -0.133    -0.376   0.712
#> 2 1         -0.192    -0.526   0.606
#> 3 2          0.216     0.847   0.412
#> 4 3         -0.831    -1.56    0.140
#> 5 4         -0.490    -1.35    0.201
```

------------------------------------------------------------------------

## Step 4 — Markov transition pipeline

For transitions, the nesting key is `tag` rather than `cluster`:

``` r

trans <- clusters_markov(
  tbl      = tbl,
  vars     = c("subject", "group", "pma"),
  cVar     = "state",
  sortBy   = "time",
  groupBy  = "subject",
  remIntra = FALSE
)

# Unnest fully to flat frame, re-nest cleanly by tag for modelling
trans_long <- tidyr::unnest(trans, data)

trans_results <- trans_long %>%
  dplyr::select(tag, dplyr::any_of(c("subject", "group", "pma", "nCount"))) %>%
  dplyr::group_by(tag) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    model = purrr::map(data, ~ lm(nCount ~ group + pma, data = .x)),
    tidy  = purrr::map(model, broom::tidy)
  ) %>%
  dplyr::select(tag, tidy) %>%
  tidyr::unnest(tidy) %>%
  dplyr::filter(term == "groupterm") %>%
  dplyr::select(tag, estimate, statistic, p.value) %>%
  dplyr::arrange(p.value)
#> Adding missing grouping variables: `source`, `target`

head(trans_results, 10)
#> # A tibble: 10 × 4
#> # Groups:   tag [10]
#>    tag   estimate statistic   p.value
#>    <chr>    <dbl>     <dbl>     <dbl>
#>  1 2_0    -0.345      -5.18 0.0000914
#>  2 0_2     0.303       4.65 0.000228 
#>  3 3_3    -0.166      -2.91 0.0107   
#>  4 1_0    -0.193      -2.53 0.0216   
#>  5 2_2     0.156       2.37 0.0337   
#>  6 0_4    -0.0701     -2.22 0.0416   
#>  7 4_0    -0.168      -2.16 0.0466   
#>  8 1_2     0.122       2.09 0.0551   
#>  9 4_1     0.126       1.94 0.0695   
#> 10 3_4     0.133       1.90 0.0765
```

------------------------------------------------------------------------

## Using `ptestR` for permutation tests

In the dHCP neonatal study, `stateR` outputs were passed directly into
`ptestR` permutation tests with 10 000 permutations. The swap is one
line:

``` r

library(ptestR)

fo %>%
  dplyr::mutate(
    res = purrr::map(data, ~ grouped_perm_glm(
      tbl         = .x,
      formla      = perc ~ group + pma,
      var_to_perm = "perc",
      permNum     = 10000,
      seed        = 42
    ))
  ) %>%
  dplyr::select(cluster, res) %>%
  tidyr::unnest(res) %>%
  dplyr::filter(term == "groupterm") %>%
  dplyr::mutate(p.fdr = p.adjust(p.perm, method = "fdr"))
```

The same pattern applies to
[`nest_dwell()`](https://CoDe-Neuro.github.io/stateR/reference/nest_dwell.md)
(replace `perc` with `mean_dwell`) and
[`clusters_markov()`](https://CoDe-Neuro.github.io/stateR/reference/clusters_markov.md)
(replace `perc` with `nCount` and `cluster` with `tag`).

------------------------------------------------------------------------

## Notes on `vars` and covariate carriage

Any column passed in `vars` is preserved in the `data` list-column and
therefore available inside every `map()` call. Covariates like `pma`,
`sex`, or `motion_outliers` should be included in `vars` when you plan
to use them in the downstream model — they do not need any special
handling.

One caution: `vars` for
[`nest_fo()`](https://CoDe-Neuro.github.io/stateR/reference/nest_fo.md)
and
[`nest_dwell()`](https://CoDe-Neuro.github.io/stateR/reference/nest_dwell.md)
defines the grouping for the metric computation. If you include a
continuous covariate that varies *within* a subject (e.g. a time-varying
measure), each unique combination of all `vars` values will become its
own group. For subject-level covariates (one value per subject) this is
not a problem.

------------------------------------------------------------------------

## Further reading

- [`vignette("getting-started")`](https://CoDe-Neuro.github.io/stateR/articles/getting-started.md)
  — introduction to the three metrics
- [`vignette("markov-transitions")`](https://CoDe-Neuro.github.io/stateR/articles/markov-transitions.md)
  — transition matrix in depth
- [`ptestR`](https://github.com/CoDe-Neuro/ptestR) — permutation tests
