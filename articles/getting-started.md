# Getting started with stateR

## What is a brain state?

Dynamic functional connectivity analyses parcel resting-state fMRI time
series into a sequence of discrete **brain states** — recurring patterns
of whole-brain co-activation identified by clustering methods such as
k-means or hidden Markov models. Each volume (or window) in the scan is
assigned a state label, producing a time series like:

    0 0 2 2 2 1 1 0 3 3 3 3 1 2 ...

Once you have this sequence, the natural questions are:

- **How often** is each state visited? (*fractional occupancy*)
- **How long** does each visit last? (*dwell time*)
- **How likely** is a transition from state A to state B? (*Markov
  transitions*)

`stateR` answers all three in a tidy, pipeable workflow.

------------------------------------------------------------------------

## Input format

All three functions expect a **long-format tibble** with one row per
subject per time point, plus any grouping or covariate columns you want
to carry through to the output:

| Column               | Role                                              |
|----------------------|---------------------------------------------------|
| Subject / session ID | Grouping — passed via `vars`                      |
| Time index           | Ordering — passed via `sortBy`                    |
| State label          | The state sequence — passed via `foVar` or `cVar` |
| Any covariates       | Carried through unchanged                         |

------------------------------------------------------------------------

## Simulated data

We simulate five subjects, each with 40 time points and four possible
states (0–3):

``` r

set.seed(42)

n_subjects  <- 5
n_timepoints <- 40

tbl <- tibble::tibble(
  subject = rep(paste0("sub-0", 1:n_subjects), each = n_timepoints),
  group   = rep(c("term", "preterm"), times = c(3 * n_timepoints,
                                                 2 * n_timepoints)),
  time    = rep(seq_len(n_timepoints), n_subjects),
  state   = sample(0:3, n_subjects * n_timepoints, replace = TRUE)
)

head(tbl, 8)
#> # A tibble: 8 × 4
#>   subject group  time state
#>   <chr>   <chr> <int> <int>
#> 1 sub-01  term      1     0
#> 2 sub-01  term      2     0
#> 3 sub-01  term      3     0
#> 4 sub-01  term      4     0
#> 5 sub-01  term      5     1
#> 6 sub-01  term      6     3
#> 7 sub-01  term      7     1
#> 8 sub-01  term      8     1
```

------------------------------------------------------------------------

## Fractional occupancy with `nest_fo()`

[`nest_fo()`](https://CoDe-Neuro.github.io/stateR/reference/nest_fo.md)
computes the proportion of time points each subject/group spends in each
state:

``` r

fo <- nest_fo(
  tbl   = tbl,
  vars  = c("subject", "group"),
  foVar = "state"
)
#> Loading required package: janitor
#> 
#> Attaching package: 'janitor'
#> The following objects are masked from 'package:stats':
#> 
#>     chisq.test, fisher.test

fo
#> # A tibble: 4 × 2
#> # Groups:   cluster [4]
#>   cluster data            
#>   <chr>   <list>          
#> 1 0       <tibble [5 × 3]>
#> 2 1       <tibble [5 × 3]>
#> 3 2       <tibble [5 × 3]>
#> 4 3       <tibble [5 × 3]>
```

The result is a **state-nested tibble** — one row per state, with a
`data` list-column holding each subject’s fractional occupancy (`perc`):

``` r

fo %>%
  tidyr::unnest(data) %>%
  head(12)
#> # A tibble: 12 × 4
#> # Groups:   cluster [3]
#>    cluster subject group    perc
#>    <chr>   <chr>   <chr>   <dbl>
#>  1 0       sub-01  term    0.325
#>  2 0       sub-02  term    0.2  
#>  3 0       sub-03  term    0.325
#>  4 0       sub-04  preterm 0.375
#>  5 0       sub-05  preterm 0.15 
#>  6 1       sub-01  term    0.25 
#>  7 1       sub-02  term    0.4  
#>  8 1       sub-03  term    0.25 
#>  9 1       sub-04  preterm 0.2  
#> 10 1       sub-05  preterm 0.425
#> 11 2       sub-01  term    0.175
#> 12 2       sub-02  term    0.175
```

To work with a specific state:

``` r

fo %>%
  tidyr::unnest(data) %>%
  dplyr::filter(cluster == "2")
#> # A tibble: 5 × 4
#> # Groups:   cluster [1]
#>   cluster subject group    perc
#>   <chr>   <chr>   <chr>   <dbl>
#> 1 2       sub-01  term    0.175
#> 2 2       sub-02  term    0.175
#> 3 2       sub-03  term    0.125
#> 4 2       sub-04  preterm 0.2  
#> 5 2       sub-05  preterm 0.175
```

------------------------------------------------------------------------

## Dwell time with `nest_dwell()`

[`nest_dwell()`](https://CoDe-Neuro.github.io/stateR/reference/nest_dwell.md)
computes the **mean continuous occupancy** per state — the average
number of consecutive time points spent in a single uninterrupted visit.
Single time-point visits (dwell = 1) are excluded, as they likely
reflect noise rather than genuine state occupation.

``` r

dwell <- nest_dwell(
  tbl    = tbl,
  vars   = c("subject", "group"),
  foVar  = "state",
  sortBy = "time"
)

dwell %>%
  tidyr::unnest(data)
#> # A tibble: 20 × 4
#> # Groups:   cluster [4]
#>    cluster subject group   mean_dwell
#>    <chr>   <chr>   <chr>        <dbl>
#>  1 0       sub-01  term          2.5 
#>  2 0       sub-02  term          2.5 
#>  3 0       sub-03  term          2   
#>  4 0       sub-04  preterm       2.4 
#>  5 0       sub-05  preterm       2   
#>  6 1       sub-01  term          2   
#>  7 1       sub-02  term          3   
#>  8 1       sub-03  term          2.5 
#>  9 1       sub-04  preterm       2   
#> 10 1       sub-05  preterm       2.25
#> 11 2       sub-01  term          2   
#> 12 2       sub-02  term          2   
#> 13 2       sub-03  term          2   
#> 14 2       sub-04  preterm       2   
#> 15 2       sub-05  preterm       2   
#> 16 3       sub-01  term          2   
#> 17 3       sub-02  term          3   
#> 18 3       sub-03  term          2   
#> 19 3       sub-04  preterm       2   
#> 20 3       sub-05  preterm       2
```

The `sortBy` argument is critical — it ensures observations are ordered
chronologically before the run-length encoding that underlies dwell time
estimation. Always pass the time index column here.

------------------------------------------------------------------------

## Markov transitions with `clusters_markov()`

[`clusters_markov()`](https://CoDe-Neuro.github.io/stateR/reference/clusters_markov.md)
computes **transition probabilities** between states. For each source
state, it counts every observed transition and normalises by the total
number of transitions out of that source — a first-order Markov chain.

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
#> 1      0      0 0_0   <tibble [5 × 5]>
#> 2      1      1 1_1   <tibble [5 × 5]>
#> 3      2      2 2_2   <tibble [5 × 5]>
#> 4      3      3 3_3   <tibble [5 × 5]>
```

Transitions are labelled by a `tag` in `"source_target"` format. To
inspect a specific transition:

``` r

trans %>%
  tidyr::unnest(data) %>%
  dplyr::filter(tag == "0_2")
#> # A tibble: 0 × 8
#> # Groups:   tag, source, target [0]
#> # ℹ 8 variables: source <int>, target <int>, tag <chr>, subject <chr>,
#> #   group <chr>, n <table[1d]>, tot <int>, nCount <table[1d]>
```

Set `remIntra = TRUE` to exclude self-transitions (e.g. state 2 → state
2), which is useful when you are interested only in genuine state
changes:

``` r

clusters_markov(
  tbl      = tbl,
  vars     = c("subject", "group"),
  cVar     = "state",
  sortBy   = "time",
  groupBy  = "subject",
  remIntra = TRUE
) %>%
  tidyr::unnest(data) %>%
  dplyr::filter(tag == "0_2")
#> # A tibble: 0 × 4
#> # Groups:   tag, source, target [0]
#> # ℹ 4 variables: source <int>, target <int>, tag <chr>, data <???>
```

------------------------------------------------------------------------

## Output structure at a glance

All three functions return the same **state-nested tibble** shape:

| Function | Nesting key | Key output column | Unit |
|----|----|----|----|
| [`nest_fo()`](https://CoDe-Neuro.github.io/stateR/reference/nest_fo.md) | `cluster` | `perc` | Proportion (0–1) |
| [`nest_dwell()`](https://CoDe-Neuro.github.io/stateR/reference/nest_dwell.md) | `cluster` | `mean_dwell` | Time points |
| [`clusters_markov()`](https://CoDe-Neuro.github.io/stateR/reference/clusters_markov.md) | `tag` (e.g. `"0_2"`) | `nCount` | Probability (0–1) |

This shared shape makes it straightforward to apply the same downstream
analysis (e.g. permutation tests with
[`ptestR`](https://github.com/CoDe-Neuro/ptestR)) across all three
metrics without changing your pipeline.

------------------------------------------------------------------------

## Further reading

- [`vignette("markov-transitions")`](https://CoDe-Neuro.github.io/stateR/articles/markov-transitions.md)
  — a deeper look at the transition matrix
- [`vignette("grouped-pipeline")`](https://CoDe-Neuro.github.io/stateR/articles/grouped-pipeline.md)
  — running statistical tests across all states
- [`?nest_fo`](https://CoDe-Neuro.github.io/stateR/reference/nest_fo.md),
  [`?nest_dwell`](https://CoDe-Neuro.github.io/stateR/reference/nest_dwell.md),
  [`?clusters_markov`](https://CoDe-Neuro.github.io/stateR/reference/clusters_markov.md)
