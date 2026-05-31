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

trans
#> # A tibble: 16 × 4
#> # Groups:   tag, source, target [16]
#>    source target tag   data            
#>     <int>  <int> <chr> <list>          
#>  1      0      0 0_0   <tibble [5 × 5]>
#>  2      0      1 0_1   <tibble [5 × 5]>
#>  3      0      2 0_2   <tibble [4 × 5]>
#>  4      0      3 0_3   <tibble [5 × 5]>
#>  5      1      0 1_0   <tibble [5 × 5]>
#>  6      1      1 1_1   <tibble [5 × 5]>
#>  7      1      2 1_2   <tibble [5 × 5]>
#>  8      1      3 1_3   <tibble [5 × 5]>
#>  9      2      0 2_0   <tibble [4 × 5]>
#> 10      2      1 2_1   <tibble [5 × 5]>
#> 11      2      2 2_2   <tibble [5 × 5]>
#> 12      2      3 2_3   <tibble [5 × 5]>
#> 13      3      0 3_0   <tibble [5 × 5]>
#> 14      3      1 3_1   <tibble [5 × 5]>
#> 15      3      2 3_2   <tibble [4 × 5]>
#> 16      3      3 3_3   <tibble [5 × 5]>
```

Transitions are labelled by a `tag` in `"source_target"` format. To
inspect a specific transition:

``` r

trans %>%
  tidyr::unnest(data) %>%
  dplyr::filter(tag == "0_2")
#> # A tibble: 4 × 8
#> # Groups:   tag, source, target [1]
#>   source target tag   subject group   n             tot nCount     
#>    <int>  <int> <chr> <chr>   <chr>   <table[1d]> <int> <table[1d]>
#> 1      0      2 0_2   sub-01  term    2              13 0.15384615 
#> 2      0      2 0_2   sub-02  term    1               8 0.12500000 
#> 3      0      2 0_2   sub-03  term    1              13 0.07692308 
#> 4      0      2 0_2   sub-04  preterm 4              15 0.26666667
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
#> # A tibble: 4 × 8
#> # Groups:   tag, source, target [1]
#>   source target tag   subject group   n             tot nCount     
#>    <int>  <int> <chr> <chr>   <chr>   <table[1d]> <int> <table[1d]>
#> 1      0      2 0_2   sub-01  term    2               7 0.28571429 
#> 2      0      2 0_2   sub-02  term    1               5 0.20000000 
#> 3      0      2 0_2   sub-03  term    1              11 0.09090909 
#> 4      0      2 0_2   sub-04  preterm 4               8 0.50000000
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
