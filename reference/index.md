# Package index

## State metrics

Core functions for quantifying brain state dynamics.

- [`nest_fo()`](https://CoDe-Neuro.github.io/stateR/reference/nest_fo.md)
  : This function estimates the fractional occupancy or probability of a
  series of states.'
- [`nest_dwell()`](https://CoDe-Neuro.github.io/stateR/reference/nest_dwell.md)
  : This function estimates the dwelling time or continuous occupancy of
  a series of states.'
- [`clusters_markov()`](https://CoDe-Neuro.github.io/stateR/reference/clusters_markov.md)
  : This function reports the likelihood of transition between different
  states. The analysis is performed in a Markovian manner, i.e., without
  memory and all states have the same relevance.'

## Internals

Helper functions used internally.

- [`dwellCount()`](https://CoDe-Neuro.github.io/stateR/reference/dwellCount.md)
  : This function obtains dwelling times for states from a list.
