# Summarize a simulation's final outcomes

Combines the inputs of one
[`run_simulation()`](https://ucla-php.github.io/school.epi.abm/reference/run_simulation.md)
call with its cumulative outcomes on the last simulated day, as one row
per simulation.

## Usage

``` r
analyze_results(simulation_outputs)
```

## Arguments

- simulation_outputs:

  the list returned by
  [`run_simulation()`](https://ucla-php.github.io/school.epi.abm/reference/run_simulation.md)

## Value

a one-row data frame of inputs and final cumulative outcomes, with
formatted "summary" columns giving each outcome's mean and quantile
interval
