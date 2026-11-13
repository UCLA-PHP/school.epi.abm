# school.epi.abm 0.0.0.9001

* `run_simulation()` installs and runs again on current CRAN packages:
  it no longer depends on `pryr` (archived on CRAN) or the deprecated
  `purrr::rbernoulli()`, and works with dplyr 1.2, which made
  multi-row `summarise()` an error. The replacements make the same
  random draws in the same order as before.
* `plot1_plotly()` now imports `plotly::subplot()`.
* `summarize_records()` keeps all-missing quantile columns numeric, so
  `analyze_results()` works on small simulations.
