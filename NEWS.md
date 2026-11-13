# school.epi.abm 0.0.0.9003

* The example data file and analysis script in `inst/extdata` are renamed
  without spaces: `lac-covid-data-simple.xlsx` and `abm-results-loop.R`.
* The LA County incidence-rate report moves from `vignettes/` to
  `analyses/`. It was a standalone report rather than a package vignette,
  and the package no longer declares a vignette builder.
* Development setup (`.Rprofile` and the Copilot setup workflow) no longer
  loads the archived `pryr` package.

# school.epi.abm 0.0.0.9002

* Package code refers to data columns through rlang's data pronoun,
  to function arguments through its environment pronoun, and groups
  with `.by`, so R CMD check no longer reports undefined global
  variables for them. Seeded simulation results and summaries are
  identical to 0.0.0.9001.

# school.epi.abm 0.0.0.9001

* `run_simulation()` installs and runs again on current CRAN packages:
  it no longer depends on `pryr` (archived on CRAN) or the deprecated
  `purrr::rbernoulli()`, and works with dplyr 1.2, which made
  multi-row `summarise()` an error. The replacements make the same
  random draws in the same order as before.
* `plot1_plotly()` now imports `plotly::subplot()`.
* `summarize_records()` keeps all-missing quantile columns numeric, so
  `analyze_results()` works on small simulations.
