# Changelog

## school.epi.abm (development version)

- `R CMD check` passes with no warnings. Package code refers to data
  columns through `.data$` and to the `day.name` dataset by its full
  name, the help page for
  [`summarize_records()`](https://ucla-php.github.io/school.epi.abm/reference/summarize_records.md)
  matches its code again, and the citation in
  [`run_simulation()`](https://ucla-php.github.io/school.epi.abm/reference/run_simulation.md)’s
  help uses `\doi{}`.
- Packages used only by the analysis scripts and the app launcher
  (`conflicted`, `ggplot2`, `htmltools`, `knitr`, `lemon`, `pkgload`,
  `RColorBrewer`, `scales`) move from Imports to Suggests, and the
  unused `gridExtra` is dropped. The Title is in title case. Seeded
  simulation results are unchanged.
- Package code follows the lab style guide
  (<https://morrison-lab.github.io/psc/>): the native pipe `|>` replaces
  `magrittr` pipes (so the package now needs R \>= 4.1.0 and no longer
  imports `magrittr`), `<-` replaces `=` for assignment, lines are
  wrapped at 80 characters, and commented-out code is removed.
- A `.lintr.R` file applies the lab’s shared linters from the `lms`
  package. Seeded simulation results are unchanged.
- Every exported function and the `day.name` dataset are documented,
  including all of
  [`run_simulation()`](https://ucla-php.github.io/school.epi.abm/reference/run_simulation.md)’s
  arguments.
- `DESCRIPTION` and `citation("school.epi.abm")` now cite the published
  paper (Morrison et al., 2023,
  <https://doi.org/10.3389/fpubh.2023.856940>) instead of the 2021
  preprint.
- `DESCRIPTION` lists the GitHub repository and issue tracker.
- The package has its first tests: smoke tests for
  [`run_simulation()`](https://ucla-php.github.io/school.epi.abm/reference/run_simulation.md),
  [`summarize_records()`](https://ucla-php.github.io/school.epi.abm/reference/summarize_records.md),
  [`analyze_results()`](https://ucla-php.github.io/school.epi.abm/reference/analyze_results.md)
  and
  [`plot1_plotly()`](https://ucla-php.github.io/school.epi.abm/reference/plot1_plotly.md),
  plus a snapshot of seeded results that fails if a change alters them.
- The example data file and analysis script in `inst/extdata` are
  renamed without spaces: `lac-covid-data-simple.xlsx` and
  `abm-results-loop.R`.
- The LA County incidence-rate report moves from `vignettes/` to
  `analyses/`. It was a standalone report rather than a package
  vignette, and the package no longer declares a vignette builder.
- Development setup (`.Rprofile` and the Copilot setup workflow) no
  longer loads the archived `pryr` package.

## school.epi.abm 0.0.0.9002

- Package code refers to data columns through the `rlang` data pronoun,
  to function arguments through its environment pronoun, and groups with
  `.by`, so R CMD check no longer reports undefined global variables for
  them. Seeded simulation results and summaries are identical to
  0.0.0.9001.

## school.epi.abm 0.0.0.9001

- [`run_simulation()`](https://ucla-php.github.io/school.epi.abm/reference/run_simulation.md)
  installs and runs again on current CRAN packages: it no longer depends
  on `pryr` (archived on CRAN) or the deprecated
  [`purrr::rbernoulli()`](https://purrr.tidyverse.org/reference/rbernoulli.html),
  and works with dplyr 1.2, which made multi-row `summarise()` an error.
  The replacements make the same random draws in the same order as
  before.
- [`plot1_plotly()`](https://ucla-php.github.io/school.epi.abm/reference/plot1_plotly.md)
  now imports
  [`plotly::subplot()`](https://rdrr.io/pkg/plotly/man/subplot.html).
- [`summarize_records()`](https://ucla-php.github.io/school.epi.abm/reference/summarize_records.md)
  keeps all-missing quantile columns numeric, so
  [`analyze_results()`](https://ucla-php.github.io/school.epi.abm/reference/analyze_results.md)
  works on small simulations.
