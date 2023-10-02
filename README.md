
<!-- README.md is generated from README.Rmd. Please edit that file -->

# school.epi.abm

<!-- badges: start -->
<!-- badges: end -->

The goal of school.epi.abm is to implement an agent-based model of COVID
infection and mitigation dynamics in K-6 schools, as described in
<https://doi.org/10.3389/fpubh.2023.856940>.

## Installation

You can install the development version of school.epi.abm like so:

``` r
# install.packages("devtools") # if not already installed
devtools::install_github("d-morrison/school.epi.abm")
```

## Examples

The graphical user interface for the model can be run using the
following command:

``` r
shiny::runApp()
```

The model can also be run directly from the R command line using the
following commands:

``` r
library(school.epi.abm)

simulation_outputs = run_simulation(
  n_schools = 10,
  verbose = TRUE)
```

We can summarize and visualize the results as follows:

``` r
average_class_data_by_day = summarize_records(simulation_outputs$class_records)

plot1a = plot1_plotly(
  class_records,
  plot3_ymax = 6)

print(plot1a)
```

``` r
analyze_results(simulation_outputs)
```

The subfolder `inst/extdata` contains the analysis script
`ABM results loop.R`, which can be used to loop over a table of input
conditions (specified in that script as the tibble
`conditions_to_cross`) and reproduce the tables in the corresponding
article
(<https://www.medrxiv.org/content/10.1101/2021.02.27.21252535v1>).
