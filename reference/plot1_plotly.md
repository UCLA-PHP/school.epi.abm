# Plot simulated outcomes over time

Draws interactive plots of daily and cumulative infection, quarantine
and outreach outcomes for students and household adults.

## Usage

``` r
plot1_plotly(
  class_records,
  average_class_data_by_day = summarize_records(class_records),
  plot3_ymax = 5,
  plot4_ymax = plot3_ymax,
  plot5_ymax = 3,
  x1 = 0,
  x2 = 0.5,
  x3 = 0.5
)
```

## Arguments

- class_records:

  the `class_records` element of the list returned by
  [`run_simulation()`](https://ucla-php.github.io/school.epi.abm/reference/run_simulation.md)

- average_class_data_by_day:

  daily summaries of `class_records`, as returned by
  [`summarize_records()`](https://ucla-php.github.io/school.epi.abm/reference/summarize_records.md)

- plot3_ymax:

  minimum upper limit of the y-axis for student infection rates

- plot4_ymax:

  minimum upper limit of the y-axis for household adult infection rates

- plot5_ymax:

  minimum upper limit of the y-axis for quarantine and outreach outcomes

- x1, x2, x3:

  currently unused

## Value

a plotly figure
