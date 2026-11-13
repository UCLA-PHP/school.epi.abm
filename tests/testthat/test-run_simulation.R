run_small_simulation <- function(seed = 1) {
  withr::local_seed(seed)
  suppressMessages(
    run_simulation(
      n_schools = 2,
      n_grades_per_school = 2,
      n_weeks_in_time_window = 1
    )
  )
}

test_that("run_simulation() returns class records and inputs", {
  out <- run_small_simulation()

  expect_named(out, c("class_records", "inputs"))
  expect_s3_class(out$class_records, "data.frame")
  expect_true(all(c("date", "school", "class") %in% names(out$class_records)))
  expect_equal(out$inputs$n_schools, 2)
  expect_equal(length(unique(out$class_records$school)), 2)
})

test_that("run_simulation() is reproducible with a fixed seed", {
  expect_equal(run_small_simulation(seed = 2), run_small_simulation(seed = 2))
})

test_that("summaries and plots work on simulation output", {
  out <- run_small_simulation()

  daily <- summarize_records(out$class_records)
  expect_equal(nrow(daily), length(unique(out$class_records$date)))

  final <- analyze_results(out)
  expect_equal(nrow(final), 1)

  expect_s3_class(plot1_plotly(out$class_records), "plotly")
})

test_that("seeded results do not change", {
  records <- run_small_simulation()$class_records
  last_day <- records[records$date == max(records$date), ]
  cumulative_columns <- grep("cumulative", names(last_day), value = TRUE)
  totals <- colSums(as.data.frame(last_day)[cumulative_columns])
  expect_snapshot_value(as.list(totals), style = "json2")
})

test_that("rbernoulli() matches its definition", {
  expected <- withr::with_seed(3, stats::runif(10) > (1 - 0.3))
  expect_equal(withr::with_seed(3, rbernoulli(10, p = 0.3)), expected)
})
