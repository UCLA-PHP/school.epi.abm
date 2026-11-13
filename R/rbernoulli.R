#' Draw Bernoulli random variables
#'
#' Internal replacement for `purrr::rbernoulli()`, which purrr deprecated in
#' version 1.0.0. It uses the same algorithm, so simulations with a fixed seed
#' give the same results as before.
#'
#' @param n number of draws
#' @param p probability of `TRUE` for each draw
#'
#' @returns a logical vector of length `n`
#' @noRd
rbernoulli <- function(n, p = 0.5) {
  stats::runif(n) > (1 - p)
}
