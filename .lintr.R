# Shared linter set from the lab's lms package, which lives in the lms/
# subdirectory of Morrison-Lab/psc. To lint this repo locally, first
# install it:
#
#   remotes::install_github(
#     "Morrison-Lab/psc/lms@f777ed8afcaf70a9054a7b5e19207270ef253623",
#     upgrade = "never",
#     build = FALSE
#   )
#
# then run pkgload::load_all() and lintr::lint_package().
if (!requireNamespace("lms", quietly = TRUE)) {
  stop(
    "Package 'lms' is required to lint this repo. Install it with:\n",
    "  remotes::install_github(\n",
    "    'Morrison-Lab/psc/lms@f777ed8afcaf70a9054a7b5e19207270ef253623',\n",
    "    upgrade = 'never',\n",
    "    build = FALSE\n",
    "  )",
    call. = FALSE
  )
}
linters <- lms::default_linters()

# Exclusions are repo-specific and stay local.
exclusions <- list(
  renv = Inf,
  dev = Inf,
  analyses = Inf,
  "inst/extdata" = Inf,
  "tests/testthat.R" = list(
    undesirable_function_linter = Inf
  )
)
