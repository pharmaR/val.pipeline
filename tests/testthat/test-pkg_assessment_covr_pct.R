# Regression tests for pkg_assessment_covr_pct() — #161 review.
#
# Ensures both call sites in val_pkg.R (the covr_skip_report gate at
# ~L538 and the meta_list construction at ~L946) resolve to NA_real_
# rather than throwing "Invalid index: field name 'covr_coverage'
# not found" for the shapes val_pkg() actually produces on the
# auto_accepted / reuse_init / remote_only paths.

.fresh_list_of <- function(...) {
  # riskmetric's assessment object is a vctrs::list_of, but building
  # one directly with mixed atomic/scalar/list elements is fragile
  # here — we care about the strict `$` behavior + downstream
  # extraction semantics, both of which are reproduced by a plain
  # list carrying the same class. `.subset2()` and `is.list()` —
  # the primitives our helper uses — behave identically.
  x <- list(...)
  class(x) <- c("list_of_pkg_metric", "vctrs_list_of",
                "vctrs_vctr", "list")
  x
}


test_that("pkg_assessment_covr_pct() returns NA_real_ for a fresh
          list_of_pkg_metric missing covr_coverage (auto_accepted /
          remote_only branch)", {
  pa <- .fresh_list_of(has_news = TRUE, license = "MIT")
  expect_identical(pkg_assessment_covr_pct(pa), NA_real_)

  # And the strict vctrs `$` semantics that this helper defends
  # against, in the shape they actually arise:
  real <- vctrs::list_of(has_news = TRUE, has_examples = TRUE)
  expect_error(real$covr_coverage, "Invalid index.*covr_coverage.*not found")
})


test_that("pkg_assessment_covr_pct() returns NA_real_ for a
          pkg_metric_na atomic (remote_only readRDS'd shape)", {
  pa <- .fresh_list_of(
    covr_coverage = structure(NA, class = c("pkg_metric_na", "pkg_metric"))
  )
  expect_identical(pkg_assessment_covr_pct(pa), NA_real_)
})


test_that("pkg_assessment_covr_pct() returns NA_real_ for a
          pkg_metric_error carrying an upstream $message", {
  err <- structure(
    list(message = "object 'res' not found",
         call = quote(pkg_ref_cache.covr_coverage.pkg_source(x, name))),
    class = c("pkg_metric_error", "pkg_metric_condition",
              "pkg_metric_covr_coverage", "pkg_metric",
              "simpleError", "error", "condition")
  )
  pa <- .fresh_list_of(covr_coverage = err)
  expect_identical(pkg_assessment_covr_pct(pa), NA_real_)
})


test_that("pkg_assessment_covr_pct() extracts a real numeric coverage
          percentage on the happy path", {
  cc <- list(totalcoverage = 84.56, filecoverage = c(a = 100, b = 80))
  class(cc) <- c("pkg_metric_covr_coverage", "pkg_metric")
  pa <- .fresh_list_of(covr_coverage = cc)
  expect_identical(pkg_assessment_covr_pct(pa), 84.56)
})


test_that("pkg_assessment_covr_pct() returns NA_real_ when totalcoverage
          is missing / non-numeric", {
  # totalcoverage absent
  cc <- list(filecoverage = 42)
  pa <- .fresh_list_of(covr_coverage = cc)
  expect_identical(pkg_assessment_covr_pct(pa), NA_real_)

  # totalcoverage a non-numeric string
  cc2 <- list(totalcoverage = "not-a-number")
  pa2 <- .fresh_list_of(covr_coverage = cc2)
  expect_identical(pkg_assessment_covr_pct(pa2), NA_real_)
})
