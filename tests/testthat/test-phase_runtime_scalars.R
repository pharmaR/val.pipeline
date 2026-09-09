# Tests for the per-phase runtime scalars added to the val_pkg() meta
# bundle in #177 (`assess_mins`, `skip_report_mins`). The scalars are
# computed inline in meta_list construction (R/val_pkg.R) from
# `get_pkg_timings()` so the summary report can render a phase-level
# breakdown in "Slowest packages" without re-parsing timings.csv.
#
# These tests exercise the small extraction expressions against a
# controlled `val_time_block()` state; they do NOT boot the full
# val_pkg() pipeline (which would require covr / testthat / a real
# package source).

# Verbatim copies of the inline expressions used in
# R/val_pkg.R meta_list construction. Kept here (rather than exported)
# because the logic is trivial and the test is really guarding the
# _shape_ contract with `get_pkg_timings()`. If either expression ever
# grows past a few lines, factor into a helper and drop this
# duplication.
compute_assess_mins <- function() {
  t <- get_pkg_timings()
  secs <- sum(unlist(t[c("assess_initial", "assess_final")]),
              na.rm = TRUE)
  if (isTRUE(secs > 0)) secs / 60 else NA_real_
}

compute_skip_report_mins <- function() {
  t <- get_pkg_timings()
  if (!is.null(t[["skip_report"]])) sum(t[["skip_report"]]) / 60
  else NA_real_
}

test_that("assess_mins sums assess_initial + assess_final phases", {
  reset_pkg_timings()
  # Seed the underlying option directly -- val_time_block() would
  # add real Sys.time() deltas which are hard to assert on exactly.
  options(val.pipeline.pkg_timings = list(
    download       = 1.0,
    untar          = 0.5,
    assess_initial = 12,   # 0.2 min
    assess_final   = 48,   # 0.8 min
    skip_report    = 30,   # 0.5 min
    decision       = 0.1,
    report         = 5
  ))
  expect_equal(compute_assess_mins(), 1.0)          # 12s + 48s = 60s
  expect_equal(compute_skip_report_mins(), 0.5)     # 30s = 0.5min
  reset_pkg_timings()
})

test_that("skip_report_mins is NA when the phase did not fire", {
  reset_pkg_timings()
  options(val.pipeline.pkg_timings = list(
    assess_initial = 6,
    assess_final   = 54
  ))
  expect_equal(compute_assess_mins(), 1.0)
  expect_true(is.na(compute_skip_report_mins()))
  reset_pkg_timings()
})

test_that("assess_mins is NA when neither assess phase captured", {
  # remote_only / reuse_init paths never fire assess_initial or
  # assess_final. The extraction has to return NA_real_ (not 0)
  # so the summary template's `is.na()` gate renders "-".
  reset_pkg_timings()
  options(val.pipeline.pkg_timings = list(
    download = 2,
    untar    = 1
  ))
  expect_true(is.na(compute_assess_mins()))
  expect_true(is.na(compute_skip_report_mins()))
  reset_pkg_timings()
})

test_that("val_time_block() populates timings map for both extractors", {
  # End-to-end sanity check that the real capture path
  # (val_time_block -> get_pkg_timings) yields a shape both
  # extractors accept. Uses tiny sleeps to guarantee non-zero deltas
  # without slowing the suite down meaningfully.
  reset_pkg_timings()
  val_time_block("assess_initial", Sys.sleep(0.01))
  val_time_block("assess_final",   Sys.sleep(0.01))
  val_time_block("skip_report",    Sys.sleep(0.01))

  a <- compute_assess_mins()
  s <- compute_skip_report_mins()

  expect_true(is.finite(a) && a > 0)
  expect_true(is.finite(s) && s > 0)
  reset_pkg_timings()
})
