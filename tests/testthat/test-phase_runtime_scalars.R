# Tests for the per-phase runtime helpers used by val_pkg()'s meta
# bundle construction in #177 (`pkg_assess_mins`, `pkg_skip_report_mins`
# in R/verbosity.R). Adopted from PR #178 review: the previous version
# of this file exercised copies of the extraction expressions rather
# than the production helpers, so a regression in val_pkg.R's real
# meta_list slots would have left every assertion green.

test_that("pkg_assess_mins() sums assess_initial + assess_final phases", {
  t <- list(
    download       = 1.0,
    untar          = 0.5,
    assess_initial = 12,   # 12s
    assess_final   = 48,   # 48s -> 60s total = 1.0 min
    skip_report    = 30,   # 30s = 0.5 min
    decision       = 0.1,
    report         = 5
  )
  expect_equal(pkg_assess_mins(t), 1.0)
  expect_equal(pkg_skip_report_mins(t), 0.5)
})

test_that("pkg_skip_report_mins() is NA when the phase did not fire", {
  t <- list(
    assess_initial = 6,
    assess_final   = 54
  )
  expect_equal(pkg_assess_mins(t), 1.0)
  expect_true(is.na(pkg_skip_report_mins(t)))
})

test_that("pkg_assess_mins() is NA when neither assess phase captured", {
  # remote_only / reuse_init paths never fire assess_initial or
  # assess_final. Both helpers must return NA_real_ (not 0) so the
  # summary template's `is.na()` gate renders "-".
  t <- list(
    download = 2,
    untar    = 1
  )
  expect_true(is.na(pkg_assess_mins(t)))
  expect_true(is.na(pkg_skip_report_mins(t)))
})

test_that("phase helpers tolerate non-list / empty input", {
  # `get_pkg_timings()` normalises to list() on unexpected shapes, but
  # test the coercion here too so a future refactor can't silently
  # blow up on NULL / character input.
  expect_true(is.na(pkg_assess_mins(NULL)))
  expect_true(is.na(pkg_skip_report_mins(NULL)))
  expect_true(is.na(pkg_assess_mins("nope")))
  expect_true(is.na(pkg_skip_report_mins("nope")))
  expect_true(is.na(pkg_assess_mins(list())))
  expect_true(is.na(pkg_skip_report_mins(list())))
})

test_that("val_time_block() + get_pkg_timings() feed the helpers end-to-end", {
  # End-to-end sanity check that the real capture path
  # (val_time_block -> get_pkg_timings) yields a shape both helpers
  # accept. Uses tiny sleeps to guarantee non-zero deltas without
  # slowing the suite down meaningfully.
  reset_pkg_timings()
  val_time_block("assess_initial", Sys.sleep(0.01))
  val_time_block("assess_final",   Sys.sleep(0.01))
  val_time_block("skip_report",    Sys.sleep(0.01))

  t <- get_pkg_timings()
  a <- pkg_assess_mins(t)
  s <- pkg_skip_report_mins(t)

  expect_true(is.finite(a) && a > 0)
  expect_true(is.finite(s) && s > 0)
  reset_pkg_timings()
})

test_that("val_pkg() meta_list wires the phase helpers into scalars", {
  # Guard against a copy-paste refactor of val_pkg.R that swaps
  # `pkg_assess_mins(get_pkg_timings())` for something else. Rather
  # than boot the full val_pkg() pipeline (which needs covr, a real
  # tarball, ...), we assert on the *source* -- the meta_list slot
  # names are what feed downstream `val_finalize()`'s list_flatten
  # into the `qual_metadata.rds` columns the summary template reads.
  # If either slot name drifts, the summary "Slowest packages" table
  # silently loses its Assess / Skip report columns.
  src_path <- testthat::test_path("..", "..", "R", "val_pkg.R")
  skip_if_not(file.exists(src_path),
              "R/val_pkg.R not reachable (installed-pkg test run)")
  meta_body <- paste(readLines(src_path), collapse = "\n")
  expect_match(meta_body,
               "assess_mins\\s*=\\s*pkg_assess_mins\\(get_pkg_timings\\(\\)\\)",
               fixed = FALSE)
  expect_match(meta_body,
               paste0("skip_report_mins\\s*=\\s*pkg_skip_report_mins",
                      "\\(get_pkg_timings\\(\\)\\)"),
               fixed = FALSE)
})
