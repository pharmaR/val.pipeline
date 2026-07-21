test_that("resolve_verbose() accepts every supported input form", {
  # Clean slate.
  op <- options(val.pipeline.verbose = NULL)
  on.exit(options(op), add = TRUE)

  expect_identical(resolve_verbose(NULL), "normal")

  options(val.pipeline.verbose = "minimal")
  expect_identical(resolve_verbose(NULL), "minimal")
  options(val.pipeline.verbose = NULL)

  # Logical shortcuts.
  expect_identical(resolve_verbose(TRUE), "normal")
  expect_identical(resolve_verbose(FALSE), "quiet")

  # Integer 0-3 tier codes.
  expect_identical(resolve_verbose(0L), "quiet")
  expect_identical(resolve_verbose(1L), "minimal")
  expect_identical(resolve_verbose(2L), "normal")
  expect_identical(resolve_verbose(3L), "verbose")
  expect_identical(resolve_verbose(1), "minimal")

  # Case-insensitive character names.
  expect_identical(resolve_verbose("QUIET"), "quiet")
  expect_identical(resolve_verbose("Verbose"), "verbose")
})


test_that("resolve_verbose() rejects invalid input with informative errors", {
  expect_error(resolve_verbose(9L), "Invalid `verbose` integer")
  expect_error(resolve_verbose("loud"), "quiet.*minimal.*normal.*verbose")
  expect_error(resolve_verbose(c("quiet", "normal")))
  expect_error(resolve_verbose(NA))
  expect_error(resolve_verbose(list("normal")))
})


test_that("val_verbosity_at_least() compares against the current tier", {
  op <- options(val.pipeline.verbose = "quiet")
  on.exit(options(op), add = TRUE)

  expect_false(val_verbosity_at_least("minimal"))
  expect_false(val_verbosity_at_least("normal"))
  expect_true(val_verbosity_at_least("quiet"))

  options(val.pipeline.verbose = "minimal")
  expect_true(val_verbosity_at_least("minimal"))
  expect_false(val_verbosity_at_least("normal"))

  options(val.pipeline.verbose = "verbose")
  expect_true(val_verbosity_at_least("minimal"))
  expect_true(val_verbosity_at_least("normal"))
  expect_true(val_verbosity_at_least("verbose"))
})


test_that("val_verbosity_at_least() rejects unknown tier names", {
  expect_error(val_verbosity_at_least("loud"))
  expect_error(val_verbosity_at_least(c("normal", "verbose")))
})


test_that("val_msg() gates output by tier and returns invisibly", {
  op <- options(val.pipeline.verbose = "quiet")
  on.exit(options(op), add = TRUE)

  # quiet: nothing prints
  expect_output(val_msg("hi\n", min_level = "minimal"), regexp = NA)
  expect_output(val_msg("hi\n", min_level = "normal"), regexp = NA)
  expect_output(val_msg("hi\n", min_level = "verbose"), regexp = NA)

  # minimal: only minimal prints
  options(val.pipeline.verbose = "minimal")
  expect_output(val_msg("A\n", min_level = "minimal"), "A")
  expect_output(val_msg("B\n", min_level = "normal"), regexp = NA)
  expect_output(val_msg("C\n", min_level = "verbose"), regexp = NA)

  # normal: minimal + normal, no verbose
  options(val.pipeline.verbose = "normal")
  expect_output(val_msg("A\n", min_level = "minimal"), "A")
  expect_output(val_msg("B\n", min_level = "normal"), "B")
  expect_output(val_msg("C\n", min_level = "verbose"), regexp = NA)

  # verbose: everything
  options(val.pipeline.verbose = "verbose")
  expect_output(val_msg("A\n", min_level = "minimal"), "A")
  expect_output(val_msg("B\n", min_level = "normal"), "B")
  expect_output(val_msg("C\n", min_level = "verbose"), "C")

  # returns invisibly
  expect_invisible(val_msg("x\n", min_level = "verbose"))
})


test_that("val_print() gates print output by tier", {
  op <- options(val.pipeline.verbose = "quiet")
  on.exit(options(op), add = TRUE)

  df <- data.frame(a = 1:2, b = c("x", "y"))

  expect_output(val_print(df, min_level = "normal"), regexp = NA)

  options(val.pipeline.verbose = "normal")
  expect_output(val_print(df, min_level = "normal"), "a b")
})


test_that("val_pkg_summary_line() formats compactly at minimal+, stays silent at quiet", {
  op <- options(val.pipeline.verbose = "minimal")
  on.exit(options(op), add = TRUE)

  out <- capture.output(
    val_pkg_summary_line("dplyr", "1.1.4", "Low", elapsed_secs = 12)
  )
  expect_length(out, 1L)
  # Timestamp bracket + decision + pkg + elapsed. No counter when pkg_idx
  # / pkg_total are NULL (standalone-call default).
  expect_match(out, "^\\s+\\[\\d{2}:\\d{2}\\]\\s+\\[Low\\]\\s+dplyr v1\\.1\\.4")
  expect_match(out, "\\(12s\\)$")
  expect_false(grepl("/", out))  # no counter

  # medium/high tags and suffix
  expect_output(
    val_pkg_summary_line("Rcpp", "1.0.13", "High",
                         elapsed_secs = NULL, suffix = "(dep-skip)"),
    "\\[High\\].+Rcpp v1\\.0\\.13.+\\(dep-skip\\)"
  )
  expect_output(
    val_pkg_summary_line("rlang", "1.1.4", "Low", suffix = "(cached)"),
    "\\(cached\\)"
  )

  # quiet: silent
  options(val.pipeline.verbose = "quiet")
  expect_output(
    val_pkg_summary_line("cli", "3.6.1", "Low", elapsed_secs = 5),
    regexp = NA
  )
})


test_that("val_pkg_summary_line() renders position-in-run counter when supplied", {
  op <- options(val.pipeline.verbose = "minimal")
  on.exit(options(op), add = TRUE)

  # Right-aligns the index to the total's character width so long
  # runs stay visually columnar.
  out1 <- capture.output(
    val_pkg_summary_line("dplyr", "1.1.4", "Low", elapsed_secs = 12,
                         pkg_idx = 1, pkg_total = 1195)
  )
  expect_match(out1, "\\(   1/1195\\)")

  out2 <- capture.output(
    val_pkg_summary_line("last", "9.9.9", "Low", elapsed_secs = 3,
                         pkg_idx = 1195, pkg_total = 1195)
  )
  expect_match(out2, "\\(1195/1195\\)")

  # Small totals get correspondingly narrow indices.
  out3 <- capture.output(
    val_pkg_summary_line("cli", "3.6.1", "Low", elapsed_secs = 5,
                         pkg_idx = 3, pkg_total = 7)
  )
  expect_match(out3, "\\(3/7\\)")

  # Partial input (only one of the two) omits the counter rather than
  # rendering something misleading like "(NA/10)".
  out4 <- capture.output(
    val_pkg_summary_line("x", "1", "Low",
                         pkg_idx = 3, pkg_total = NULL)
  )
  expect_false(grepl("/", out4))
})


test_that("val_pkg_summary_line() renders an abbreviated HH:MM timestamp", {
  op <- options(val.pipeline.verbose = "minimal")
  on.exit(options(op), add = TRUE)

  # Explicit POSIXct is honoured verbatim (in US/Eastern).
  ts <- as.POSIXct("2026-07-21 06:30:00", tz = "US/Eastern")
  out <- capture.output(
    val_pkg_summary_line("cli", "3.6.1", "Low", elapsed_secs = 5,
                         pkg_idx = 1, pkg_total = 10, timestamp = ts)
  )
  expect_match(out, "\\[06:30\\]")

  # Character(1) passes through verbatim, letting callers pre-format if
  # they want a different timezone.
  out2 <- capture.output(
    val_pkg_summary_line("cli", "3.6.1", "Low",
                         timestamp = "12:34")
  )
  expect_match(out2, "\\[12:34\\]")

  # NULL / non-POSIX defaults to Sys.time() in HH:MM.
  out3 <- capture.output(
    val_pkg_summary_line("cli", "3.6.1", "Low",
                         timestamp = NULL)
  )
  expect_match(out3, "\\[\\d{2}:\\d{2}\\]")
})


test_that("val_pkg_summary_line() tolerates bad elapsed_secs by omitting the runtime", {
  op <- options(val.pipeline.verbose = "minimal")
  on.exit(options(op), add = TRUE)

  # NA / negative / non-numeric all render as empty runtime tag, no crash.
  expect_output(
    val_pkg_summary_line("pkg", "1.0", "Low", elapsed_secs = NA_real_),
    "pkg v1\\.0"
  )
  expect_output(
    val_pkg_summary_line("pkg", "1.0", "Low", elapsed_secs = -1),
    "pkg v1\\.0"
  )
  expect_output(
    val_pkg_summary_line("pkg", "1.0", "Low", elapsed_secs = Inf),
    "pkg v1\\.0"
  )
})


test_that("apply_verbose() sets the option in the caller and restores it on exit", {
  op <- options(val.pipeline.verbose = "normal")
  on.exit(options(op), add = TRUE)

  f <- function(verbose) {
    apply_verbose(verbose)
    getOption("val.pipeline.verbose")
  }

  expect_identical(f("quiet"), "quiet")
  expect_identical(getOption("val.pipeline.verbose"), "normal")

  expect_identical(f("verbose"), "verbose")
  expect_identical(getOption("val.pipeline.verbose"), "normal")

  # A NULL verbose resolves against the current option, doesn't clobber it.
  expect_identical(f(NULL), "normal")
  expect_identical(getOption("val.pipeline.verbose"), "normal")
})


test_that("apply_verbose() propagates errors from bad verbose input", {
  op <- options(val.pipeline.verbose = "normal")
  on.exit(options(op), add = TRUE)

  f <- function(verbose) apply_verbose(verbose)
  expect_error(f("loud"))
  # Option still unchanged after failed call.
  expect_identical(getOption("val.pipeline.verbose"), "normal")
})
