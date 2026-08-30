# Tests for the subprocess isolation and skip_pkgs config path added
# in #159.

test_that("capture_covr_skip_report(subprocess = FALSE) matches the
          subprocess = TRUE result for a well-behaved pkg", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
  writeLines(c(
    "Package: skipdemosub", "Type: Package", "Title: A Demo",
    "Version: 0.0.0.9000",
    "Description: For test-covr_skip_report_isolation tests.",
    "License: MIT + file LICENSE", "Encoding: UTF-8",
    "Suggests: testthat"
  ), file.path(root, "DESCRIPTION"))
  writeLines("# empty", file.path(root, "NAMESPACE"))
  writeLines(c('library(testthat)', 'test_check("skipdemosub")'),
             file.path(root, "tests", "testthat.R"))
  writeLines(c(
    'test_that("p1", { expect_true(TRUE) })',
    'test_that("s1", { skip("nope") })',
    'test_that("s2", { skip("nope") })'
  ), file.path(root, "tests", "testthat", "test-x.R"))

  in_proc <- capture_covr_skip_report(root, subprocess = FALSE)
  sub_proc <- capture_covr_skip_report(root, subprocess = TRUE)

  expect_identical(in_proc$totals, sub_proc$totals)
  expect_identical(in_proc$top_reasons, sub_proc$top_reasons)
})


test_that("capture_covr_skip_report(subprocess = TRUE) returns NULL when
          the child crashes / errors, without propagating", {
  skip_if_not_installed("callr")
  # Point at a source path that doesn't exist. The child sees the
  # missing tests dir before the impl runs and returns NULL. Front
  # door catches that and returns NULL cleanly.
  res <- capture_covr_skip_report("/definitely/not/a/pkg",
                                  subprocess = TRUE)
  expect_null(res)
})


test_that("pull_covr_skip_report_config() reads skip_pkgs from config and
          option", {
  # Default config: skip_pkgs contains `np` (seeded in #159).
  cfg <- pull_covr_skip_report_config()
  expect_true("np" %in% cfg$skip_pkgs)

  # R-option override wins over config.
  withr::with_options(
    list(val.pipeline.covr_skip_report_skip_pkgs = c("foo", "bar")),
    {
      cfg <- pull_covr_skip_report_config()
      expect_setequal(cfg$skip_pkgs, c("foo", "bar"))
    }
  )

  # Empty override clears the list.
  withr::with_options(
    list(val.pipeline.covr_skip_report_skip_pkgs = character(0)),
    {
      cfg <- pull_covr_skip_report_config()
      expect_length(cfg$skip_pkgs, 0L)
    }
  )
})


test_that("pull_covr_skip_report_config() strips NA / empty entries and
          rejects non-character skip_pkgs", {
  withr::with_options(
    list(val.pipeline.covr_skip_report_skip_pkgs = c("np", NA, "", "foo")),
    {
      cfg <- pull_covr_skip_report_config()
      expect_setequal(cfg$skip_pkgs, c("np", "foo"))
    }
  )

  withr::with_options(
    list(val.pipeline.covr_skip_report_skip_pkgs = c(1, 2, 3)),
    {
      expect_error(
        pull_covr_skip_report_config(),
        "must be a character vector"
      )
    }
  )
})
