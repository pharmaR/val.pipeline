# Guard tests for the post-assessment collation refactor in val_build() (#69).
#
# val_build() collates per-package `_assess_record.rds` files into one big
# frame (-> qual_assessments.rds) and per-package meta bundles into another
# (-> qual_metadata0.rds). The refactor swapped `purrr::reduce(bind_rows)`
# (O(n^2)) for a single `dplyr::bind_rows(list_of_frames)` (O(n)). These
# tests pin the equivalence so a future well-meaning refactor can't
# silently break byte-identical output.

# ---- helpers ---------------------------------------------------------------

# Mimic the shape of a single `_assess_record.rds` payload: a 1-row
# data.frame with numeric / character / list columns. Kept intentionally
# narrow (5 cols) to keep the test readable; the equivalence property does
# not depend on column count.
make_fake_assess_record <- function(pkg, ver, downloads = 100L) {
  data.frame(
    package = pkg,
    version = ver,
    val_date = as.Date("2026-01-01"),
    downloads_1yr = downloads,
    r_cmd_check_warnings = 0L,
    stringsAsFactors = FALSE
  )
}

# Mimic the shape of a single `_meta.rds` payload: a named list whose
# `depends` / `suggests` / `rev_deps` / `sys_info` slots are wrapped into
# list-columns by the val_build map body before bind_rows.
make_fake_pkg_meta <- function(pkg, ver,
                               decision = "Low",
                               depends = c("methods", "utils"),
                               suggests = NA_character_,
                               rev_deps = character(0)) {
  list(
    pkg = pkg,
    ver = ver,
    r_ver = getRversion(),
    sys_info = list(R.Version()),
    repos = "CRAN",
    val_date = as.Date("2026-01-01"),
    ref = "source",
    metric_pkg = "riskmetric",
    decision = decision,
    decision_reason = "Primary",
    decision_reason_note = NA_character_,
    final_decision = decision,
    final_decision_reason = "Primary",
    final_decision_reason_note = NA_character_,
    depends = if (length(depends) == 0L) NA_character_ else depends,
    suggests = if (length(suggests) == 0L) NA_character_ else suggests,
    rev_deps = if (length(rev_deps) == 0L) NA_character_ else rev_deps,
    assessment_runtime = list(txt = "0.1 secs", mins = 0.001)
  )
}

# Verbatim copy of the per-element transform in val_build()'s meta
# collation block. Kept here (rather than exported) because the test
# needs to reproduce the pipeline exactly to prove the O(n) and O(n^2)
# bind_rows strategies land on the same frame; wrapping this into a
# helper called from both val_build and here would be a bigger diff
# than the fix warrants (#69).
bundle_row <- function(.x) {
  x <- purrr::list_flatten(.x)
  x$depends <- list(x$depends)
  x$suggests <- list(x$suggests)
  x$rev_deps <- list(x$rev_deps)
  x$sys_info <- list(x$sys_info)
  dplyr::as_tibble(x)
}

# ---- assessment_bundle equivalence ----------------------------------------

test_that("bind_rows(list) matches reduce(bind_rows) for assess_record frames", {
  records <- list(
    make_fake_assess_record("askpass",  "1.2.0", downloads = 5000L),
    make_fake_assess_record("cli",      "3.6.4", downloads = 250000L),
    make_fake_assess_record("dplyr",    "1.1.4", downloads = 900000L)
  )

  # Old (O(n^2)) path
  old <- purrr::reduce(records, dplyr::bind_rows)
  # New (O(n)) path — what val_build() now uses.
  new <- dplyr::bind_rows(records)

  expect_identical(new, old)
  expect_equal(nrow(new), length(records))
  expect_equal(colnames(new), colnames(records[[1]]))
})

test_that("bind_rows(list) matches reduce(bind_rows) for varied-column frames", {
  # Simulate a case where some records have an extra column (as can happen
  # when riskmetric adds a new metric mid-run): bind_rows fills NA. Both
  # strategies must agree on the padded result.
  records <- list(
    make_fake_assess_record("a", "1.0.0"),
    cbind(make_fake_assess_record("b", "1.0.0"),
          extra_metric = 42L),
    make_fake_assess_record("c", "1.0.0")
  )

  old <- purrr::reduce(records, dplyr::bind_rows)
  new <- dplyr::bind_rows(records)

  expect_identical(new, old)
  expect_true("extra_metric" %in% colnames(new))
  expect_true(is.na(new$extra_metric[[1]]))
  expect_equal(new$extra_metric[[2]], 42L)
  expect_true(is.na(new$extra_metric[[3]]))
})

# ---- pkgs_df0 (meta) equivalence -------------------------------------------

test_that("bind_rows(list) matches reduce(bind_rows) for pkg_bundle tibbles", {
  pkg_bundles <- list(
    askpass = make_fake_pkg_meta("askpass", "1.2.0"),
    dplyr   = make_fake_pkg_meta("dplyr",   "1.1.4",
                                 depends = c("methods", "R6"),
                                 suggests = c("testthat", "knitr")),
    failed  = make_fake_pkg_meta("failed",  "0.1.0",
                                 decision = "High",
                                 rev_deps = c("askpass", "cli"))
  )

  rows <- purrr::map(pkg_bundles, bundle_row)

  # Old (O(n^2)) path — the pattern val_build() used before #69.
  old <- purrr::reduce(rows, dplyr::bind_rows)
  # New (O(n)) path — what val_build() now uses.
  new <- dplyr::bind_rows(rows)

  expect_identical(new, old)
  expect_equal(nrow(new), length(pkg_bundles))
  # List columns preserved on both sides.
  expect_type(new$depends,  "list")
  expect_type(new$suggests, "list")
  expect_type(new$rev_deps, "list")
  expect_type(new$sys_info, "list")
  # Content of a list column round-trips per row.
  expect_identical(new$depends[[2]], c("methods", "R6"))
  expect_identical(new$rev_deps[[3]], c("askpass", "cli"))
})

test_that("bind_rows(list) matches reduce(bind_rows) on 1-element bundle list", {
  # Regression guard: a single-package run should behave identically under
  # both strategies. purrr::reduce on length-1 input returns the element
  # itself; bind_rows on a length-1 list of tibbles returns the tibble.
  # These must be equal for byte-identical qual_metadata0.rds output.
  pkg_bundles <- list(only = make_fake_pkg_meta("only", "1.0.0"))
  rows <- purrr::map(pkg_bundles, bundle_row)

  old <- purrr::reduce(rows, dplyr::bind_rows)
  new <- dplyr::bind_rows(rows)

  expect_identical(new, old)
  expect_equal(nrow(new), 1L)
})

# ---- val_build() empty-input guard ----------------------------------------

test_that("val_build() stops with actionable msg when no assess_records exist", {
  # After #69 an empty `assessed/` directory is now caught with a targeted
  # stop() rather than allowed to fall through and write an empty RDS.
  # (Prior to the fix, `purrr::reduce(list(), dplyr::bind_rows)` errored
  # with `Must supply \`.init\` when \`.x\` is empty`, which was cryptic.)
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_val_build_empty_assessed")
  if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)

  err <- tryCatch(
    val_build(
      pkg_names = c("nonexistent_pkg_zzz"),
      ref = "remote",
      metric_pkg = "riskmetric",
      deps = NULL,
      val_date = as.Date("2024-01-01"),
      out = test_dir
    ),
    error = function(e) e
  )

  # We only assert on the shape of the message when we actually got as far
  # as the collation step (i.e. dirs were created). If some earlier failure
  # (e.g. no network for available.packages()) short-circuits us we skip.
  r_ver <- getRversion()
  reached_collate <- dir.exists(file.path(test_dir, paste0("R_", r_ver)))
  skip_if_not(reached_collate,
              "val_build() failed before reaching the collation step")

  expect_s3_class(err, "error")
  expect_match(
    conditionMessage(err),
    "No `_assess_record.rds` files found",
    fixed = FALSE
  )
})
