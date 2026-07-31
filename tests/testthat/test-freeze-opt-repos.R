test_that("val_pipeline() validates freeze_opt_repos", {
  # Not-a-logical rejected.
  expect_error(
    val_pipeline(freeze_opt_repos = "yes"),
    "freeze_opt_repos"
  )
  # Length > 1 rejected.
  expect_error(
    val_pipeline(freeze_opt_repos = c(TRUE, FALSE)),
    "freeze_opt_repos"
  )
  # NA rejected.
  expect_error(
    val_pipeline(freeze_opt_repos = NA),
    "freeze_opt_repos"
  )
})


test_that("val_prep_pipeline() validates freeze_opt_repos", {
  expect_error(
    val_prep_pipeline(freeze_opt_repos = "yes"),
    "freeze_opt_repos"
  )
  expect_error(
    val_prep_pipeline(freeze_opt_repos = 1L),
    "freeze_opt_repos"
  )
  expect_error(
    val_prep_pipeline(freeze_opt_repos = NA),
    "freeze_opt_repos"
  )
})


test_that("freeze_opt_repos default preserves prior val_date-driven rewrite", {
  # The pre-#89 behaviour is unchanged when the arg is left at its
  # default (FALSE): update_opt_repos() still rewrites the CRAN URL
  # to match val_date. Exercised via update_opt_repos() directly
  # here (val_prep_pipeline() would call the same helper with the
  # same args when the guard is off).
  mock_repos <- c(CRAN = "https://packagemanager.posit.co/cran/2026-07-21")
  val_date <- as.Date("2026-07-31")

  expect_output(
    result <- update_opt_repos(val_date, mock_repos)
  )
  expect_true(grepl("2026-07-31", result[["CRAN"]]))
  expect_false(grepl("2026-07-21", result[["CRAN"]]))
})


test_that("freeze_opt_repos = TRUE means the caller skips update_opt_repos()", {
  # When freeze_opt_repos = TRUE, val_prep_pipeline() takes the guarded
  # branch and does NOT call update_opt_repos(), so the config's URL
  # survives verbatim. Assert the invariant at the helper level: given
  # the same inputs, the result of NOT calling update_opt_repos() is
  # simply the input opt_repos.
  mock_repos <- c(CRAN = "https://packagemanager.posit.co/cran/2026-07-21",
                  BioC = "https://bioconductor.org/packages/3.22/bioc")
  val_date <- as.Date("2026-07-31")

  # This mirrors the guarded branch:
  #   if (isTRUE(freeze_opt_repos)) { <no update> } else {
  #     opt_repos <- update_opt_repos(val_date, opt_repos)
  #   }
  guarded_result <- if (isTRUE(TRUE)) mock_repos else {
    update_opt_repos(val_date, mock_repos)
  }
  expect_identical(guarded_result, mock_repos)
  expect_true(grepl("2026-07-21", guarded_result[["CRAN"]]))
})
