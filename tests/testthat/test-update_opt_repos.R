


test_that("update_opt_repos() updates CRAN repo with validation date", {
  mock_repos <- c(CRAN = "https://packagemanager.posit.co/cran/2024-01-01")
  val_date <- as.Date("2024-06-01")
  
  expect_output(
    result <- update_opt_repos(val_date, mock_repos)
  )
  
  expect_true(grepl("2024-06-01", result[["CRAN"]]))
})

test_that("update_opt_repos() handles latest snapshot for today", {
  mock_repos <- c(CRAN = "https://packagemanager.posit.co/cran/latest")
  val_date <- Sys.Date()
  
  expect_output(
    result <- update_opt_repos(val_date, mock_repos)
    ,"already set to latest snapshot"
  )
  
  expect_equal(result, mock_repos)
})

test_that("update_opt_repos() updates to latest when val_date is today", {
  mock_repos <- c(CRAN = "https://packagemanager.posit.co/cran/2024-01-01")
  val_date <- Sys.Date()
  
  expect_output(
    result <- update_opt_repos(val_date, mock_repos)
    ,"Updating 'CRAN' repo to use latest snapshot"
  )
  
  expect_true(grepl("latest", result[["CRAN"]]))
})

test_that("update_opt_repos() handles repos without CRAN", {
  mock_repos <- c(BioCsoft = "https://bioconductor.org/packages/3.18/bioc")
  val_date <- as.Date("2024-06-01")
  
  result <- update_opt_repos(val_date, mock_repos)
  
  expect_equal(result, mock_repos)
})

test_that("update_opt_repos() handles case insensitive CRAN", {
  mock_repos <- c(cran = "https://packagemanager.posit.co/cran/2024-01-01")
  val_date <- as.Date("2024-06-01")
  
  expect_output(
    result <- update_opt_repos(val_date, mock_repos)
  )
  
  expect_true(grepl("2024-06-01", result[["cran"]]))
})

test_that("update_opt_repos() handles empty repos", {
  mock_repos <- c()
  val_date <- as.Date("2024-06-01")
  
  result <- update_opt_repos(val_date, mock_repos)
  
  expect_equal(result, mock_repos)
})

test_that("update_opt_repos() handles NULL repos", {
  mock_repos <- NULL
  val_date <- as.Date("2024-06-01")
  
  result <- update_opt_repos(val_date, mock_repos)
  
  expect_null(result)
})

# --- #140: PPM URLs with date-encoded slug ---

test_that("update_opt_repos() leaves PPM slug-with-date + /latest untouched", {
  # PPM frozen-mirror pattern: the slug encodes the snapshot date, and
  # `/latest` is the canonical tail. The URL must not be rewritten.
  mock_repos <- c(
    CRAN = "https://sce-ppm-test.arcusbio.com/cran-r4.5-2026-07-21/latest"
  )
  val_date <- as.Date("2026-07-21")

  expect_output(
    result <- update_opt_repos(val_date, mock_repos),
    "slug already encodes a snapshot date"
  )
  expect_equal(result[["CRAN"]], mock_repos[["CRAN"]])
})

test_that("update_opt_repos() leaves PPM slug URL alone even when val_date drifts", {
  # If the operator wrote a date-frozen slug into the config, treat it as
  # authoritative regardless of val_date drift. Users who want date-swap
  # behavior can drop the date from the slug or set freeze_opt_repos.
  mock_repos <- c(
    CRAN = "https://sce-ppm-test.arcusbio.com/cran-r4.5-2026-07-21/latest"
  )
  val_date <- as.Date("2026-08-15")

  expect_output(
    result <- update_opt_repos(val_date, mock_repos),
    "slug already encodes a snapshot date"
  )
  expect_equal(result[["CRAN"]], mock_repos[["CRAN"]])
})

test_that("update_opt_repos() never introduces a date into the slug segment", {
  # Regression: the old nested-gsub could produce
  # `.../cran-r4.5-2026-07-21/2026-07-21/src/contrib` by rewriting both
  # the slug date and the /latest tail.
  mock_repos <- c(
    CRAN = "https://sce-ppm-test.arcusbio.com/cran-r4.5-2026-07-21/latest"
  )
  val_date <- as.Date("2024-06-01")

  suppressMessages(
    result <- update_opt_repos(val_date, mock_repos)
  )
  # Should NOT contain a doubled date path segment
  expect_false(
    grepl("cran-r4\\.5-2026-07-21/\\d{4}-\\d{2}-\\d{2}", result[["CRAN"]])
  )
})

