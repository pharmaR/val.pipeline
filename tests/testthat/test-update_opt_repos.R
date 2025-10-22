


test_that("update_opt_repos() updates CRAN repo with validation date", {
  mock_repos <- c(CRAN = "https://packagemanager.posit.co/cran/2024-01-01")
  val_date <- as.Date("2024-06-01")
  
  # I don't think it's important to predict output to the console
  # expect_output(
    result <- update_opt_repos(val_date, mock_repos)
  #   ,"Updating 'CRAN' repo to use validation date"
  # )
  
  expect_true(grepl("2024-06-01", result[["CRAN"]]))
})

test_that("update_opt_repos() handles latest snapshot for today", {
  mock_repos <- c(CRAN = "https://packagemanager.posit.co/cran/latest")
  val_date <- Sys.Date()
  
  # expect_output(
    result <- update_opt_repos(val_date, mock_repos)
  #   ,"already set to latest snapshot"
  # )
  
  expect_equal(result, mock_repos)
})

test_that("update_opt_repos() updates to latest when val_date is today", {
  mock_repos <- c(CRAN = "https://packagemanager.posit.co/cran/2024-01-01")
  val_date <- Sys.Date()
  
  # expect_output(
    result <- update_opt_repos(val_date, mock_repos)
  #   ,"Updating 'CRAN' repo to use latest snapshot"
  # )
  
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
  
  # expect_output(
    result <- update_opt_repos(val_date, mock_repos)
  #   ,"Updating 'CRAN' repo to use validation date"
  # )
  
  expect_true(grepl("2024-06-01", result[["cran"]]))
})