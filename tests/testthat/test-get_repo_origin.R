

test_that("get_repo_origin returns correct repo name when match found", {
  mock_repos <- c(CRAN = "https://cran.r-project.org", 
                  BioCsoft = "https://bioconductor.org/packages/3.18/bioc")
  
  old_repos <- getOption("repos"); on.exit(options(repos = old_repos)); options(repos = mock_repos)
  
  result <- get_repo_origin("https://cran.r-project.org", "testpkg")
  expect_equal(result, "https://cran.r-project.org" |> setNames("CRAN"))
})

test_that("get_repo_origin returns repo name when names_only = TRUE", {
  mock_repos <- c(CRAN = "https://cran.r-project.org", 
                  BioCsoft = "https://bioconductor.org/packages/3.18/bioc")
  
  old_repos <- getOption("repos"); on.exit(options(repos = old_repos)); options(repos = mock_repos)
  
  result <- get_repo_origin("https://cran.r-project.org", "testpkg", names_only = TRUE)
  expect_equal(result, "CRAN")
})



test_that("get_repo_origin returns 'unknown' when no match found", {
  mock_repos <- c(CRAN = "https://cran.r-project.org")
  
  old_repos <- getOption("repos"); on.exit(options(repos = old_repos)); options(repos = mock_repos)
  
  result <- get_repo_origin("https://unknown-repo.com", "testpkg")
  expect_equal(result, "unknown")
})

test_that("get_repo_origin handles multiple matches and shows warning", {
  mock_repos <- c(CRAN = "https://cran.r-project.org", 
                  CRAN2 = "https://cran.r-project.org")
  
  old_repos <- getOption("repos"); on.exit(options(repos = old_repos)); options(repos = mock_repos)
  
  expect_output(
    result <- get_repo_origin("https://cran.r-project.org", "testpkg"),
    "WARNING.*multiple repos"
  )
  expect_equal(result, "https://cran.r-project.org" |> setNames("CRAN"))
})

test_that("get_repo_origin handles NULL repo_src", {
  mock_repos <- c(CRAN = "https://cran.r-project.org")

  old_repos <- getOption("repos"); on.exit(options(repos = old_repos)); options(repos = mock_repos)
  
  result <- get_repo_origin(NULL, "testpkg")
  expect_equal(result, "unknown")
})

test_that("get_repo_origin handles empty repos option", {
  mock_repos <- c()
  old_repos <- getOption("repos"); on.exit(options(repos = old_repos)); options(repos = mock_repos)
  
  result <- get_repo_origin(
    repo_src = "https://cran.r-project.org", pkg_name =  "testpkg")
  expect_equal(result, "unknown")
})

test_that("get_repo_origin works with Posit Package Manager URLs", {
  mock_repos <- c(CRAN = "https://packagemanager.posit.co/cran/2024-06-01")
  
  old_repos <- getOption("repos"); on.exit(options(repos = old_repos)); options(repos = mock_repos)
  
  result <- get_repo_origin("https://packagemanager.posit.co/cran/2024-06-01", "testpkg")
  expect_equal(result, "https://packagemanager.posit.co/cran/2024-06-01" |> setNames("CRAN"))
  
  result_names <- get_repo_origin("https://packagemanager.posit.co/cran/2024-06-01", "testpkg", names_only = TRUE)
  expect_equal(result_names, "CRAN")
})
