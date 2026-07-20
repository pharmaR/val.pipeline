

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


test_that("get_repo_origin normalises non-CRAN/BioC github sources to 'github'", {
  # Any user-defined label pointing at a github.com URL should collapse
  # to a single 'github' bucket so downstream consumers (e.g.
  # write_qualified_pkg_lists() writing qualified-<source>.txt files
  # for PPM) don't need to know about every user-defined GitHub label.
  mock_repos <- c(
    CRAN = "https://cran.r-project.org",
    github_pharmaverse = "https://github.com/pharmaverse",
    github_openpharma  = "https://github.com/openpharma"
  )
  old_repos <- getOption("repos"); on.exit(options(repos = old_repos)); options(repos = mock_repos)

  # names_only = TRUE returns the normalised label.
  expect_equal(
    get_repo_origin("https://github.com/pharmaverse", "admiral", names_only = TRUE),
    "github"
  )
  expect_equal(
    get_repo_origin("https://github.com/openpharma", "cardx", names_only = TRUE),
    "github"
  )

  # names_only = FALSE preserves the URL as the value and rewrites the
  # name to 'github'.
  full <- get_repo_origin("https://github.com/pharmaverse", "admiral")
  expect_equal(unname(full), "https://github.com/pharmaverse")
  expect_equal(names(full), "github")

  # A CRAN-labelled entry is left alone (no accidental github collapse).
  expect_equal(
    get_repo_origin("https://cran.r-project.org", "dplyr", names_only = TRUE),
    "CRAN"
  )
})


test_that("get_repo_origin does not normalise a repo explicitly labelled 'CRAN' or 'BioC' even if URL contains github", {
  # Guardrail: an admin who (perhaps unusually) labels their CRAN or BioC
  # mirror with a github-hosted URL should NOT have that source silently
  # rebadged as 'github'.
  mock_repos <- c(
    CRAN = "https://github.com/rstudio/cranmirror",
    BioC = "https://github.com/bioconductor/bioc_mirror"
  )
  old_repos <- getOption("repos"); on.exit(options(repos = old_repos)); options(repos = mock_repos)

  expect_equal(
    get_repo_origin("https://github.com/rstudio/cranmirror", "dplyr",
                    names_only = TRUE),
    "CRAN"
  )
  expect_equal(
    get_repo_origin("https://github.com/bioconductor/bioc_mirror", "limma",
                    names_only = TRUE),
    "BioC"
  )
})
