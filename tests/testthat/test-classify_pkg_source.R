# Helper: write a minimal DESCRIPTION file to a tempdir and return the path.
write_desc <- function(fields) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(d, "DESCRIPTION")
  lines <- vapply(names(fields),
                  function(k) paste0(k, ": ", fields[[k]]),
                  character(1))
  writeLines(lines, path)
  path
}


test_that("classify_pkg_source() returns URL match when opt_repos resolves it (step 1 wins)", {
  mock_repos <- c(
    CRAN = "https://cran.r-project.org",
    BioC = "https://bioconductor.org/packages/3.18/bioc"
  )
  old <- getOption("repos"); on.exit(options(repos = old))
  options(repos = mock_repos)

  # URL match succeeds — DESCRIPTION should NOT be consulted, even if
  # it disagrees.
  misleading_desc <- write_desc(list(
    Package    = "trickypkg",
    biocViews  = "Software"          # would say 'BioC' but URL says CRAN
  ))

  expect_equal(
    classify_pkg_source(
      repo_src  = "https://cran.r-project.org",
      pkg_name  = "trickypkg",
      desc_path = misleading_desc
    ),
    "CRAN"
  )
})


test_that("classify_pkg_source() falls back to DESCRIPTION when URL match is 'unknown'", {
  mock_repos <- c(merged = "https://packagemanager.posit.co/validated/latest")
  old <- getOption("repos"); on.exit(options(repos = old))
  options(repos = mock_repos)

  # This is the merged-PPM scenario: URL for the individual pkg
  # doesn't line up with any known repo entry.
  bioc_desc <- write_desc(list(
    Package    = "limma",
    biocViews  = "Software, DifferentialExpression"
  ))

  expect_equal(
    classify_pkg_source(
      repo_src  = "https://github.com/some/mirror",  # won't match
      pkg_name  = "limma",
      desc_path = bioc_desc
    ),
    "BioC"
  )
})


test_that("classify_pkg_source() detects CRAN via 'Repository: CRAN' in DESCRIPTION", {
  mock_repos <- NULL
  old <- getOption("repos"); on.exit(options(repos = old))
  options(repos = mock_repos)  # force URL step to bail

  desc <- write_desc(list(
    Package    = "dplyr",
    Repository = "CRAN"
  ))

  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "dplyr", desc_path = desc),
    "CRAN"
  )
})


test_that("classify_pkg_source() detects github via 'RemoteType: github'", {
  desc <- write_desc(list(
    Package    = "admiral",
    RemoteType = "github",
    RemoteHost = "api.github.com",
    RemoteRepo = "admiral"
  ))

  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "admiral",
                        desc_path = desc),
    "github"
  )
})


test_that("classify_pkg_source() detects github via URL/BugReports as a last resort", {
  desc_via_url <- write_desc(list(
    Package    = "somepkg",
    URL        = "https://github.com/someowner/somepkg"
  ))
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "somepkg",
                        desc_path = desc_via_url),
    "github"
  )

  desc_via_bug <- write_desc(list(
    Package    = "otherpkg",
    BugReports = "https://github.com/someowner/otherpkg/issues"
  ))
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "otherpkg",
                        desc_path = desc_via_bug),
    "github"
  )
})


test_that("classify_pkg_source() prefers Repository:CRAN over github URL/BugReports", {
  # Many CRAN pkgs list a github URL for issue tracking. This must NOT
  # cause them to be misclassified as 'github'.
  desc <- write_desc(list(
    Package    = "dplyr",
    Repository = "CRAN",
    URL        = "https://github.com/tidyverse/dplyr",
    BugReports = "https://github.com/tidyverse/dplyr/issues"
  ))
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "dplyr",
                        desc_path = desc),
    "CRAN"
  )
})


test_that("classify_pkg_source() prefers biocViews over Repository / RemoteType / URL signals", {
  desc <- write_desc(list(
    Package    = "limma",
    biocViews  = "Software",
    Repository = "CRAN",                 # ignored because biocViews wins
    RemoteType = "github",               # ignored
    URL        = "https://github.com/x/limma"
  ))
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "limma",
                        desc_path = desc),
    "BioC"
  )
})


test_that("classify_pkg_source() returns 'unknown' when nothing resolves", {
  # No URL, DESCRIPTION with no useful fields.
  desc <- write_desc(list(
    Package = "opaque",
    Title   = "An unclassifiable package"
  ))
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "opaque",
                        desc_path = desc),
    "unknown"
  )
})


test_that("classify_pkg_source() handles missing / unreadable DESCRIPTION gracefully", {
  # No desc_path.
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "x", desc_path = NULL),
    "unknown"
  )
  # Non-existent file.
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "x",
                        desc_path = tempfile()),
    "unknown"
  )
  # Empty string.
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "x", desc_path = ""),
    "unknown"
  )
})


test_that("classify_pkg_source() accepts a directory path (looks for DESCRIPTION inside)", {
  dir <- withr::local_tempdir()
  writeLines(c("Package: fromdir", "biocViews: Software"),
             file.path(dir, "DESCRIPTION"))
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "fromdir",
                        desc_path = dir),
    "BioC"
  )
})


test_that("classify_pkg_source() ignores empty biocViews / Repository fields", {
  desc <- write_desc(list(
    Package    = "shell",
    biocViews  = "",
    Repository = "",
    URL        = "https://gitlab.com/user/shell"  # not github
  ))
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "shell",
                        desc_path = desc),
    "unknown"
  )
})


test_that("classify_pkg_source() only matches 'github.com/<owner>/<repo>', not stray 'github' substrings", {
  desc <- write_desc(list(
    Package    = "mysteryX",
    URL        = "https://internal.example.com/github-like-page"
  ))
  # 'github' appears as a substring but not as github.com/<owner>/<repo>.
  expect_equal(
    classify_pkg_source(repo_src = NULL, pkg_name = "mysteryX",
                        desc_path = desc),
    "unknown"
  )
})
