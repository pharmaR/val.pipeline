
test_that("write_pipeline_toml() writes the expected TOML shape", {
  tmp <- withr::local_tempfile(fileext = ".toml")

  path <- write_pipeline_toml(
    pkgs      = c("dplyr", "ggplot2", "rlang"),
    opt_repos = c(
      CRAN = "https://packagemanager.posit.co/cran/2026-06-21",
      BioC = "https://bioconductor.org/packages/3.22/bioc"
    ),
    r_version = "4.5",
    name      = "unit-test",
    path      = tmp
  )

  expect_identical(path, tmp)
  expect_true(file.exists(tmp))

  txt <- readLines(tmp)

  # [project] header + name + r_version
  expect_true(any(txt == "[project]"))
  expect_true(any(grepl('^name = "unit-test"$', txt)))
  expect_true(any(grepl('^r_version = "4\\.5"$', txt)))

  # repositories serialized as an array-of-inline-tables, one entry
  # per line, with alias + url keys and a trailing comma after the
  # final entry (matches rv's expected format).
  expect_true(any(grepl("^repositories = \\[$", txt)))
  cran_line <- grep("alias = \"CRAN\"", txt, value = TRUE)
  bioc_line <- grep("alias = \"BioC\"", txt, value = TRUE)
  expect_length(cran_line, 1L)
  expect_length(bioc_line, 1L)
  expect_match(cran_line, "url = \"https://packagemanager")
  expect_match(bioc_line, "url = \"https://bioconductor")
  # Trailing comma on the last entry.
  expect_match(bioc_line, "\\},\\s*$")
  # Preserved input order (CRAN before BioC).
  expect_lt(
    which(txt == cran_line),
    which(txt == bioc_line)
  )

  # dependencies rendered one-per-line for readability, as inline
  # tables with `install_suggestions = true` (issue #148 — the new
  # default, so rv preinstalls Suggests up-front and downstream
  # `testthat::skip_if_not_installed()` guards stop silently
  # zeroing out coverage).
  expect_true(any(txt == "dependencies = ["))
  dep_lines <- grep("\\{ name = \"", txt, value = TRUE)
  expect_length(dep_lines, 3L)
  expect_match(dep_lines[1], "\\{ name = \"dplyr\"")
  expect_match(dep_lines[2], "\\{ name = \"ggplot2\"")
  expect_match(dep_lines[3], "\\{ name = \"rlang\"")
  # Every dep carries the install_suggestions field.
  expect_true(all(grepl("install_suggestions = true", dep_lines)))
  # Trailing comma on the last dep entry (mirrors the repositories
  # block so re-ordering / adding is a one-line diff).
  expect_match(dep_lines[3], "\\},\\s*$")
  expect_true(any(txt == "]"))
})


test_that("write_pipeline_toml(install_suggestions = FALSE) renders bare-string deps", {
  # Opt-out path preserved for smoke-test fixtures / callers that don't
  # want the Suggests bloat.
  tmp <- withr::local_tempfile(fileext = ".toml")
  write_pipeline_toml(
    pkgs      = c("dplyr", "ggplot2", "rlang"),
    opt_repos = c(
      CRAN = "https://packagemanager.posit.co/cran/2026-06-21"
    ),
    r_version = "4.5",
    name      = "unit-test",
    install_suggestions = FALSE,
    path      = tmp
  )
  txt <- readLines(tmp)

  expect_true(any(txt == "dependencies = ["))
  expect_true(any(txt == "\t\"dplyr\","))
  expect_true(any(txt == "\t\"ggplot2\","))
  expect_true(any(txt == "\t\"rlang\""))
  expect_true(any(txt == "]"))
  # No inline-table shape should appear anywhere in the dep block.
  expect_length(grep("\\{ name = \"dplyr\"", txt), 0L)
})


test_that("write_pipeline_toml() validates its inputs", {
  tmp <- withr::local_tempfile(fileext = ".toml")

  expect_error(
    write_pipeline_toml(
      pkgs = character(0),
      opt_repos = c(CRAN = "x"),
      path = tmp
    ),
    "non-empty character vector"
  )

  expect_error(
    write_pipeline_toml(
      pkgs = "dplyr",
      opt_repos = "no-names",
      path = tmp
    ),
    "named character vector"
  )

  expect_error(
    write_pipeline_toml(
      pkgs = "dplyr",
      opt_repos = c(CRAN = "x"),
      path = ""
    ),
    "non-empty string"
  )

  expect_error(
    write_pipeline_toml(
      pkgs = "dplyr",
      opt_repos = c(CRAN = "x"),
      install_suggestions = NA,
      path = tmp
    ),
    "TRUE or FALSE"
  )

  expect_error(
    write_pipeline_toml(
      pkgs = "dplyr",
      opt_repos = c(CRAN = "x"),
      install_suggestions = c(TRUE, FALSE),
      path = tmp
    ),
    "TRUE or FALSE"
  )
})


test_that("write_pipeline_toml() r_version defaults to current R major.minor", {
  tmp <- withr::local_tempfile(fileext = ".toml")
  write_pipeline_toml(
    pkgs = "dplyr",
    opt_repos = c(CRAN = "https://example.com"),
    path = tmp
  )
  expected <- paste(R.Version()$major, R.Version()$minor, sep = ".")
  txt <- readLines(tmp)
  expect_true(any(grepl(
    paste0('^r_version = "', gsub("\\.", "\\\\.", expected), '"$'),
    txt
  )))
})


test_that("write_pipeline_toml() prepends an unnamed local_repo as 'local'", {
  tmp <- withr::local_tempfile(fileext = ".toml")
  write_pipeline_toml(
    pkgs       = "dplyr",
    opt_repos  = c(
      CRAN = "https://packagemanager.posit.co/cran/2026-06-21",
      BioC = "https://bioconductor.org/packages/3.22/bioc"
    ),
    local_repo = "https://ppm.example.com/internal",
    path       = tmp
  )
  txt <- readLines(tmp)

  local_line <- grep("alias = \"local\"", txt, value = TRUE)
  cran_line  <- grep("alias = \"CRAN\"",  txt, value = TRUE)
  bioc_line  <- grep("alias = \"BioC\"",  txt, value = TRUE)
  expect_length(local_line, 1L)
  expect_match(local_line, "url = \"https://ppm\\.example\\.com/internal\"")
  # local must be first, then CRAN, then BioC
  expect_lt(which(txt == local_line), which(txt == cran_line))
  expect_lt(which(txt == cran_line),  which(txt == bioc_line))
})


test_that("write_pipeline_toml() uses the name of a named local_repo as its alias", {
  tmp <- withr::local_tempfile(fileext = ".toml")
  write_pipeline_toml(
    pkgs       = "dplyr",
    opt_repos  = c(CRAN = "https://packagemanager.posit.co/cran/2026-06-21"),
    local_repo = c(internal_ppm = "https://ppm.example.com/internal"),
    path       = tmp
  )
  txt <- readLines(tmp)
  local_line <- grep("alias = \"internal_ppm\"", txt, value = TRUE)
  cran_line  <- grep("alias = \"CRAN\"", txt, value = TRUE)
  expect_length(local_line, 1L)
  expect_match(local_line, "url = \"https://ppm\\.example\\.com/internal\"")
  expect_lt(which(txt == local_line), which(txt == cran_line))
  # 'local' should not appear when an alias was supplied.
  expect_length(grep("alias = \"local\"", txt), 0L)
})


test_that("write_pipeline_toml() validates local_repo", {
  tmp <- withr::local_tempfile(fileext = ".toml")
  expect_error(
    write_pipeline_toml(
      pkgs       = "dplyr",
      opt_repos  = c(CRAN = "https://example.com"),
      local_repo = c("a", "b"),
      path       = tmp
    ),
    "non-empty character\\(1\\)"
  )
  expect_error(
    write_pipeline_toml(
      pkgs       = "dplyr",
      opt_repos  = c(CRAN = "https://example.com"),
      local_repo = "",
      path       = tmp
    ),
    "non-empty character\\(1\\)"
  )
})


test_that("write_pipeline_toml() does not mutate the caller's opt_repos", {
  tmp <- withr::local_tempfile(fileext = ".toml")

  # Snapshot the caller's opt_repos before + after. The `local_repo`
  # prepend must happen only inside the emitted toml; the caller's
  # object must stay identical (same names, same values, same order).
  opt_repos <- c(
    CRAN = "https://packagemanager.posit.co/cran/2026-06-21",
    BioC = "https://bioconductor.org/packages/3.22/bioc"
  )
  before <- opt_repos

  write_pipeline_toml(
    pkgs       = "dplyr",
    opt_repos  = opt_repos,
    local_repo = "https://ppm.example.com/internal",
    path       = tmp
  )

  expect_identical(opt_repos, before)
  # And the local_repo alias must not have leaked into the caller's names.
  expect_false("local" %in% names(opt_repos))
})
