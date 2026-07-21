
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

  # dependencies rendered one-per-line for readability.
  expect_true(any(txt == "dependencies = ["))
  expect_true(any(txt == "\t\"dplyr\","))
  expect_true(any(txt == "\t\"ggplot2\","))
  expect_true(any(txt == "\t\"rlang\""))
  expect_true(any(txt == "]"))
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
