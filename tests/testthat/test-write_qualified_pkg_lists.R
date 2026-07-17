test_that("write_qualified_pkg_lists() writes one file per source with one pkg per line", {
  qm <- data.frame(
    pkg            = c("dplyr", "ggplot2", "limma",  "edgeR", "yaml", "waldo"),
    repo_name      = c("CRAN",  "CRAN",    "BioC",   "BioC",  "CRAN", "CRAN"),
    final_decision = c("Low",   "Low",     "Low",    "Low",   "Low",  "High"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low")

  expect_named(paths, c("BioC", "CRAN"))  # sorted
  expect_true(all(file.exists(paths)))

  cran <- readLines(file.path(out_dir, "qualified-CRAN.txt"))
  bioc <- readLines(file.path(out_dir, "qualified-BioC.txt"))

  # Alphabetised, deduplicated, 'High' rows excluded.
  expect_equal(cran, c("dplyr", "ggplot2", "yaml"))
  expect_equal(bioc, c("edgeR", "limma"))
})


test_that("write_qualified_pkg_lists() handles GitHub sources", {
  qm <- data.frame(
    pkg            = c("admiral", "pharmaverseadam", "dplyr"),
    repo_name      = c("github_pharmaverse", "github_pharmaverse", "CRAN"),
    final_decision = c("Low", "Low", "Low"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low")

  expect_true(file.exists(file.path(out_dir, "qualified-github_pharmaverse.txt")))
  expect_equal(
    readLines(file.path(out_dir, "qualified-github_pharmaverse.txt")),
    c("admiral", "pharmaverseadam")
  )
})


test_that("write_qualified_pkg_lists() skips unknown / NA repo_name rows", {
  qm <- data.frame(
    pkg            = c("dplyr", "ghost1", "ghost2"),
    repo_name      = c("CRAN",  "unknown", NA_character_),
    final_decision = c("Low", "Low", "Low"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  expect_message(
    paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low"),
    "Skipping 2 qualified pkg\\(s\\) with unknown repo_name"
  )
  expect_named(paths, "CRAN")
  expect_equal(readLines(file.path(out_dir, "qualified-CRAN.txt")), "dplyr")
  expect_false(file.exists(file.path(out_dir, "qualified-unknown.txt")))
  expect_false(file.exists(file.path(out_dir, "qualified-NA.txt")))
})


test_that("write_qualified_pkg_lists() returns character(0) when nothing qualifies", {
  qm <- data.frame(
    pkg            = c("dplyr", "ggplot2"),
    repo_name      = c("CRAN",  "CRAN"),
    final_decision = c("High", "Medium"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  expect_message(
    paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low"),
    "No qualified packages found"
  )
  expect_identical(paths, character(0))
  expect_length(list.files(out_dir), 0L)
})


test_that("write_qualified_pkg_lists() creates out_dir if it doesn't exist", {
  qm <- data.frame(
    pkg = "dplyr", repo_name = "CRAN", final_decision = "Low",
    stringsAsFactors = FALSE
  )

  tmp_root <- withr::local_tempdir()
  new_dir <- file.path(tmp_root, "fresh", "nested", "dir")
  expect_false(dir.exists(new_dir))

  paths <- write_qualified_pkg_lists(qm, new_dir, qualified_decision = "Low")
  expect_true(dir.exists(new_dir))
  expect_true(file.exists(file.path(new_dir, "qualified-CRAN.txt")))
})


test_that("write_qualified_pkg_lists() errors on missing required columns", {
  bad <- data.frame(pkg = "dplyr", final_decision = "Low",
                    stringsAsFactors = FALSE)
  out_dir <- withr::local_tempdir()
  expect_error(
    write_qualified_pkg_lists(bad, out_dir, qualified_decision = "Low"),
    "missing required columns.*repo_name"
  )
})


test_that("write_qualified_pkg_lists() writes files with no trailing header / quoting / blank line", {
  qm <- data.frame(
    pkg = c("dplyr", "ggplot2"),
    repo_name = c("CRAN", "CRAN"),
    final_decision = c("Low", "Low"),
    stringsAsFactors = FALSE
  )
  out_dir <- withr::local_tempdir()
  write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low")

  raw <- readLines(file.path(out_dir, "qualified-CRAN.txt"))
  expect_equal(raw, c("dplyr", "ggplot2"))
  expect_false(any(grepl("\"", raw)))
  expect_false(any(grepl("^\\s*$", raw)))
})
