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
  # After get_repo_origin() normalisation, every github-hosted source
  # (regardless of how it was labelled in the user's opt_repos config)
  # will land in qual_metadata's repo_name column as simply "github",
  # so all qualified github pkgs collapse into a single
  # qualified-github.txt file.
  qm <- data.frame(
    pkg            = c("admiral", "pharmaverseadam", "dplyr"),
    repo_name      = c("github",  "github",          "CRAN"),
    final_decision = c("Low", "Low", "Low"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low")

  expect_true(file.exists(file.path(out_dir, "qualified-github.txt")))
  expect_equal(
    readLines(file.path(out_dir, "qualified-github.txt")),
    c("admiral", "pharmaverseadam")
  )
})


test_that("write_qualified_pkg_lists() routes unknown / NA repo_name rows to qualified-NA.txt", {
  qm <- data.frame(
    pkg            = c("dplyr", "ghost1", "ghost2"),
    repo_name      = c("CRAN",  "unknown", NA_character_),
    final_decision = c("Low", "Low", "Low"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  expect_message(
    paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low"),
    "Routing 2 qualified pkg\\(s\\) with unknown repo_name to 'qualified-NA.txt'"
  )
  expect_named(paths, c("CRAN", "NA"))
  expect_equal(readLines(file.path(out_dir, "qualified-CRAN.txt")), "dplyr")
  expect_equal(readLines(file.path(out_dir, "qualified-NA.txt")),
               c("ghost1", "ghost2"))
  expect_false(file.exists(file.path(out_dir, "qualified-unknown.txt")))
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
  # Missing 'pkg' is unrecoverable.
  bad <- data.frame(final_decision = "Low", repo_name = "CRAN",
                    stringsAsFactors = FALSE)
  out_dir <- withr::local_tempdir()
  expect_error(
    write_qualified_pkg_lists(bad, out_dir, qualified_decision = "Low"),
    "missing required columns.*pkg"
  )
})


test_that("write_qualified_pkg_lists() reverse-engineers repo_name from 'repos' URLs when missing", {
  # Older qual_metadata.rds files don't carry repo_name — only the raw
  # install URL in 'repos'. The helper should recover the label by
  # matching URLs against the current session's getOption("repos").
  mock_repos <- c(
    CRAN = "https://cran.r-project.org",
    BioC = "https://bioconductor.org/packages/3.18/bioc"
  )
  old <- getOption("repos"); on.exit(options(repos = old))
  options(repos = mock_repos)

  qm <- data.frame(
    pkg            = c("dplyr", "limma", "yaml"),
    repos          = c("https://cran.r-project.org",
                       "https://bioconductor.org/packages/3.18/bioc",
                       "https://cran.r-project.org"),
    final_decision = c("Low", "Low", "Low"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  expect_message(
    paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low"),
    "reverse-engineering.*from the 'repos' URL"
  )
  expect_named(paths, c("BioC", "CRAN"))
  expect_equal(readLines(file.path(out_dir, "qualified-CRAN.txt")),
               c("dplyr", "yaml"))
  expect_equal(readLines(file.path(out_dir, "qualified-BioC.txt")),
               "limma")
})


test_that("write_qualified_pkg_lists() routes unresolvable 'repos' URLs to qualified-NA.txt", {
  # 'repos' URL doesn't match anything in getOption("repos") — the row
  # ends up as 'unknown' and gets folded into qualified-NA.txt so it
  # can't silently drop out of provisioning.
  mock_repos <- c(CRAN = "https://cran.r-project.org")
  old <- getOption("repos"); on.exit(options(repos = old))
  options(repos = mock_repos)

  qm <- data.frame(
    pkg            = c("dplyr", "ghost"),
    repos          = c("https://cran.r-project.org",
                       "https://long-gone-mirror.example.com"),
    final_decision = c("Low", "Low"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  # Two messages fire: one for reverse-engineering, one for NA routing.
  paths <- suppressMessages(
    write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low")
  )
  expect_named(paths, c("CRAN", "NA"))
  expect_equal(readLines(file.path(out_dir, "qualified-CRAN.txt")), "dplyr")
  expect_equal(readLines(file.path(out_dir, "qualified-NA.txt")), "ghost")
})


test_that("write_qualified_pkg_lists() errors when neither 'repo_name' nor 'repos' is present", {
  qm <- data.frame(
    pkg            = c("dplyr", "yaml"),
    final_decision = c("Low", "Low"),
    stringsAsFactors = FALSE
  )
  out_dir <- withr::local_tempdir()
  expect_error(
    write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low"),
    "neither a 'repo_name' nor a 'repos' column"
  )
})


test_that("write_qualified_pkg_lists() prefers 'repo_name' over 'repos' when both present", {
  # Even if 'repos' would resolve differently via getOption("repos"),
  # an explicitly-provided repo_name column should win — no
  # reverse-engineering, no unexpected message.
  mock_repos <- c(CRAN = "https://cran.r-project.org")
  old <- getOption("repos"); on.exit(options(repos = old))
  options(repos = mock_repos)

  qm <- data.frame(
    pkg            = "dplyr",
    repo_name      = "BioC",          # would be 'CRAN' if reverse-engineered
    repos          = "https://cran.r-project.org",
    final_decision = "Low",
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  expect_no_message(
    paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low"),
    message = "reverse-engineering"
  )
  expect_named(paths, "BioC")
  expect_true(file.exists(file.path(out_dir, "qualified-BioC.txt")))
  expect_false(file.exists(file.path(out_dir, "qualified-CRAN.txt")))
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
