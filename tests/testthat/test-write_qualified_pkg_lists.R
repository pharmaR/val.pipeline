test_that("write_qualified_pkg_lists() writes one file per source with one pkg per line", {
  qm <- data.frame(
    pkg            = c("dplyr", "ggplot2", "limma",  "edgeR", "yaml", "waldo"),
    repo_name      = c("CRAN",  "CRAN",    "BioC",   "BioC",  "CRAN", "CRAN"),
    final_decision = c("Low",   "Low",     "Low",    "Low",   "Low",  "High"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low",
                                     blocklist_sources = character())

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
    paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low",
                                       blocklist_sources = character()),
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
    paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low",
                                       blocklist_sources = character()),
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


test_that("write_qualified_pkg_lists() emits blocklist-<src>.txt for blocklist_sources", {
  # For a source in blocklist_sources, the file lists assessed pkgs
  # whose final_decision != qualified_decision (or is NA). Qualified
  # pkgs from that source are dropped -- PPM will mirror the whole
  # upstream source and use the blocklist to exclude the rest.
  qm <- data.frame(
    pkg            = c("dplyr", "ggplot2", "limma",  "edgeR",  "GenomeInfoDb", "someBiocPkg"),
    repo_name      = c("CRAN",  "CRAN",    "BioC",   "BioC",   "BioC",         "BioC"),
    final_decision = c("Low",   "Low",     "Low",    "High",   "Medium",       NA_character_),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  paths <- write_qualified_pkg_lists(qm, out_dir,
                                     qualified_decision = "Low",
                                     blocklist_sources = "BioC")

  expect_named(paths, c("BioC", "CRAN"))
  # BioC file is a blocklist of non-qualified pkgs; qualified 'limma' excluded.
  expect_equal(basename(paths[["BioC"]]), "blocklist-BioC.txt")
  expect_equal(basename(paths[["CRAN"]]), "qualified-CRAN.txt")

  expect_equal(readLines(paths[["BioC"]]),
               c("GenomeInfoDb", "edgeR", "someBiocPkg"))
  expect_equal(readLines(paths[["CRAN"]]),
               c("dplyr", "ggplot2"))
  # And the allow-list file for BioC must NOT have been written.
  expect_false(file.exists(file.path(out_dir, "qualified-BioC.txt")))
})


test_that("write_qualified_pkg_lists() supports multiple blocklist sources", {
  qm <- data.frame(
    pkg            = c("dplyr",  "limma", "edgeR", "admiral", "shady"),
    repo_name      = c("CRAN",   "BioC",  "BioC",  "github",  "github"),
    final_decision = c("Low",    "Low",   "High",  "Low",     "High"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  paths <- write_qualified_pkg_lists(qm, out_dir,
                                     qualified_decision = "Low",
                                     blocklist_sources = c("BioC", "github"))

  expect_setequal(names(paths), c("BioC", "CRAN", "github"))
  expect_equal(basename(paths[["BioC"]]),   "blocklist-BioC.txt")
  expect_equal(basename(paths[["github"]]), "blocklist-github.txt")
  expect_equal(basename(paths[["CRAN"]]),   "qualified-CRAN.txt")

  expect_equal(readLines(paths[["BioC"]]),   "edgeR")
  expect_equal(readLines(paths[["github"]]), "shady")
  expect_equal(readLines(paths[["CRAN"]]),   "dplyr")
})


test_that("write_qualified_pkg_lists() writes an EMPTY blocklist file when every pkg from that source is qualified", {
  # An empty blocklist is meaningful: it tells PPM 'mirror this whole
  # source, block nothing'. We must still create the file so downstream
  # provisioning has a stable filename to point at.
  qm <- data.frame(
    pkg            = c("dplyr", "limma", "edgeR"),
    repo_name      = c("CRAN",  "BioC",  "BioC"),
    final_decision = c("Low",   "Low",   "Low"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  paths <- write_qualified_pkg_lists(qm, out_dir,
                                     qualified_decision = "Low",
                                     blocklist_sources = "BioC")

  expect_true(file.exists(file.path(out_dir, "blocklist-BioC.txt")))
  # File exists, but contains zero package names.
  expect_length(readLines(file.path(out_dir, "blocklist-BioC.txt")), 0L)
})


test_that("write_qualified_pkg_lists() ignores blocklist_sources = 'NA' (unknown bucket is allow-list only)", {
  # A source we couldn't identify can't be safely inverted into 'block
  # everything except', so the NA bucket always emits a qualified-NA.txt
  # even if the caller lists 'NA' in blocklist_sources.
  qm <- data.frame(
    pkg            = c("dplyr", "ghost1", "ghost2"),
    repo_name      = c("CRAN",  "unknown", NA_character_),
    final_decision = c("Low",   "Low",     "High"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  suppressMessages(
    paths <- write_qualified_pkg_lists(qm, out_dir,
                                       qualified_decision = "Low",
                                       blocklist_sources = c("BioC", "NA"))
  )
  expect_true(file.exists(file.path(out_dir, "qualified-NA.txt")))
  expect_false(file.exists(file.path(out_dir, "blocklist-NA.txt")))
  expect_equal(readLines(file.path(out_dir, "qualified-NA.txt")), "ghost1")
})


test_that("write_qualified_pkg_lists() blocklist_sources default comes from config.yml (BioC)", {
  qm <- data.frame(
    pkg            = c("dplyr", "limma",  "edgeR"),
    repo_name      = c("CRAN",  "BioC",   "BioC"),
    final_decision = c("Low",   "Low",    "High"),
    stringsAsFactors = FALSE
  )

  out_dir <- withr::local_tempdir()
  # No blocklist_sources argument -> pull_config() default should kick
  # in and route BioC into blocklist-BioC.txt.
  paths <- write_qualified_pkg_lists(qm, out_dir, qualified_decision = "Low")

  expect_true(file.exists(file.path(out_dir, "blocklist-BioC.txt")))
  expect_false(file.exists(file.path(out_dir, "qualified-BioC.txt")))
  expect_equal(readLines(file.path(out_dir, "blocklist-BioC.txt")), "edgeR")
})
