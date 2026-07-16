skip_if_no_quarto <- function() {
  testthat::skip_if_not_installed("quarto")
  testthat::skip_if(is.null(quarto::quarto_path()),
                    "Quarto CLI not available")
}

make_fake_qual_metadata <- function() {
  data.frame(
    pkg = c("dplyr", "rlang", "stringr", "ggplot2", "VeryOldPkg"),
    ver = c("1.2.1", "1.2.0", "1.5.2", "3.5.1", "0.0.1"),
    r_ver = "4.5.2",
    sys_info = "linux",
    repos = "https://packagemanager.posit.co/cran/2026-06-21",
    val_date = as.Date("2026-06-21"),
    ref = "source",
    metric_pkg = "riskmetric",
    decision = c("Low", "Low", "Low", "Low", "High"),
    decision_reason = c("Pre-Approved package", "Auto-Accepted",
                        "Risk Assessment", "Auto-Accepted",
                        "Risk Assessment"),
    decision_reason_note = c(NA, "downloads_1yr",
                             "covr_coverage, has_website",
                             "downloads_1yr, reverse_dependencies",
                             "r_cmd_check_errors"),
    final_decision = c("Low", "Low", "Medium", "Low", "High"),
    final_decision_reason = c("Pre-Approved package", "Auto-Accepted",
                              "Dependency", "Auto-Accepted",
                              "Risk Assessment"),
    final_decision_reason_note = c(NA, "downloads_1yr",
                                   "OldSlowPkg",
                                   "downloads_1yr, reverse_dependencies",
                                   "r_cmd_check_errors"),
    depends = I(list(character(0), character(0),
                     c("OldSlowPkg"), character(0), character(0))),
    suggests = I(list(character(0), character(0), character(0),
                      character(0), character(0))),
    rev_deps = I(list(character(0), character(0), character(0),
                      character(0), character(0))),
    assessment_runtime_txt = c("0.1", "0.05", "3.5", "0.2", "0.5"),
    assessment_runtime_mins = as.difftime(
      c(0.1, 0.05, 3.5, 0.2, 0.5), units = "mins"),
    stringsAsFactors = FALSE
  )
}

test_that("val_pipeline_report errors on missing qual_metadata_path", {
  expect_error(
    val_pipeline_report(qual_metadata_path = tempfile(fileext = ".rds")),
    "qual_metadata file not found"
  )
})

test_that("val_pipeline_report validates arg types", {
  expect_error(
    val_pipeline_report(qual_metadata_path = c("a", "b")),
    "length"
  )
  expect_error(
    val_pipeline_report(qual_metadata_path = 42),
    "is.character"
  )
})

test_that("val_pipeline_report errors on missing qual_assessments_path", {
  tmp <- tempfile(fileext = ".rds")
  saveRDS(make_fake_qual_metadata(), tmp)
  on.exit(unlink(tmp), add = TRUE)

  expect_error(
    val_pipeline_report(
      qual_metadata_path = tmp,
      qual_assessments_path = tempfile(fileext = ".rds")
    ),
    "qual_assessments file not found"
  )
})

test_that("val_pipeline_report renders HTML from metadata alone", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    quiet = TRUE
  )

  expect_length(out, 1L)
  expect_true(file.exists(out))
  expect_match(out, "\\.html$")

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Dependency-downgraded pkg's note carries the failing dep name
  expect_true(grepl("OldSlowPkg", html))
  # Auto-Accepted note carries the driver metric
  expect_true(grepl("downloads_1yr", html))
})

test_that("val_pipeline_report writes to out_dir when provided", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)

  custom_out <- file.path(work, "custom_out")
  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    out_dir = custom_out,
    format = "html",
    file_stem = "run_2026_06_21_summary",
    quiet = TRUE
  )

  expect_true(dir.exists(custom_out))
  expect_true(file.exists(file.path(custom_out,
                                    "run_2026_06_21_summary.html")))
  expect_equal(normalizePath(out, winslash = "/"),
               normalizePath(file.path(custom_out,
                                       "run_2026_06_21_summary.html"),
                             winslash = "/"))
})

test_that("val_pipeline_report handles missing _note cols (older runs)", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm <- make_fake_qual_metadata()
  qm$decision_reason_note <- NULL
  qm$final_decision_reason_note <- NULL
  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(qm, qm_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    quiet = TRUE
  )
  expect_true(file.exists(out))
})

test_that("val_pipeline_report handles missing runtime cols (ancient runs)", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm <- make_fake_qual_metadata()
  qm$assessment_runtime_mins <- NULL
  qm$assessment_runtime_txt <- NULL
  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(qm, qm_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    quiet = TRUE
  )
  expect_true(file.exists(out))

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Placeholder rendered rather than a crash
  expect_true(grepl("No .*assessment_runtime_mins.* recorded", html))
})

test_that("val_pipeline_report handles NA final_decision (pre-#53 runs)", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm <- make_fake_qual_metadata()
  qm$final_decision[c(1L, 3L)] <- NA
  qm$final_decision_reason[c(1L, 3L)] <- NA
  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(qm, qm_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    quiet = TRUE
  )
  expect_true(file.exists(out))
})

test_that("val_pipeline_report errors clearly when required cols missing", {
  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm <- make_fake_qual_metadata()
  qm$final_decision <- NULL
  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(qm, qm_path)

  expect_error(
    val_pipeline_report(qm_path, qual_assessments_path = NA, quiet = TRUE),
    "missing required columns.*final_decision"
  )
})

test_that("val_pipeline_report tolerates qual_assessments missing metric cols", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)

  qa <- data.frame(
    package = c("dplyr", "rlang"),
    version = c("1.2.1", "1.2.0"),
    val_date = as.Date("2026-06-21"),
    riskmetric_version = "0.2.7",
    ref = "source",
    downloads_1yr = c(1e7, 8e6),
    reverse_dependencies = c(100L, 500L),
    dependencies = c(3L, 0L),
    stringsAsFactors = FALSE
  )
  qa_path <- file.path(work, "qual_assessments.rds")
  saveRDS(qa, qa_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = qa_path,
    format = "html",
    quiet = TRUE
  )
  expect_true(file.exists(out))
})

test_that("val_pipeline_report shows Candidate packages when supplied", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    n_candidates = 12345L,
    quiet = TRUE
  )

  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("Candidate packages", html))
  expect_true(grepl("12,345", html, fixed = TRUE))
})

test_that("val_pipeline_report auto-detects candidate count from sibling RDS", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)
  saveRDS(
    data.frame(package = paste0("p", seq_len(9876))),
    file.path(work, "pre_filtered_pkg_metrics.rds")
  )

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    quiet = TRUE
  )
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("Candidate packages", html))
  expect_true(grepl("9,876", html, fixed = TRUE))
})

test_that("val_pipeline_report omits Candidate packages row when NA passed", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)
  # Sibling RDS present, but NA overrides auto-detect
  saveRDS(
    data.frame(package = paste0("p", seq_len(100))),
    file.path(work, "pre_filtered_pkg_metrics.rds")
  )

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    n_candidates = NA,
    quiet = TRUE
  )
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl("Candidate packages", html))
})

test_that("val_pipeline_report renames source label and drops NA cells", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  # Include NA rows to prove they don't leak as literal "NA" into the
  # run-metadata table (this mimics dependency-only rows in real runs).
  qm <- make_fake_qual_metadata()
  qm$ref[c(3L, 5L)] <- NA_character_
  qm$metric_pkg[c(3L, 5L)] <- NA_character_
  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(qm, qm_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    n_candidates = NA,
    quiet = TRUE
  )
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("Assessment reference", html))
  expect_false(grepl("Assessment source", html))
  # No stray "NA" cells introduced by our label rows
  expect_false(grepl("source, NA", html, fixed = TRUE))
  expect_false(grepl("riskmetric, NA", html, fixed = TRUE))
})
