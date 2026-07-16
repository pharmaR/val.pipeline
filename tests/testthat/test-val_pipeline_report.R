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
