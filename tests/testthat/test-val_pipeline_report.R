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

# Small helper that builds a minimal pre_filtered_pkg_metrics data frame
# matching the column shape produced by val_categorize().
make_fake_pre_filtered <- function(n = 10) {
  data.frame(
    package = sprintf("pf_pkg%02d", seq_len(n)),
    version = "1.0.0",
    final_risk = factor(
      rep(c("Low", "Medium", "High"), length.out = n),
      levels = c("Low", "Medium", "High")
    ),
    auto_pass = FALSE,
    repo_name = "CRAN",
    dwnlds = seq(100L, by = 1000L, length.out = n),
    rev_deps = seq_len(n),
    n_deps = seq_len(n),
    n_vig = 0L,
    news_curr = 1,
    src_cntrl = 1,
    n_sites = 1,
    stringsAsFactors = FALSE
  )
}

test_that("val_pipeline_report renders Pre-Filter Summary when RDS present", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)
  saveRDS(make_fake_pre_filtered(),
          file.path(work, "pre_filtered_pkg_metrics.rds"))

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    quiet = TRUE
  )
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("Candidate packages evaluated", html, fixed = TRUE))
  expect_true(grepl("Pre-Filter Summary", html, fixed = TRUE))
  expect_true(grepl("Pre-filter risk distribution", html, fixed = TRUE))
  expect_true(grepl("Pass / drop breakdown", html, fixed = TRUE))
  expect_true(grepl("Packages dropped by pre-filter", html, fixed = TRUE))
  # Dropped rows (Medium/High) appear in the appendix table
  expect_true(grepl("pf_pkg02", html, fixed = TRUE))
})

test_that("summary_template gates dropped-packages table on HTML output", {
  # Source-level guard: the per-package dropped table is intentionally
  # omitted from PDF renders because a full CRAN-scale run can produce a
  # dropped list large enough to make the archival PDF unwieldy. The
  # sub-section heading and count-summary sentence still render in PDF;
  # only the (potentially huge) filterable listing is HTML-only.
  qmd <- system.file(
    "report", "summary", "summary_template.qmd",
    package = "val.pipeline"
  )
  if (!nzchar(qmd) || !file.exists(qmd)) {
    qmd <- file.path("..", "..", "inst", "report", "summary",
                     "summary_template.qmd")
  }
  skip_if_not(file.exists(qmd), "summary_template.qmd not found")
  src <- paste(readLines(qmd, warn = FALSE), collapse = "\n")
  expect_match(
    src,
    "pre-filter-dropped-table.*has_pre_filter\\s*&&\\s*is_html"
  )
  # PDF branch of the header chunk should explain the omission.
  expect_true(grepl("omitted from the PDF", src, fixed = TRUE))
})

test_that("val_pipeline_report tolerates missing pre_filtered_pkg_metrics", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    n_candidates = NA,
    pre_filtered_path = NA,
    format = "html",
    quiet = TRUE
  )
  expect_true(file.exists(out))
})

test_that("val_pipeline_report errors on missing pre_filtered_path", {
  qm_path <- tempfile(fileext = ".rds")
  saveRDS(make_fake_qual_metadata(), qm_path)
  on.exit(unlink(qm_path), add = TRUE)
  expect_error(
    val_pipeline_report(
      qual_metadata_path = qm_path,
      qual_assessments_path = NA,
      pre_filtered_path = tempfile(fileext = ".rds")
    ),
    "pre_filtered_pkg_metrics file not found"
  )
})

test_that("val_pipeline_report shows val_pipeline() runtime row when supplied", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    n_candidates = NA,
    pre_filtered_path = NA,
    pipeline_runtime = as.difftime(67335, units = "secs"),
    format = "html",
    quiet = TRUE
  )
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("val_pipeline() runtime", html, fixed = TRUE))
  expect_true(grepl("18h 42m 15s", html, fixed = TRUE))
})

test_that("val_pipeline_report omits pre-filter sub-headers when RDS absent", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    pre_filtered_path = NA,
    format = "html",
    quiet = TRUE
  )
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  # Parent heading still renders
  expect_true(grepl("Pre-Filter Summary", html, fixed = TRUE))
  # Sub-headers should NOT render as h2 when has_pre_filter is FALSE
  expect_false(grepl(">Pass / drop breakdown<", html, fixed = TRUE))
  expect_false(grepl(">Pre-filter risk distribution<", html, fixed = TRUE))
  expect_false(grepl(">Per-metric risk distribution<", html, fixed = TRUE))
  expect_false(grepl(">Packages dropped by pre-filter<", html, fixed = TRUE))
})

test_that("val_pipeline_report per-metric distribution renders _cat cols", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)

  pf <- make_fake_pre_filtered(9)
  # Add a couple of per-metric _cat columns
  pf$rev_deps_cat <- factor(
    rep(c("Low", "Medium", "High"), length.out = nrow(pf)),
    levels = c("Low", "Medium", "High")
  )
  pf$n_deps_cat <- factor(
    rep(c("Low", "Low", "High"), length.out = nrow(pf)),
    levels = c("Low", "Medium", "High")
  )
  saveRDS(pf, file.path(work, "pre_filtered_pkg_metrics.rds"))

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    quiet = TRUE
  )
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("Per-metric risk distribution", html, fixed = TRUE))
  # Pretty metric labels appear (raw column names are hidden).
  expect_true(grepl("Reverse dependencies", html, fixed = TRUE))
  expect_true(grepl("Dependencies", html, fixed = TRUE))
  expect_false(grepl("rev_deps_cat", html, fixed = TRUE))
  expect_false(grepl("n_deps_cat", html, fixed = TRUE))
})

test_that("val_pipeline_report includes Downloads (1yr) row from primary_risk_category", {
  skip_if_no_quarto()

  work <- tempfile(pattern = "vpr_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  qm_path <- file.path(work, "qual_metadata.rds")
  saveRDS(make_fake_qual_metadata(), qm_path)

  pf <- make_fake_pre_filtered(9)
  pf$primary_risk_category <- factor(
    rep(c("Low", "Medium", "High"), length.out = nrow(pf)),
    levels = c("Low", "Medium", "High")
  )
  pf$rev_deps_cat <- factor(
    rep("Low", nrow(pf)), levels = c("Low", "Medium", "High")
  )
  saveRDS(pf, file.path(work, "pre_filtered_pkg_metrics.rds"))

  out <- val_pipeline_report(
    qual_metadata_path = qm_path,
    qual_assessments_path = NA,
    format = "html",
    quiet = TRUE
  )
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("Downloads (1yr)", html, fixed = TRUE))
  expect_false(grepl("primary_risk_category", html, fixed = TRUE))
})
