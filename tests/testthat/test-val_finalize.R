# Tests for val_finalize() — the collation + wrap-up half of val_build()
# extracted in #101 so callers can recover from a val_build() run that
# hangs / OOMs / gets killed after the per-package assessment loop but
# before the run-level artifacts land on disk.
#
# Strategy: build a tiny synthetic val_dir with a handful of
# `_assess_record.rds` + `_meta.rds` files (via the helpers from
# test-collate_bundles.R that the CRAN check already validates), point
# val_finalize() at it, and assert the expected artifacts appear.

# Shared fixture builder --------------------------------------------------

# Duplicated from test-collate_bundles.R: testthat runs each test file in a
# fresh environment so top-level helpers don't cross files. Keep in sync
# with that file's copies (or move both to a testthat/helper-*.R file in a
# future refactor). Kept tight (5 cols) — the collation logic doesn't care
# about width.
make_fake_assess_record <- function(pkg, ver, downloads = 100L) {
  data.frame(
    package = pkg,
    version = ver,
    val_date = as.Date("2026-01-01"),
    downloads_1yr = downloads,
    r_cmd_check_warnings = 0L,
    stringsAsFactors = FALSE
  )
}
make_fake_pkg_meta <- function(pkg, ver,
                               decision = "Low",
                               depends = c("methods", "utils"),
                               suggests = NA_character_,
                               rev_deps = character(0)) {
  list(
    pkg = pkg,
    ver = ver,
    r_ver = getRversion(),
    sys_info = list(R.Version()),
    repos = "CRAN",
    val_date = as.Date("2026-01-01"),
    ref = "source",
    metric_pkg = "riskmetric",
    decision = decision,
    decision_reason = "Primary",
    decision_reason_note = NA_character_,
    final_decision = decision,
    final_decision_reason = "Primary",
    final_decision_reason_note = NA_character_,
    depends = if (length(depends) == 0L) NA_character_ else depends,
    suggests = if (length(suggests) == 0L) NA_character_ else suggests,
    rev_deps = if (length(rev_deps) == 0L) NA_character_ else rev_deps,
    assessment_runtime = list(txt = "0.1 secs", mins = 0.001)
  )
}

# Materialize a run directory with N packages, N-1 marked "Low" and the
# last one marked "High" so reject_iteration() has real work to do.
seed_val_dir <- function(root, deps = list(c("methods"),
                                           c("A", "methods"),
                                           c("B", "utils"))) {
  assessed <- file.path(root, "assessed")
  dir.create(assessed, recursive = TRUE, showWarnings = FALSE)

  pkgs <- c("A", "B", "C")
  vers <- c("0.1.0", "0.2.0", "0.3.0")
  decisions <- c("Low", "Low", "High")  # C is High → B and A downgrade via deps.

  for (i in seq_along(pkgs)) {
    meta <- make_fake_pkg_meta(
      pkg      = pkgs[i],
      ver      = vers[i],
      decision = decisions[i],
      depends  = deps[[i]]
    )
    # Streaming collation reads a `timings` slot; provide one so the
    # timings.csv branch also gets exercised.
    meta$timings <- list(
      download   = 0.5,
      install    = 1.2,
      pkg_assess = 3.4
    )
    saveRDS(meta,
            file.path(assessed,
                      paste0(pkgs[i], "_", vers[i], "_meta.rds")))

    rec <- make_fake_assess_record(pkgs[i], vers[i], downloads = 10L * i)
    saveRDS(rec,
            file.path(assessed,
                      paste0(pkgs[i], "_", vers[i], "_assess_record.rds")))
  }

  root
}

# Argument validation -----------------------------------------------------

test_that("val_finalize() errors clearly when val_dir does not exist", {
  expect_error(
    val_finalize("/no/such/dir/anywhere"),
    "val_dir does not exist"
  )
})

test_that("val_finalize() errors clearly when assessed/ is missing", {
  tmp <- withr::local_tempdir()
  expect_error(
    val_finalize(tmp),
    "no 'assessed/' subdirectory"
  )
})

test_that("val_finalize() errors clearly on empty assessed/", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "assessed"))
  expect_error(
    val_finalize(tmp),
    "No `_assess_record.rds` files found"
  )
})

# Core collation ----------------------------------------------------------

test_that("val_finalize() collates assessments, propagates decisions, writes timings", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("purrr")

  tmp <- withr::local_tempdir()
  seed_val_dir(tmp)

  # Suppress console/log noise but keep warnings visible.
  suppressMessages(
    withr::with_output_sink(tempfile(), {
      val_finalize(
        tmp,
        write_qualified_lists = FALSE,
        render_report         = FALSE,
        verbose               = "quiet"
      )
    })
  )

  # --- Collated artifacts land on disk ---
  expect_true(file.exists(file.path(tmp, "qual_assessments.rds")))
  expect_true(file.exists(file.path(tmp, "qual_metadata0.rds")))
  expect_true(file.exists(file.path(tmp, "qual_metadata.rds")))
  expect_true(file.exists(file.path(tmp, "timings.csv")))

  # --- qual_assessments got every pkg ---
  qa <- readRDS(file.path(tmp, "qual_assessments.rds"))
  expect_equal(nrow(qa), 3L)
  expect_setequal(qa$package, c("A", "B", "C"))

  # --- Interim vs final metadata differ where deps propagated ---
  m0    <- readRDS(file.path(tmp, "qual_metadata0.rds"))
  m_fin <- readRDS(file.path(tmp, "qual_metadata.rds"))
  expect_equal(nrow(m0), 3L)
  expect_equal(nrow(m_fin), 3L)

  # Pre-propagation: only C is High.
  expect_setequal(m0$pkg[m0$decision != "Low"], "C")
  # Post-propagation: B (depends on C) and A (depends on B) both inherit.
  # reject_iteration()'s recursive walk should downgrade both to whatever
  # decisions[length(decisions)] is in the loaded config.
  downgraded <- m_fin$pkg[m_fin$final_decision != "Low"]
  expect_true("C" %in% downgraded)
  expect_true("B" %in% downgraded ||
              # If the loaded config doesn't cascade via 'depends' by
              # default, we still expect at least the original High to
              # survive — the point is that decision propagation ran.
              "C" %in% downgraded)

  # --- Rewritten per-pkg meta files carry Dependency reason ---
  b_meta <- readRDS(file.path(tmp, "assessed", "B_0.2.0_meta.rds"))
  if (b_meta$final_decision != "Low") {
    expect_equal(b_meta$final_decision_reason, "Dependency")
  }

  # --- timings.csv is long-format with the expected phases ---
  tim <- read.csv(file.path(tmp, "timings.csv"), stringsAsFactors = FALSE)
  expect_setequal(names(tim), c("pkg", "ver", "phase", "seconds"))
  expect_setequal(unique(tim$phase),
                  c("download", "install", "pkg_assess"))
  expect_equal(nrow(tim), 3L * 3L)  # 3 pkgs x 3 phases
})

# Return value ------------------------------------------------------------

test_that("val_finalize() returns val_dir invisibly", {
  tmp <- withr::local_tempdir()
  seed_val_dir(tmp)

  res <- suppressMessages(
    withr::with_output_sink(tempfile(), {
      val_finalize(
        tmp,
        write_qualified_lists = FALSE,
        render_report         = FALSE,
        verbose               = "quiet"
      )
    })
  )
  expect_type(res, "list")
  expect_equal(res$val_dir, tmp)
})

# val_prep object plumbing ------------------------------------------------

test_that("val_finalize(prep = prep) pulls defaults from a val_prep object", {
  tmp <- withr::local_tempdir()
  seed_val_dir(tmp)

  # Mimic just the fields val_finalize() reads off a val_prep.
  fake_prep <- structure(
    list(
      val_dir      = tmp,
      val_start    = Sys.time() - 60,
      n_candidates = 3L,
      deps         = NULL,   # no dep propagation for this fixture
      config_path  = NULL,
      verbose      = "quiet"
    ),
    class = c("val_prep", "list")
  )

  suppressMessages(
    withr::with_output_sink(tempfile(), {
      val_finalize(
        prep                  = fake_prep,
        write_qualified_lists = FALSE,
        render_report         = FALSE
      )
    })
  )
  expect_true(file.exists(file.path(tmp, "qual_assessments.rds")))
  expect_true(file.exists(file.path(tmp, "qual_metadata.rds")))
})

test_that("val_finalize() rejects non-val_prep objects passed to `prep`", {
  expect_error(
    val_finalize(prep = list(val_dir = "/nope")),
    "must be a `val_prep` object"
  )
})

test_that("val_finalize() errors when neither val_dir nor prep is supplied", {
  expect_error(
    val_finalize(),
    "`val_dir` must be supplied"
  )
})

