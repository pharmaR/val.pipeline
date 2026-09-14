# Contract for apply_val_build_options(): sets `repos` (always) and
# `pkgType = "source"` (only when ref == "source"), and returns the
# previous slot values in an `options()`-shaped list so the caller
# can restore via `on.exit(options(old), add = TRUE)`. Root motivation
# is #181 -- the outer `val_pipeline()` seam used to set
# `pkgType = "source"` unconditionally, diverging parent-session
# state between `val_pipeline() -> val_build()` and bare `val_build()`
# and silently regressing covr_coverage on some source-tier packages.

test_that("apply_val_build_options(ref = 'source') sets repos AND pkgType", {
  withr::with_options(
    list(repos = c(CRAN = "https://old.example/cran"), pkgType = "both"),
    {
      old <- apply_val_build_options(
        ref = "source",
        opt_repos = c(CRAN = "https://new.example/cran",
                      BioC = "https://new.example/bioc")
      )
      # New state is live.
      expect_equal(getOption("repos")[["CRAN"]],
                   "https://new.example/cran")
      expect_equal(getOption("repos")[["BioC"]],
                   "https://new.example/bioc")
      expect_equal(getOption("pkgType"), "source")
      # Returned `old` captured both slots verbatim, ready for
      # `options(old)`.
      expect_setequal(names(old), c("repos", "pkgType"))
      expect_equal(old$repos[["CRAN"]], "https://old.example/cran")
      expect_equal(old$pkgType, "both")
      # Restoring returns state to the with_options snapshot.
      options(old)
      expect_equal(getOption("pkgType"), "both")
      expect_equal(getOption("repos")[["CRAN"]],
                   "https://old.example/cran")
    }
  )
})

test_that("apply_val_build_options(ref = 'remote') does NOT touch pkgType", {
  withr::with_options(
    list(repos = c(CRAN = "https://old.example/cran"), pkgType = "both"),
    {
      old <- apply_val_build_options(
        ref = "remote",
        opt_repos = c(CRAN = "https://new.example/cran")
      )
      expect_equal(getOption("repos")[["CRAN"]],
                   "https://new.example/cran")
      # pkgType left alone.
      expect_equal(getOption("pkgType"), "both")
      # Returned `old` should have `repos` only (options() returns just
      # the slot(s) the setter mutated).
      expect_equal(names(old), "repos")
      expect_equal(old$repos[["CRAN"]], "https://old.example/cran")
      options(old)
      expect_equal(getOption("repos")[["CRAN"]],
                   "https://old.example/cran")
      expect_equal(getOption("pkgType"), "both")
    }
  )
})

test_that("apply_val_build_options rejects invalid ref", {
  expect_error(
    apply_val_build_options(ref = "binary",
                            opt_repos = c(CRAN = "https://x/cran")),
    "should be one of"
  )
})

test_that(
  "apply_val_build_options(ref = 'remote') round-trips through options()", {
  # The end-to-end contract we rely on in val_build(): after the
  # function's `on.exit(options(old), add = TRUE)` fires, the caller
  # session is byte-identical to before the call, even for `ref =
  # 'remote'` (i.e. no phantom pkgType slot appears).
  withr::with_options(
    list(repos = c(CRAN = "https://old.example/cran")),
    {
      before <- options()
      old <- apply_val_build_options(
        ref = "remote",
        opt_repos = c(CRAN = "https://new.example/cran")
      )
      options(old)
      after <- options()
      # `repos` restored, no new pkgType slot introduced.
      expect_equal(after$repos, before$repos)
      expect_identical(is.null(before$pkgType), is.null(after$pkgType))
    }
  )
})


# ---------------------------------------------------------------------------
# Source-scan regression tests (#181)
#
# The pkgType-source leak was a call-path bug, not a helper-signature bug:
# the unit tests above verify apply_val_build_options()'s contract in
# isolation, but they can't catch a re-introduction of an unconditional
# `options(pkgType = "source")` at the val_pipeline() -> val_build() seam
# (which is exactly how the bug shipped originally). These grep-level
# assertions guard the two structural invariants that carry the fix:
#
#   1. val_pipeline.R must NOT set pkgType in the post-prep options()
#      block that hands off to val_build(). Any `pkgType = "source"` at
#      that seam re-introduces the parent-session divergence.
#   2. val_build.R must set options AFTER
#      configure_riskmetric_offline_if_requested() -- the riskmetric-
#      offline helper calls available.packages() to build its offline
#      cache, and running that under a pre-poisoned pkgType regressed
#      covr_coverage on some source-tier packages.
#
# These read source via `system.file(..., package = "val.pipeline")` (test
# targets the *installed* copy of the package, matching devtools::test()
# via load_all()).
# ---------------------------------------------------------------------------

read_pkg_source <- function(rel_path) {
  candidates <- c(
    system.file(file.path("..", rel_path), package = "val.pipeline"),
    file.path(testthat::test_path(), "..", "..", rel_path)
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit) == 0L) {
    skip(paste0("cannot locate ", rel_path, " for source-scan test"))
  }
  readLines(hit[[1L]], warn = FALSE)
}

test_that("val_pipeline.R does NOT set pkgType at the post-prep seam (#181)", {
  src <- read_pkg_source("R/val_pipeline.R")
  # Isolate the block from the last `options(repos = prep$opt_repos, ...)`
  # to end of file. That's the post-prep seam that ships state to
  # val_build(); any pkgType set here is the regression.
  seam_start <- grep("options\\(repos = prep\\$opt_repos", src)
  expect_length(seam_start, 1L)
  seam <- src[seq(seam_start, length(src))]
  # Naked-source regression: any line in the seam that mutates pkgType
  # via `options(pkgType = ...)` is the bug.
  bad <- grep("options\\([^)]*pkgType\\s*=", seam, value = TRUE)
  expect_length(bad, 0L)
})

test_that("val_build.R sets options AFTER configure_riskmetric_offline (#181)", {
  src <- read_pkg_source("R/val_build.R")
  # Structural check: `apply_val_build_options()` (or a direct
  # `options(pkgType = ...)`) must appear AFTER
  # `configure_riskmetric_offline_if_requested(` in val_build's body.
  # The riskmetric-offline helper calls available.packages() to build
  # its cache and running that under `pkgType = "source"` diverges the
  # cache from the bare-val_build path.
  offline_line <- grep(
    "configure_riskmetric_offline_if_requested\\(",
    src
  )
  apply_line <- grep(
    "apply_val_build_options\\(",
    src
  )
  # Expect at least one hit for each.
  expect_gte(length(offline_line), 1L)
  expect_gte(length(apply_line), 1L)
  # The FIRST apply_val_build_options() call (val_build's own top-of-body
  # option set) must land AFTER the FIRST configure_riskmetric_offline
  # call.
  expect_true(apply_line[[1L]] > offline_line[[1L]])
})
