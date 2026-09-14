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
