test_that("val_build() accepts a `workers` argument", {
  expect_true("workers" %in% names(formals(val_build)))
  expect_equal(formals(val_build)$workers, 1L)
})

test_that("val_pipeline() accepts a `workers` argument", {
  expect_true("workers" %in% names(formals(val_pipeline)))
  expect_equal(formals(val_pipeline)$workers, 1L)
})

test_that("val_build() rejects non-positive-integer `workers`", {
  # Reach the workers check by giving val_build() a bogus prep so it
  # never proceeds past parameter validation. The workers check runs
  # after arg matching but before the assessment loop; giving a bad
  # `workers` should still fire even though the rest of the arguments
  # aren't meaningful.
  expect_error(val_build(workers = 0), "workers")
  expect_error(val_build(workers = -1), "workers")
  expect_error(val_build(workers = NA_integer_), "workers")
  expect_error(val_build(workers = c(1, 2)), "workers")
})

test_that("parallel workers rehydrate options(repos = opt_repos) (#132)", {
  # Source-level guard. `future::multisession` boots each worker in a
  # fresh R session that does NOT inherit `options()`, so the parent's
  # `options(repos = opt_repos, ...)` set at the top of val_build()
  # must be explicitly re-applied inside the future_mapply() FUN body.
  # If someone refactors and drops the re-apply, every parallel-assessed
  # pkg silently gets `repos = "unknown"` stamped onto its `_meta.rds`
  # bundle (get_repo_origin() -> getOption("repos") returns R's default
  # `c(CRAN = "@CRAN@")` with no matching entry). Real end-to-end
  # verification would require spinning up a multisession cluster and
  # actually assessing a package, which is prohibitively expensive
  # here; a source-level presence check catches the regression class.
  src_path <- system.file("R", "val_build.R", package = "val.pipeline",
                          mustWork = FALSE)
  if (!nzchar(src_path) || !file.exists(src_path)) {
    src_path <- testthat::test_path("..", "..", "R", "val_build.R")
  }
  skip_if_not(file.exists(src_path), "val_build.R source not available")
  src <- paste(readLines(src_path, warn = FALSE), collapse = "\n")

  # Parent hoists opt_repos into a tier variable alongside the other
  # option tiers already re-hydrated across the worker boundary.
  expect_match(src, "repos_tier\\s*<-\\s*opt_repos", perl = TRUE)

  # Worker body re-applies options(repos = repos_tier). Allow either
  # the `pkgType`-included or bare form.
  expect_match(src, "options\\(repos\\s*=\\s*repos_tier", perl = TRUE)
})
