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
  src_path <- src_path_for("val_build.R")
  skip_if_not(nzchar(src_path), "val_build.R source not available")
  src <- paste(readLines(src_path, warn = FALSE), collapse = "\n")

  # Parent hoists opt_repos into a tier variable alongside the other
  # option tiers already re-hydrated across the worker boundary.
  expect_match(src, "repos_tier\\s*<-\\s*opt_repos", perl = TRUE)

  # Worker body re-applies options(repos = repos_tier). Allow either
  # the `pkgType`-included or bare form.
  expect_match(src, "options\\(repos\\s*=\\s*repos_tier", perl = TRUE)
})

test_that("parallel workers reinstate the BiocManager shim (#136)", {
  # Source-level guard. `configure_bioc_repositories()` rewrites
  # BiocManager::repositories() via utils::assignInNamespace() -- an
  # in-memory session-scoped mutation that does NOT survive the
  # future::multisession boundary. Parent calls
  # `configure_bioc_repositories_if_requested()` at val_build entry
  # (L207), but the worker boots a fresh R session with a stock
  # BiocManager namespace, so any Bioc-touching call inside the
  # worker (e.g. riskmetric::assess_reverse_dependencies()) hits the
  # public bioconductor.org URL and fails on air-gapped hosts. The
  # fix is to re-invoke `configure_bioc_repositories_if_requested()`
  # inside the future_mapply() FUN body -- the VAL_PIPELINE_INTERNAL_BIOC
  # env var DOES cross the process boundary, so the helper picks it
  # up and reinstalls the shim. Live-network verification is out of
  # scope for a unit test; this presence check locks the fix in
  # against future refactors that drop the re-invocation.
  #
  # Same category / same fix for `configure_riskmetric_offline()`,
  # which shims riskmetric's assess_reverse_dependencies.default,
  # memoise_bioc_available, and pkg_bioc -- the memoise_bioc_available
  # shim in particular is what saves an air-gapped worker from a
  # hard-coded read.dcf() against bioconductor.org. Both shims must
  # be re-invoked inside every worker.
  src_path <- src_path_for("val_build.R")
  skip_if_not(nzchar(src_path), "val_build.R source not available")
  src <- paste(readLines(src_path, warn = FALSE), collapse = "\n")

  # Both re-invocations live inside the worker FUN body (not the
  # parent-side setup at L207/L208). Look for at least two
  # occurrences of each: parent-side + worker-side.
  for (fn in c("configure_bioc_repositories_if_requested",
               "configure_riskmetric_offline_if_requested")) {
    expect_match(src, fn, perl = TRUE)
    n_hits <- length(gregexpr(fn, src, fixed = TRUE)[[1]])
    expect_gte(n_hits, 2L)
  }
})
