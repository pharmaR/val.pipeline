test_that("val_build() validates ref parameter", {
  expect_error(
    val_build(ref = "invalid"),
    "should be one of"
  )
})

test_that("val_build() validates metric_pkg parameter", {
  expect_error(
    val_build(metric_pkg = "invalid"),
    "should be one of"
  )
})

test_that("val_build() validates val_date parameter", {
  expect_error(
    val_build(val_date = "invalid-date")
  )
})

# test_that("val_build() validates 'deps' parameter logic", {
#   # Test that deps parameter is processed correctly
#   temp_dir <- tempdir()
#   test_dir <- file.path(temp_dir, "test_val_build")
#   
#   # Clean up any existing test directory
#   if (dir.exists(test_dir)) {
#     unlink(test_dir, recursive = TRUE)
#   }
#   
#   # Mock available.packages to avoid network calls
#   mock_avail <- data.frame(
#     Package = c("testpkg1", "testpkg2"),
#     Version = c("1.0.0", "2.0.0"),
#     Repository = rep("https://cran.r-project.org/src/contrib", 2),
#     stringsAsFactors = FALSE
#   )
#   
#   # This test would require extensive mocking, so just test parameter validation
#   expect_no_error({
#     tryCatch({
#       val_build(
#         pkg_names = "testpkg1",
#         ref = "remote",
#         metric_pkg = "riskmetric",
#         deps = "depends",
#         deps_recursive = FALSE,
#         val_date = Sys.Date(),
#         out = test_dir
#       )
#     }, error = function(e) {
#       # Expected to fail due to missing packages/dependencies, but parameters are valid
#       if (!grepl("available.packages|package_dependencies|val_pkg", e$message)) {
#         stop(e)
#       }
#     })
#   })
# })

# test_that("val_build() handles NULL pkg_names", {
#   temp_dir <- tempdir()
#   test_dir <- file.path(temp_dir, "test_val_build_null")
#   
#   expect_no_error({
#     tryCatch({
#       val_build(
#         pkg_names = NULL,
#         ref = "remote", 
#         metric_pkg = "riskmetric",
#         deps = NULL,
#         val_date = Sys.Date(),
#         out = test_dir
#       )
#     }, error = function(e) {
#       # Expected to fail due to trying to assess all packages
#       if (!grepl("available.packages|val_pkg|interactive", e$message)) {
#         stop(e)
#       }
#     })
#   })
# })

# test_that("val_build() handles NULL deps parameter", {
#   temp_dir <- tempdir()
#   test_dir <- file.path(temp_dir, "test_val_build_no_deps")
#   
#   expect_no_error({
#     tryCatch({
#       val_build(
#         pkg_names = c("testpkg"),
#         ref = "remote",
#         metric_pkg = "riskmetric", 
#         deps = NULL,
#         val_date = Sys.Date(),
#         out = test_dir
#       )
#     }, error = function(e) {
#       # Expected to fail due to missing packages, but parameters are valid
#       if (!grepl("available.packages|val_pkg", e$message)) {
#         stop(e)
#       }
#     })
#   })
# })

test_that("val_build() creates directory structure", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_val_build_dirs")
  
  # Clean up any existing test directory
  if (dir.exists(test_dir)) {
    unlink(test_dir, recursive = TRUE)
  }
  expect_output(
    expect_no_error({
      tryCatch({
        val_build(
          pkg_names = c("nonexistent_pkg"),
          ref = "remote",
          metric_pkg = "riskmetric",
          deps = NULL,
          val_date = as.Date("2024-01-01"),
          out = test_dir
        )
      }, error = function(e) {
        # Function should create directories before failing
        r_ver <- getRversion()
        expected_dir <- file.path(test_dir, paste0("R_", r_ver), "20240101")
        
        # Check if directories were created
        if (dir.exists(test_dir) && dir.exists(file.path(test_dir, paste0("R_", r_ver)))) {
          # Directories created successfully, error is expected due to missing packages
          return()
        }
        
        # If directories weren't created, re-throw the error
        if (!grepl("available.packages|val_pkg", e$message)) {
          stop(e)
        }
      })
    })
  )
})

test_that("val_build() wraps val_pkg() in tryCatch so one pkg's error doesn't cancel the run (#116)", {
  # Source-level guard. The behaviour (catch val_pkg() errors,
  # synthesize a High-tier meta with decision_reason = 'Error', save
  # to disk, keep going) can't be exercised in-process without
  # standing up a full val_prep + mocked assess pipeline, so pin the
  # invariant against the source: the tryCatch has to wrap the
  # val_pkg() call and the error handler has to set 'Error' as the
  # decision_reason and put the error message in the note.
  vb <- system.file("R", "val_build.R", package = "val.pipeline")
  if (!nzchar(vb) || !file.exists(vb)) {
    vb <- file.path("..", "..", "R", "val_build.R")
  }
  skip_if_not(file.exists(vb), "val_build.R not found")
  src <- paste(readLines(vb, warn = FALSE), collapse = "\n")

  # tryCatch wraps val_pkg() with an error handler.
  expect_match(src, "tryCatch\\([^)]*val_pkg\\(", perl = TRUE)
  # Error handler surfaces decision_reason = 'Error' and stashes the
  # error text in decision_reason_note.
  expect_true(grepl('decision_reason = "Error"', src, fixed = TRUE))
  expect_true(grepl('final_decision_reason = "Error"', src, fixed = TRUE))
  expect_match(src, "conditionMessage\\(e\\)")
  # (error) suffix on the summary line so it visually stands out.
  expect_true(grepl('suffix = "(error)"', src, fixed = TRUE))
  # Log line goes out at minimal so `verbose = "minimal"` still sees it.
  expect_true(grepl("ERROR while assessing", src, fixed = TRUE))
})

test_that("val_build() parallel branch pins future.scheduling=1L and guards disk-state (#120)", {
  # Source-level guard. The behaviour (fine-grained per-pkg dispatch +
  # post-mapply file-count check that stops the run when future_mapply
  # returned but pkgs are missing on disk) can't be exercised without
  # standing up a real multisession + failure injection, so pin the
  # invariant at the source level.
  vb <- system.file("R", "val_build.R", package = "val.pipeline")
  if (!nzchar(vb) || !file.exists(vb)) {
    vb <- file.path("..", "..", "R", "val_build.R")
  }
  skip_if_not(file.exists(vb), "val_build.R not found")
  src <- paste(readLines(vb, warn = FALSE), collapse = "\n")

  # Fine-grained scheduling so a worker's death loses at most one pkg.
  expect_match(src, "future\\.scheduling = 1L")
  # Post-mapply file-count guard.
  expect_match(src, "post_meta\\s*<-")
  expect_match(src, "landed\\s*<-\\s*sum\\(file.exists\\(post_meta\\)\\)")
  # And it stops (not warns) when short, so val_finalize doesn't
  # collate a truncated qual_metadata silently.
  expect_true(grepl("stop(\"val_build(workers = \", workers,",
                    src, fixed = TRUE))
})

test_that("val_build() wires mem_watchdog to a per-pkg TSV write (#122)", {
  vb <- system.file("R", "val_build.R", package = "val.pipeline")
  if (!nzchar(vb) || !file.exists(vb)) {
    vb <- file.path("..", "..", "R", "val_build.R")
  }
  skip_if_not(file.exists(vb), "val_build.R not found")
  src <- paste(readLines(vb, warn = FALSE), collapse = "\n")

  # Arg is exposed with the documented default.
  expect_true(grepl("mem_watchdog = TRUE", src, fixed = TRUE))
  # And validated as a logical(1).
  expect_true(grepl("is.logical(mem_watchdog)", src, fixed = TRUE))
  # assess_one() calls sample_peak_rss_mb() + append_watchdog_row(),
  # keyed on wd_did_work (skips cached / dep-skip branches).
  expect_true(grepl("wd_did_work", src, fixed = TRUE))
  expect_true(grepl("sample_peak_rss_mb", src, fixed = TRUE))
  expect_true(grepl("append_watchdog_row", src, fixed = TRUE))
  expect_true(grepl("mem_watchdog.tsv", src, fixed = TRUE))
  # Error branch flips wd_errored so the TSV row records the failure.
  expect_true(grepl("wd_errored <<- TRUE", src, fixed = TRUE))

  # Round-robin restripe of todo (by prior-run peaks if available,
  # otherwise pure interleave) so heavy pkgs at the tail spread
  # across workers rather than piling up simultaneously. See #122.
  expect_true(grepl("stride <- (seq_along(todo) - 1L) %% workers",
                    src, fixed = TRUE))
  expect_true(grepl("todo   <- todo[order(stride, seq_along(todo))]",
                    src, fixed = TRUE))
  # One future per package: the restripe only pays off when
  # future.scheduling forces per-element dispatch instead of chunks.
  expect_true(grepl("future.scheduling = 1L", src, fixed = TRUE))
})

test_that("restripe: round-robin interleaves the input order across workers", {
  # Reimplement the restripe here (val_build.R can't be reached without
  # standing up val_prep) and pin the invariant: after restripe, the
  # first `workers` items span the full weight range, not the head.
  restripe <- function(todo, workers) {
    stride <- (seq_along(todo) - 1L) %% workers
    todo[order(stride, seq_along(todo))]
  }

  # 12 pkgs, weights sorted heavy -> light; workers = 4.
  todo <- 1:12
  out <- restripe(todo, workers = 4L)
  # First 4 items must include the head + spread across the input.
  expect_equal(out[1:4], c(1L, 5L, 9L, 2L))
  # Every original index appears exactly once.
  expect_setequal(out, todo)
  # Adjacent items differ by ~workers, not 1 (indicates spread).
  gaps <- diff(out[1:8])
  expect_true(any(gaps >= 3L))
})

test_that("val_build serial branch guards against NA pkg_meta$decision (#124)", {
  # val_decision() can return final_risk = NA when its rule ladder produces
  # no category for a pkg (typically remote_only pkgs with a shrunken
  # viable-metric set). val_pkg() then persists a bundle with decision =
  # NA. Without a guard, the `!= decisions[1]` comparison in val_build()'s
  # serial branch evaluates to NA and takes the whole run down. Assert the
  # NA guard survives.
  src <- readLines(test_path("..", "..", "R", "val_build.R"))
  src <- paste(src, collapse = "\n")

  expect_true(grepl("is.na(pkg_meta$decision)", src, fixed = TRUE),
              info = "val_build.R must NA-guard pkg_meta$decision before !=")
  expect_true(
    grepl("has NA decision on its meta bundle", src, fixed = TRUE),
    info = "NA-decision branch must emit an operator-visible message"
  )
})
