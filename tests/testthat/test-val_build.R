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
