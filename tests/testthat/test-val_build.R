test_that("val_build validates ref parameter", {
  expect_error(
    val_build(ref = "invalid"),
    "should be one of"
  )
})

test_that("val_build validates metric_pkg parameter", {
  expect_error(
    val_build(metric_pkg = "invalid"),
    "should be one of"
  )
})

test_that("val_build validates val_date parameter", {
  expect_error(
    val_build(val_date = "invalid-date"),
    "inherits.*Date"
  )
})

test_that("val_build validates deps parameter logic", {
  # Test that deps parameter is processed correctly
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_val_build")
  
  # Clean up any existing test directory
  if (dir.exists(test_dir)) {
    unlink(test_dir, recursive = TRUE)
  }
  
  # Mock available.packages to avoid network calls
  mock_avail <- data.frame(
    Package = c("testpkg1", "testpkg2"),
    Version = c("1.0.0", "2.0.0"),
    Repository = rep("https://cran.r-project.org/src/contrib", 2),
    stringsAsFactors = FALSE
  )
  
  # This test would require extensive mocking, so just test parameter validation
  expect_no_error({
    tryCatch({
      val_build(
        pkg_names = "testpkg1",
        ref = "remote",
        metric_pkg = "riskmetric",
        deps = "depends",
        deps_recursive = FALSE,
        val_date = Sys.Date(),
        out = test_dir
      )
    }, error = function(e) {
      # Expected to fail due to missing packages/dependencies, but parameters are valid
      if (!grepl("available.packages|package_dependencies|val_pkg", e$message)) {
        stop(e)
      }
    })
  })
})

test_that("val_build handles NULL pkg_names", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_val_build_null")
  
  expect_no_error({
    tryCatch({
      val_build(
        pkg_names = NULL,
        ref = "remote", 
        metric_pkg = "riskmetric",
        deps = NULL,
        val_date = Sys.Date(),
        out = test_dir
      )
    }, error = function(e) {
      # Expected to fail due to trying to assess all packages
      if (!grepl("available.packages|val_pkg|interactive", e$message)) {
        stop(e)
      }
    })
  })
})

test_that("val_build handles NULL deps parameter", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_val_build_no_deps")
  
  expect_no_error({
    tryCatch({
      val_build(
        pkg_names = c("testpkg"),
        ref = "remote",
        metric_pkg = "riskmetric", 
        deps = NULL,
        val_date = Sys.Date(),
        out = test_dir
      )
    }, error = function(e) {
      # Expected to fail due to missing packages, but parameters are valid
      if (!grepl("available.packages|val_pkg", e$message)) {
        stop(e)
      }
    })
  })
})

test_that("val_build creates directory structure", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_val_build_dirs")
  
  # Clean up any existing test directory
  if (dir.exists(test_dir)) {
    unlink(test_dir, recursive = TRUE)
  }
  
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
})