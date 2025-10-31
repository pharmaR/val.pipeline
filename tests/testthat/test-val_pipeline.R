
test_that("val_pipeline validates ref parameter", {
  expect_error(
    val_pipeline(ref = "invalid"),
    "should be one of"
  )
})

test_that("val_pipeline validates metric_pkg parameter", {
  expect_error(
    val_pipeline(metric_pkg = "invalid"),
    "should be one of"
  )
})

test_that("val_pipeline validates val_date parameter", {
  expect_error(
    val_pipeline(val_date = "invalid-date")
  )
})

# test_that("val_pipeline accepts valid parameters", {
#   temp_dir <- tempdir()
#   test_dir <- file.path(temp_dir, "test_val_pipeline")
#   
#   # Clean up any existing test directory
#   if (dir.exists(test_dir)) {
#     unlink(test_dir, recursive = TRUE)
#   }
#   
#   # This test will likely fail due to complex dependencies, but should validate parameters
#   expect_no_error({
#     tryCatch({
#       val_pipeline(
#         ref = "remote",
#         metric_pkg = "riskmetric",
#         deps = "depends",
#         deps_recursive = FALSE,
#         val_date = Sys.Date(),
#         replace = FALSE,
#         out = test_dir
#       )
#     }, error = function(e) {
#       # Expected to fail due to missing riskmetric/riskscore dependencies
#       if (!grepl("pkg_ref|riskscore|riskmetric|val_categorize|val_build", e$message)) {
#         stop(e)
#       }
#     })
#   })
# })

# test_that("val_pipeline handles deps parameter variations", {
#   temp_dir <- tempdir()
#   test_dir <- file.path(temp_dir, "test_val_pipeline_deps")
#   
#   expect_no_error({
#     tryCatch({
#       val_pipeline(
#         ref = "remote",
#         metric_pkg = "riskmetric",
#         deps = c("depends", "suggests"),
#         deps_recursive = TRUE,
#         val_date = Sys.Date(),
#         out = test_dir
#       )
#     }, error = function(e) {
#       # Expected to fail due to complex dependencies
#       if (!grepl("pkg_ref|riskscore|riskmetric|val_categorize|val_build", e$message)) {
#         stop(e)
#       }
#     })
#   })
# })

# test_that("val_pipeline handles source ref parameter", {
#   temp_dir <- tempdir()
#   test_dir <- file.path(temp_dir, "test_val_pipeline_source")
#   
#   expect_no_error({
#     tryCatch({
#       val_pipeline(
#         ref = "source",
#         metric_pkg = "riskmetric",
#         deps = "depends",
#         val_date = Sys.Date(),
#         out = test_dir
#       )
#     }, error = function(e) {
#       # Expected to fail due to complex dependencies
#       if (!grepl("pkg_ref|riskscore|riskmetric|val_categorize|val_build", e$message)) {
#         stop(e)
#       }
#     })
#   })
# })

# test_that("val_pipeline sets up options correctly", {
#   # Test that the function would set up options (without running full pipeline)
#   old_repos <- getOption("repos")
#   
#   expect_no_error({
#     tryCatch({
#       val_pipeline(
#         ref = "remote",
#         metric_pkg = "riskmetric",
#         deps = NULL,
#         val_date = Sys.Date(),
#         out = tempdir()
#       )
#     }, error = function(e) {
#       # Function should set options before failing
#       # Check that repos option might have been modified
#       current_repos <- getOption("repos")
#       
#       # Expected to fail due to dependencies, but options setup should work
#       if (!grepl("pkg_ref|riskscore|riskmetric|val_categorize|val_build|pull_config", e$message)) {
#         stop(e)
#       }
#     })
#   })
#   
#   # Restore original repos
#   options(repos = old_repos)
# })


