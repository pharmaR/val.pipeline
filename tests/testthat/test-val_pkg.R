
test_that("val_pkg() validates 'ref' parameter", {
  mock_avail <- data.frame(
    Package = "testpkg",
    Version = "1.0.0",
    Repository = "https://cran.r-project.org/src/contrib",
    stringsAsFactors = FALSE
  )
  
  expect_error(
    val_pkg(
      pkg = "testpkg",
      ver = "1.0.0", 
      avail_pkgs = mock_avail,
      ref = "invalid",
      out_dir = tempdir()
    ),
    "should be one of"
  )
})

test_that("val_pkg() validates 'metric_pkg' parameter", {
  mock_avail <- data.frame(
    Package = "testpkg",
    Version = "1.0.0",
    Repository = "https://cran.r-project.org/src/contrib",
    stringsAsFactors = FALSE
  )
  
  expect_error(
    val_pkg(
      pkg = "testpkg",
      ver = "1.0.0",
      avail_pkgs = mock_avail,
      ref = "remote",
      metric_pkg = "invalid",
      out_dir = tempdir()
    ),
    "should be one of"
  )
})

test_that("val_pkg() validates 'val_date' parameter", {
  mock_avail <- data.frame(
    Package = "testpkg",
    Version = "1.0.0", 
    Repository = "https://cran.r-project.org/src/contrib",
    stringsAsFactors = FALSE
  )
  
  expect_error(
    val_pkg(
      pkg = "testpkg",
      ver = "1.0.0",
      avail_pkgs = mock_avail,
      ref = "remote",
      metric_pkg = "riskmetric",
      out_dir = tempdir(),
      val_date = "invalid-date"
    )
  )
})


# test_that("val_pkg() creates required directories", {
#   mock_avail <- data.frame(
#     Package = "testpkg",
#     Version = "1.0.0",
#     Repository = "https://cran.r-project.org/src/contrib",
#     stringsAsFactors = FALSE
#   )
#   
#   temp_dir <- tempdir()
#   test_dir <- file.path(temp_dir, "test_val_pkg")
#   
#   # Clean up any existing test directory
#   if (dir.exists(test_dir)) {
#     unlink(test_dir, recursive = TRUE)
#   }
#   
#   # Mock the riskmetric functions to avoid actual package assessment
#   skip_if_not_installed("riskmetric")
#   
#   # This test would require extensive mocking of riskmetric functions
#   # For now, just test that the function accepts valid parameters
#   expect_no_error({
#     tryCatch({
#       val_pkg(
#         pkg = "testpkg",
#         ver = "1.0.0",
#         avail_pkgs = mock_avail,
#         ref = "remote",
#         metric_pkg = "riskmetric", 
#         out_dir = test_dir,
#         val_date = Sys.Date()
#       )
#     }, error = function(e) {
#       # Expected to fail due to missing riskmetric setup, but parameters are valid
#       if (!grepl("pkg_ref|assessment", e$message)) {
#         stop(e)
#       }
#     })
#   })
# })



# test_that("val_pkg() handles source ref parameter", {
#   mock_avail <- data.frame(
#     Package = "testpkg",
#     Version = "1.0.0",
#     Repository = "https://cran.r-project.org/src/contrib", 
#     stringsAsFactors = FALSE
#   )
#   
#   temp_dir <- tempdir()
#   test_dir <- file.path(temp_dir, "test_val_pkg_source")
#   
#   skip_if_not_installed("riskmetric")
#   
#   expect_no_error({
#     tryCatch({
#       val_pkg(
#         pkg = "testpkg",
#         ver = "1.0.0",
#         avail_pkgs = mock_avail,
#         ref = "source",
#         metric_pkg = "riskmetric",
#         out_dir = test_dir,
#         val_date = Sys.Date()
#       )
#     }, error = function(e) {
#       # Expected to fail due to missing package/download, but parameters are valid
#       if (!grepl("download|pkg_ref|assessment", e$message)) {
#         stop(e)
#       }
#     })
#   })
# })

