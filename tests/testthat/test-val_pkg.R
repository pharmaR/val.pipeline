
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


test_that("val_pkg NA-decision capture surfaces via source-level markers (#124)", {
  # val_decision() can return final_risk = NA (typically remote_only /
  # Bioc pkgs with a thin viable-metric set). val_pkg() must NOT silently
  # coerce that NA to a tier -- it must (a) keep decision = NA, (b) flip
  # decision_reason to "Incomplete Assessment", and (c) stash an
  # `assessment_gaps` list on the meta bundle so val_finalize() and the
  # summary report can surface it. Standing up the full val_pkg() call
  # requires a live package build, so pin the invariant at the source
  # level.
  src_path <- src_path_for("val_pkg.R")
  skip_if_not(nzchar(src_path), "val_pkg.R source not available")
  src <- readLines(src_path)
  src <- paste(src, collapse = "\n")

  expect_true(grepl("Incomplete Assessment", src, fixed = TRUE),
              info = "val_pkg.R must set decision_reason='Incomplete Assessment' when final_risk is NA")
  expect_true(grepl("is.na(decision$final_risk)", src, fixed = TRUE),
              info = "val_pkg.R must branch on is.na(decision$final_risk)")
  expect_true(grepl("assessment_gaps = assessment_gaps", src, fixed = TRUE),
              info = "meta bundle must carry the assessment_gaps list-col")
})

test_that("val_finalize preserves assessment_gaps as a list-col (#124)", {
  # `purrr::list_flatten(bundle)` would explode assessment_gaps across ~5
  # nested cols. It must be pulled out before flatten (mirroring how
  # `timings` is handled) and reattached as a single list-col so the
  # report can read qual_metadata$assessment_gaps[[i]].
  src_path <- src_path_for("val_finalize.R")
  skip_if_not(nzchar(src_path), "val_finalize.R source not available")
  src <- readLines(src_path)
  src <- paste(src, collapse = "\n")

  expect_true(
    grepl("bundle[[\"assessment_gaps\"]] <- NULL", src, fixed = TRUE),
    info = "val_finalize must strip assessment_gaps before list_flatten()"
  )
  expect_true(
    grepl("x$assessment_gaps <- list(gaps)", src, fixed = TRUE),
    info = "val_finalize must reattach assessment_gaps as a list-col"
  )
})

test_that("val_pkg persists val_pipeline_ver on the meta bundle (#130)", {
  # A resumed run may span multiple val.pipeline versions if the
  # operator upgraded between sessions. Persist the running version on
  # every meta bundle so the summary report can surface the distinct
  # set. Standing up val_pkg() takes a real build, so pin at the
  # source level.
  src_path <- src_path_for("val_pkg.R")
  skip_if_not(nzchar(src_path), "val_pkg.R source not available")
  src <- readLines(src_path)
  src <- paste(src, collapse = "\n")

  expect_true(
    grepl("val_pipeline_ver = as.character(utils::packageVersion(\"val.pipeline\"))",
          src, fixed = TRUE),
    info = "meta_list must carry val_pipeline_ver"
  )
})
