
test_that("workable_assessments() validates 'metric_pkg' parameter", {
  mock_source <- list(assessment = list(), scores = list())
  
  expect_error(
    workable_assessments(
      pkg = "test",
      ver = "1.0.0",
      metric_pkg = "invalid",
      source = mock_source
    ),
    "should be one of"
  )
})

test_that("workable_assessments() validates 'source_ref' parameter", {
  mock_source <- list(assessment = list(), scores = list())
  
  expect_error(
    workable_assessments(
      pkg = "test",
      ver = "1.0.0",
      metric_pkg = "riskmetric",
      source = mock_source,
      source_ref = "invalid"
    ),
    "should be one of"
  )
})

test_that("workable_assessments() requires list source", {
  expect_error(
    workable_assessments(
      pkg = "test",
      ver = "1.0.0",
      metric_pkg = "riskmetric",
      source = "invalid"
    ),
    "Invalid source.*Must be a list"
  )
})

test_that("workable_assessments() requires assessment and scores in source", {
  mock_source <- list(wrong = "data")
  
  expect_error(
    workable_assessments(
      pkg = "test",
      ver = "1.0.0",
      metric_pkg = "riskmetric",
      source = mock_source
    ),
    "Invalid source.*assessment.*scores"
  )
})

test_that("workable_assessments() creates basic data frame", {
  mock_source <- list(
    assessment = list(),
    scores = list()
  )
  
  skip_if_not_installed("riskmetric")
  
  result <- workable_assessments(
    pkg = "testpkg",
    ver = "1.0.0",
    val_date = as.Date("2024-01-01"),
    metric_pkg = "riskmetric",
    source = mock_source,
    source_ref = "remote"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(result$package, "testpkg")
  expect_equal(result$version, "1.0.0")
  expect_equal(result$val_date, as.Date("2024-01-01"))
  expect_equal(result$ref, "remote")
})

test_that("workable_assessments() processes assessment metrics", {
  mock_source <- list(
    assessment = list(
      downloads_1yr = 1000,
      reverse_dependencies = c("pkg1", "pkg2"),
      has_vignettes = 1
    ),
    scores = list(
      news_current = 0.8
    )
  )
  
  skip_if_not_installed("riskmetric")
  
  result <- workable_assessments(
    pkg = "testpkg",
    ver = "1.0.0",
    metric_pkg = "riskmetric",
    source = mock_source,
    source_ref = "remote")
  
  expect_equal(result$downloads_1yr, 1000)
  expect_equal(result$reverse_dependencies, 2)
  expect_equal(result$has_vignettes, 1)
  expect_equal(result$news_current, 0.8)
})

test_that("workable_assessments() handles pkg_metric_error objects", {
  error_obj <- structure("error", class = c("pkg_metric_error", "character"))
  
  mock_source <- list(
    assessment = list(
      downloads_1yr = error_obj,
      r_cmd_check = error_obj
    ),
    scores = list()
  )
  
  skip_if_not_installed("riskmetric")
  
  result <- workable_assessments(
    pkg = "testpkg",
    ver = "1.0.0",
    metric_pkg = "riskmetric",
    source = mock_source,
    source_ref = "source"
  )
  
  expect_true(is.na(result$downloads_1yr))
  expect_true(is.na(result$r_cmd_check_errors))
  expect_true(is.na(result$r_cmd_check_warnings))
})

test_that("workable_assessments() rejects unimplemented metric packages", {
  mock_source <- list(assessment = list(), scores = list())
  
  expect_error(
    workable_assessments(
      pkg = "test",
      ver = "1.0.0",
      metric_pkg = "val.meter",
      source = mock_source,
      source_ref = "source"
    ),
    "Not yet implemented.*val.meter"
  )
  
  expect_error(
    workable_assessments(
      pkg = "test",
      ver = "1.0.0", 
      metric_pkg = "risk.assessr",
      source = mock_source
    ),
    "Not yet implemented.*risk.assessr"
  )
})
