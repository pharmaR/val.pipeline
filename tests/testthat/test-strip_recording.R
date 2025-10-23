
test_that("strip_recording removes .recording attribute from list elements", {
  # Create a dummy mock assessment with .recording attributes
  metric1 <- "value1"
  attr(metric1, ".recording") <- "some_recording"
  class(metric1) <- c("with_eval_recording", "character")
  
  metric2 <- 42
  attr(metric2, ".recording") <- "another_recording"
  class(metric2) <- c("with_eval_recording", "numeric")
  
  mock_assessment <- list(metric1 = metric1, metric2 = metric2)
  class(mock_assessment) <- "pkg_assessment"
  
  result <- strip_recording(mock_assessment)
  
  expect_null(attr(result$metric1, ".recording"))
  expect_null(attr(result$metric2, ".recording"))
  expect_equal(class(result), "pkg_assessment")
  expect_false("with_eval_recording" %in% class(result$metric1))
  expect_false("with_eval_recording" %in% class(result$metric2))

  # Try with real {riskmetric} output
  result2 <- riskmetric::pkg_ref("zoo", source = "pkg_cran_remote") |>
    riskmetric::pkg_assess() |>
    strip_recording()

  expect_null(attr(result2$has_news, ".recording"))
  expect_null(attr(result$downloads_1yr, ".recording"))
  expect_true("list_of_pkg_metric" %in% class(result2))
  expect_true("list" %in% class(result2))
  expect_false("with_eval_recording" %in% class(result2$has_news))
  expect_false("with_eval_recording" %in% class(result2$downloads_1yr))
})




test_that("strip_recording preserves other attributes and classes", {
  metric1 <- "value1"
  attr(metric1, ".recording") <- "recording"
  attr(metric1, "other_attr") <- "keep"
  class(metric1) <- c("with_eval_recording", "character")
  
  mock_assessment <- list(metric1 = metric1)
  class(mock_assessment) <- "pkg_assessment"
  
  result <- strip_recording(mock_assessment)
  
  expect_equal(attr(result$metric1, "other_attr"), "keep")
  expect_true("character" %in% class(result$metric1))

  # Try with real {riskmetric} output
  result2 <- riskmetric::pkg_ref("askpass", source = "pkg_cran_remote") |>
    riskmetric::pkg_assess() |>
    strip_recording()

  expect_equal(attr(result2$has_news, "label"), "number of discovered NEWS files")
  expect_true("pkg_metric" %in% class(result2$has_news))
  expect_true("integer" %in% class(result2$has_news))
})

test_that("strip_recording handles empty list", {
  empty_list <- list()
  class(empty_list) <- "pkg_assessment"
  
  result <- strip_recording(empty_list)
  
  expect_equal(length(result), 0)
  expect_equal(class(result), "pkg_assessment")
})
