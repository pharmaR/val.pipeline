test_that("build_decisions_df() returns data frame with required columns", {
  # expect_output(
    result <- build_decisions_df(rule_type = "decide")
    # ,"Building decision data.frame"
  # )
  
  expect_s3_class(result, "data.frame")
  required_cols <- c("metric", "decision", "decision_id", "condition", "metric_type", "auto_accept", "accept_condition")
  expect_true(all(required_cols %in% names(result)))
})

test_that("build_decisions_df() handles different rule types", {
  # expect_output(
    result_remote <- build_decisions_df(rule_type = "remote_reduce")
    # ,"Building decision data.frame"
  # )
  
  expect_s3_class(result_remote, "data.frame")
})

test_that("build_decisions_df() filters viable metrics", {
  viable_metrics <- c("downloads_1yr")
  
  # expect_output(
    result <- build_decisions_df(rule_type = "remote_reduce", viable_metrics = viable_metrics)
    # ,"Building decision data.frame"
  # )
  
  expect_true(all(result$metric %in% viable_metrics))
})

test_that("build_decisions_df() handles empty viable metrics", {
  expect_error(
    build_decisions_df(rule_type = "remote_reduce", viable_metrics = character(0)),
    "None of the metrics.*are viable"
  )
})
