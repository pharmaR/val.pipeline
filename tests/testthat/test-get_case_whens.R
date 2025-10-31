test_that("get_case_whens() generates case_when expressions", {
  # Create minimal test data
  met_dec_df <- data.frame(
    metric = "test_metric",
    derived_col = "test_metric",
    decision = factor("Low", levels = c("Low", "Medium", "High")),
    decision_id = 1,
    condition = "~ .x > 100",
    auto_accept = NA_character_,
    stringsAsFactors = FALSE
  )
  
  result <- get_case_whens(met_dec_df, "test_metric", "High")
  
  expect_equal(result[[1]][length(result[[1]])] |> as.character(), "High")
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true("test_metric_cat" %in% names(result))

  # use "real" data
  met_dec_df <- build_decisions_df(rule_type = "decide") |> dplyr::mutate(derived_col = metric)
  metric_name <- "downloads_1yr"
  result <- get_case_whens(met_dec_df, metric_name, "High")
  expect_equal(result[[1]][length(result[[1]])] |> as.character(), "High")
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(paste(metric_name, "cat", sep = "_") %in% names(result))
})



test_that("get_case_whens() handles ids parameter", {
  met_dec_df <- data.frame(
    metric = "test_metric",
    derived_col = "test_metric", 
    decision = factor("Low", levels = c("Low", "Medium", "High")),
    decision_id = 1,
    condition = "~ .x > 100",
    auto_accept = NA_character_,
    stringsAsFactors = FALSE
  )
  
  result <- get_case_whens(met_dec_df, "test_metric", "High", ids = TRUE)
  
  expect_equal(result[[1]][length(result[[1]])] |> as.character() |> as.integer(), 3)
  expect_type(result, "list")
  expect_true("test_metric_catid" %in% names(result))

  # use "real" data
  met_dec_df <- build_decisions_df(rule_type = "decide") |> dplyr::mutate(derived_col = metric)
  metric_name <- "downloads_1yr"
  result <- get_case_whens(met_dec_df, metric_name, "High", ids = TRUE)
  
  expect_equal(result[[1]][length(result[[1]])] |> as.character() |> as.integer(), 3)
  expect_true(paste(metric_name, "catid", sep = "_") %in% names(result))
  expect_type(result, "list")
})



test_that("get_case_whens() handles auto_accept parameter", {
  met_dec_df <- data.frame(
    metric = "test_metric",
    derived_col = "test_metric",
    decision = factor("Low", levels = c("Low", "Medium", "High")),
    decision_id = 1,
    condition = "~ .x > 100",
    auto_accept = "~ .x < 50",
    stringsAsFactors = FALSE
  )
  
  result <- get_case_whens(met_dec_df, "test_metric", "High", auto_accept = TRUE)

  
  expect_equal(result[[1]][length(result[[1]])] |> as.character(), "FALSE")
  expect_type(result, "list")
  expect_true("test_metric_cataa" %in% names(result))

  # use "real" data
  met_dec_df <- build_decisions_df(rule_type = "decide") |> dplyr::mutate(derived_col = metric)
  metric_name <- "downloads_1yr"
  result <- get_case_whens(met_dec_df, metric_name, "High", auto_accept = TRUE)
  expect_equal(result[[1]][length(result[[1]])] |> as.character(), "FALSE")
  expect_type(result, "list")
  expect_true(paste(metric_name, "cataa", sep = "_") %in% names(result))
})

test_that("get_case_whens() handles no conditions", {
  met_dec_df <- data.frame(
    metric = "test_metric",
    derived_col = "test_metric",
    decision = factor("Low", levels = c("Low", "Medium", "High")),
    decision_id = 1,
    condition = NA_character_,
    auto_accept = NA_character_,
    stringsAsFactors = FALSE
  )
  
  result <- get_case_whens(met_dec_df, "test_metric", "High")
  
  expect_equal(result[[1]][length(result[[1]])] |> as.character(), "High")
  expect_type(result, "list")
  expect_true("test_metric_cat" %in% names(result))

  # use "real" data
  met_dec_df <- build_decisions_df(rule_type = "decide") |> dplyr::mutate(derived_col = metric)
  metric_name <- "downloads_1yr"
  result <- get_case_whens(met_dec_df, metric_name, "High")
  expect_equal(result[[1]][length(result[[1]])] |> as.character(), "High")
  expect_type(result, "list")
  expect_true(paste(metric_name, "cat", sep = "_") %in% names(result))
})


