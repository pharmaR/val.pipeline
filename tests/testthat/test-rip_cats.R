
test_that("rip_cats() processes minimal data correctly", {
  # Create minimal test data
  met_dec_df <- data.frame(
    metric = "test_metric",
    derived_col = "test_metric",
    decision = factor("High", levels = c("Low", "Medium", "High")),
    decision_id = 3,
    condition = "~ .x > 100",
    auto_accept = NA_character_,
    stringsAsFactors = FALSE
  )
  
  pkgs_df <- data.frame(
    package = "testpkg",
    test_metric = 150,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats(met_dec_df, pkgs_df),
    "Decisions based off 'test_metric' metric"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("package" %in% names(result))
  expect_true("final_risk_cat" %in% names(result))
  expect_true(result$final_risk_cat == "High")
})


test_that("rip_cats() handles multiple metrics", {
  met_dec_df <- data.frame(
    metric = c("metric1", "metric2"),
    derived_col = c("metric1", "metric2"),
    decision = factor(c("High", "Medium"), levels = c("Low", "Medium", "High")),
    decision_id = c(3, 2),
    condition = c("~ .x > 100", "~ .x < 50"),
    auto_accept = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  
  pkgs_df <- data.frame(
    package = "testpkg",
    metric1 = 150,
    metric2 = 30,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats(met_dec_df, pkgs_df),
    "Decisions based off.*metric"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("metric1_cat", "metric2_cat", "final_risk_cat") %in% names(result)))
  expect_true(result$final_risk_cat == "High")
})





test_that("rip_cats() preserves package data", {
  met_dec_df <- data.frame(
    metric = "test_metric",
    derived_col = "test_metric", 
    decision = factor("Low", levels = c("Low", "Medium", "High")),
    decision_id = 1,
    condition = "~ .x < 50",
    auto_accept = NA_character_,
    stringsAsFactors = FALSE
  )
  
  pkgs_df <- data.frame(
    package = c("pkg1", "pkg2"),
    test_metric = c(25, 75),
    other_col = c("a", "b"),
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats(met_dec_df, pkgs_df),
    "Decisions based off 'test_metric' metric"
  )
  
  expect_equal(nrow(result), 2)
  expect_true("other_col" %in% names(result))
  expect_equal(result$package, c("pkg1", "pkg2"))
  expect_equal(result$final_risk_cat, factor(c("Low", "High"), levels = c("Low", "Medium", "High")))
})


