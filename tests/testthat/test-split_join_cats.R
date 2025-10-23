test_that("split_join_cats() requires pkgs_data parameter", {
  expect_error(
    split_join_cats(pkgs_data = NULL),
    "pkgs_data.*cannot be NULL"
  )
})

test_that("split_join_cats() handles data with no applicable metrics", {
  mock_dec_df <- data.frame(
    metric = "other_metric",
    decision = factor("High", levels = c("Low", "Medium", "High")),
    decision_id = 3,
    condition = "~ .x > 100",
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = "testpkg",
    repo_name = "CRAN",
    test_metric = 50,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- split_join_cats(
      pkgs_data = mock_pkgs,
      dec_df = mock_dec_df,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    ),
    "No applicable.*metrics found"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("final_risk_cat" %in% names(result))
  expect_true(is.na(result$final_risk_cat))
})

test_that("split_join_cats() processes CRAN packages with downloads_1yr", {
  
  mock_dec_df <- data.frame(
    metric = "downloads_1yr",
    decision = factor("High", levels = c("Low", "Medium", "High")),
    decision_id = 3,
    condition = "~ .x > 1000",
    auto_accept = NA_character_,
    derived_col = "downloads_1yr",
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = "testpkg",
    repo_name = "CRAN",
    downloads_1yr = 2000,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- split_join_cats(
      pkgs_data = mock_pkgs,
      dec_df = mock_dec_df,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    ),
    "Applying Decisions Categories.*1.*Primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(result$final_risk_cat, factor("High", levels = c("Low", "Medium", "High")))
})

test_that("split_join_cats() processes non-CRAN packages without downloads_1yr", {
  mock_dec_df <- data.frame(
    metric = c("downloads_1yr", "other_metric"),
    decision = factor(c("High", "Medium"), levels = c("Low", "Medium", "High")),
    decision_id = c(3, 2),
    condition = c("~ .x > 1000", "~ .x < 50"),
    auto_accept = NA_character_,
    derived_col = c("downloads_1yr", "other_metric"),
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = "testpkg",
    repo_name = "BioC",
    downloads_1yr = 2000,
    other_metric = 30,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- split_join_cats(
      pkgs_data = mock_pkgs,
      dec_df = mock_dec_df,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    ),
    "Applying Decisions Categories.*2.*Primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("other_metric_cat", "final_risk_cat") %in% names(result)))
  expect_equal(result$final_risk_cat, factor("Medium", levels = c("Low", "Medium", "High")))
})


test_that("split_join_cats() handles mixed CRAN and non-CRAN packages", {
  mock_dec_df <- data.frame(
    metric = c("downloads_1yr", "other_metric"),
    decision = factor(c("High", "Medium"), levels = c("Low", "Medium", "High")),
    decision_id = c(3, 2),
    condition = c("~ .x > 1000", "~ .x < 50"),
    auto_accept = NA_character_,
    derived_col = c("downloads_1yr", "other_metric"),
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = c("cran_pkg", "bioc_pkg"),
    repo_name = c("CRAN", "BioC"),
    downloads_1yr = c(2000, 1500),
    other_metric = c(30, 40),
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- split_join_cats(
      pkgs_data = mock_pkgs,
      dec_df = mock_dec_df,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    ),
    "Applying Decisions Categories.*2.*Primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("cran_pkg", "bioc_pkg") %in% result$package))
})

test_that("split_join_cats() preserves original data structure", {
  mock_dec_df <- data.frame(
    metric = "test_metric",
    decision = factor("Low", levels = c("Low", "Medium", "High")),
    decision_id = 1,
    condition = "~ .x < 50",
    auto_accept = NA_character_,
    derived_col = "test_metric",
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = "testpkg",
    repo_name = "CRAN",
    test_metric = 25,
    extra_col = "preserved",
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- split_join_cats(
      pkgs_data = mock_pkgs,
      dec_df = mock_dec_df,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    ),
    "Applying Decisions Categories"
  )
  
  expect_true("extra_col" %in% names(result))
  expect_equal(result$extra_col, "preserved")
})
