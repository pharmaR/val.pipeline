test_that("rip_cats_by_pkg() requires pkgs_df parameter", {
  expect_error(
    rip_cats_by_pkg(pkgs_df = NULL),
    "pkgs_df.*cannot be NULL"
  )
})

test_that("rip_cats_by_pkg() requires else_cat parameter", {
  mock_pkgs <- data.frame(package = "test", stringsAsFactors = FALSE)
  
  expect_error(
    rip_cats_by_pkg(pkgs_df = mock_pkgs, else_cat = NULL),
    "else_cat.*cannot be NULL"
  )
})

test_that("rip_cats_by_pkg() processes data with no matching metrics", {
  mock_dec_df <- data.frame(
    metric = "other_metric",
    metric_type = "secondary",
    decision = factor("High", levels = c("Low", "Medium", "High")),
    decision_id = 3,
    condition = "~ .x > 100",
    auto_accept = NA_character_,
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = "testpkg",
    test_metric = 50,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    ),
    "No.*primary.*metrics found"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("final_risk_cat" %in% names(result))
})

test_that("rip_cats_by_pkg() processes data with matching metrics", {
  mock_dec_df <- data.frame(
    metric = "test_metric",
    metric_type = "primary",
    decision = factor("High", levels = c("Low", "Medium", "High")),
    decision_id = 3,
    condition = "~ .x > 100",
    auto_accept = NA_character_,
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = "testpkg",
    test_metric = 150,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    ),
    "Applying Decisions Categories.*primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("package" %in% names(result))
  expect_true("final_risk_cat" %in% names(result))
})

test_that("rip_cats_by_pkg() filters downloads_1yr for non-CRAN repos", {
  mock_dec_df <- data.frame(
    metric = c("downloads_1yr", "other_metric"),
    metric_type = c("primary", "primary"),
    decision = factor(c("High", "Medium"), levels = c("Low", "Medium", "High")),
    decision_id = c(3, 2),
    condition = c("~ .x > 1000", "~ .x < 50"),
    auto_accept = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = "testpkg",
    downloads_1yr = 2000,
    other_metric = 30,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      repo_name = "BioC",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    ),
    "Applying Decisions Categories.*1.*primary"
  )
  
  expect_s3_class(result, "data.frame")
})

test_that("rip_cats_by_pkg() preserves package data", {
  mock_dec_df <- data.frame(
    metric = "test_metric",
    metric_type = "primary",
    decision = factor("Low", levels = c("Low", "Medium", "High")),
    decision_id = 1,
    condition = "~ .x < 50",
    auto_accept = NA_character_,
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = c("pkg1", "pkg2"),
    test_metric = c(25, 75),
    other_col = c("a", "b"),
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    ),
    "Applying Decisions Categories"
  )
  
  expect_equal(nrow(result), 2)
  expect_true("other_col" %in% names(result))
  expect_equal(result$package, c("pkg1", "pkg2"))
})