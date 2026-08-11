

test_that("rip_cats_by_pkg() requires 'pkgs_df' parameter", {
  expect_error(
    rip_cats_by_pkg(pkgs_df = NULL),
    "pkgs_df.*cannot be NULL"
  )
})


test_that("rip_cats_by_pkg() requires 'else_cat' parameter", {
  mock_pkgs <- data.frame(package = "test", stringsAsFactors = FALSE)
  
  expect_error(
    rip_cats_by_pkg(pkgs_df = mock_pkgs, else_cat = NULL),
    "else_cat.*cannot be NULL"
  )
})


test_that("rip_cats_by_pkg() processes data with no matching metric types", {
  mock_dec_df <- data.frame(
    metric = "other_metric",
    metric_type = "secondary",
    decision = factor("Low", levels = c("Low", "Medium", "High")),
    decision_id = 1,
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
    )
    ,"No.*primary.*metrics found"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("final_risk_cat" %in% names(result))
  expect_true(is.na(result$final_risk_cat))
})


test_that("rip_cats_by_pkg() processes data with matching metric types", {
  metric_name = "test_metric"
  metric_dec = factor("Low", levels = c("Low", "Medium", "High"))
  mock_dec_df <- data.frame(
    metric = metric_name,
    metric_type = "primary",
    decision = metric_dec,
    decision_id = 1,
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
    )
  #   ,"Applying Decisions Categories.*primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(result$final_risk_cat == metric_dec)
  expect_true(paste(metric_name, "cat", sep = "_") %in% names(result))
  
  # Now try a 'secondary' metric
  mock_dec_df <- data.frame(
    metric = metric_name,
    metric_type = "secondary",
    decision = metric_dec,
    decision_id = 1,
    condition = "~ .x > 100",
    auto_accept = NA_character_,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "secondary",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    )
  #   ,"Applying Decisions Categories.*secondary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(result$final_risk_cat == metric_dec)
  expect_true(paste(metric_name, "cat", sep = "_") %in% names(result))
})


test_that("rip_cats_by_pkg() bypass fires for pass_primary members below the Low tier's floor (#112)", {
  # Config-shaped downloads_1yr rule with bounds at 80k (High/Medium
  # boundary) and 200k (Medium/Low boundary). Under #112 the bypass
  # fires when a pass_primary member's downloads sit below the *Low*
  # tier's floor (200k), not just below the High tier's ceiling (80k),
  # because a pkg between 80k and 200k still lands in Medium and fails
  # `accept_cats: Low` on its own -- the whole point of the bypass is
  # to save those pkgs from the primary-metric axe.
  mock_dec_df <- data.frame(
    metric = "downloads_1yr",
    metric_type = "primary",
    decision = factor(c("High", "Medium", "Low"),
                      levels = c("Low", "Medium", "High")),
    decision_id = c(3, 2, 1),
    condition = c("~ is.na(.x) | .x < 80000",
                  "~ dplyr::between(.x, 80000, 200000)",
                  "~ .x > 200000"),
    auto_accept = c(NA_character_, NA_character_, "~ .x > 1000000"),
    stringsAsFactors = FALSE
  )

  # `bypass_pkg` is on the pass_primary list with 100k downloads (mid-
  # tier: Medium under the config). Pre-#112 it would NOT be bypassed
  # (100k >= 80k). Post-#112 it IS bypassed (100k < 200k), so
  # downloads_1yr should be dropped from the primary metric set.
  mock_pkgs <- data.frame(
    package = "bypass_pkg",
    downloads_1yr = 100000L,
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    pull_config = function(val = NULL, rule_type = NULL, config_path = NULL) {
      if (identical(val, "pass_primary")) "bypass_pkg" else NULL
    }
  )

  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      repo_name = "CRAN",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    )
  )
  # Bypass fired: downloads_1yr should no longer appear in the primary
  # categorisation output.
  expect_false("downloads_1yr_cat" %in% names(result))
})

test_that("rip_cats_by_pkg() bypass does NOT fire when pass_primary member exceeds Low tier's floor (#112)", {
  # Same config, but pkg has 250k downloads -- already in Low tier
  # under downloads_1yr alone. Bypass should NOT fire, so
  # downloads_1yr stays in the primary set.
  mock_dec_df <- data.frame(
    metric = "downloads_1yr",
    metric_type = "primary",
    decision = factor(c("High", "Medium", "Low"),
                      levels = c("Low", "Medium", "High")),
    decision_id = c(3, 2, 1),
    condition = c("~ is.na(.x) | .x < 80000",
                  "~ dplyr::between(.x, 80000, 200000)",
                  "~ .x > 200000"),
    auto_accept = c(NA_character_, NA_character_, "~ .x > 1000000"),
    stringsAsFactors = FALSE
  )
  mock_pkgs <- data.frame(
    package = "popular_pkg",
    downloads_1yr = 250000L,
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    pull_config = function(val = NULL, rule_type = NULL, config_path = NULL) {
      if (identical(val, "pass_primary")) "popular_pkg" else NULL
    }
  )
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      repo_name = "CRAN",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    )
  )
  # Bypass did NOT fire: downloads_1yr should still appear.
  expect_true("downloads_1yr_cat" %in% names(result))
})


test_that("rip_cats_by_pkg() filters 'downloads_1yr' for non-CRAN repos", {
  
  metric_name = "other_metric"
  mock_dec_df <- data.frame(
    metric = c("downloads_1yr", metric_name),
    metric_type = c("primary", "primary"),
    decision = factor(c("Low", "Medium"), levels = c("Low", "Medium", "High")),
    decision_id = c(1, 2),
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
    )
  #   ,"Applying Decisions Categories.*1.*primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(result$final_risk_cat == "Medium")
  expect_true(paste(metric_name, "cat", sep = "_") %in% names(result))
  expect_false("downloads_1yr_cat" %in% names(result))
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
  
  # this test is a little wonky because it includes two pkgs in mock_pkgs when
  # there is only supposed to be one row
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
    )
  #   ,"Applying Decisions Categories"
  )
  
  expect_equal(nrow(result), 2)
  expect_true("other_col" %in% names(result))
  expect_equal(result$package, c("pkg1", "pkg2"))
})

test_that("rip_cats_by_pkg() handles multiple metrics correctly", {
  mock_dec_df <- data.frame(
    metric = c("metric1", "metric2"),
    metric_type = c("primary", "primary"),
    decision = factor(c("Low", "High"), levels = c("Low", "Medium", "High")),
    decision_id = c(1, 3),
    condition = c("~ .x < 50", "~ .x > 100"),
    auto_accept = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = "testpkg",
    metric1 = 30,
    metric2 = 150,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    )
  #   ,"Applying Decisions Categories.*primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(result$final_risk_cat == "High")
  expect_true(all(c("metric1_cat", "metric2_cat") %in% names(result)))
})

test_that("rip_cats_by_pkg() assigns 'else_cat' when no conditions met", {
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
    package = "testpkg",
    test_metric = 100,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    )
  #   ,"Applying Decisions Categories.*primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(result$final_risk_cat == "High")
  expect_true(paste("test_metric", "cat", sep = "_") %in% names(result))
})

test_that("rip_cats_by_pkg() handles empty pkgs_df gracefully", {
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
    package = character(0),
    test_metric = numeric(0),
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    )
  #   ,"Applying Decisions Categories.*primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("package", "test_metric") %in% names(result)))
})


test_that("rip_cats_by_pkg() handles NA values in metrics", {
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
    package = "testpkg",
    test_metric = NA,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    )
  #   ,"Applying Decisions Categories.*primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(result$final_risk_cat == "High")
  expect_true(paste("test_metric", "cat", sep = "_") %in% names(result))
})

test_that("rip_cats_by_pkg() handles 'auto_accept' conditions", {
  mock_dec_df <- data.frame(
    metric = "test_metric",
    metric_type = "primary",
    decision = factor(c("Low", "Medium"), levels = c("Low", "Medium", "High")),
    decision_id = c(1, 2),
    condition = c("~ .x < 50", "~ .x >= 50 & .x < 100"),
    auto_accept = c("~ .x < 10", NA_character_),
    stringsAsFactors = FALSE
  )
  
  mock_pkgs <- data.frame(
    package = "testpkg",
    test_metric = 5,
    stringsAsFactors = FALSE
  )
  
  expect_output(
    result <- rip_cats_by_pkg(
      label = "primary",
      dec_df = mock_dec_df,
      pkgs_df = mock_pkgs,
      decisions = c("Low", "Medium", "High"),
      else_cat = "High"
    )
  #   ,"Applying Decisions Categories.*primary"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(result$final_risk_cat == "Low")
  expect_true(result$test_metric_cataa == TRUE)
  expect_true("test_metric_cataa" %in% names(result))
})


