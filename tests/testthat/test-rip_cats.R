
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


test_that("rip_cats() scales to large pkgs_df (vectorised, not rowwise)", {
  # Regression test for the perf fix that removed `dplyr::rowwise()` from
  # rip_cats(). The case_when() expressions built by get_case_whens() are
  # fully vectorised, so this should complete in a fraction of a second
  # even for tens of thousands of rows. The previous rowwise()-backed
  # implementation took multiple seconds on this input on CI hardware.
  met_dec_df <- data.frame(
    metric      = c("dwnlds", "dwnlds", "dwnlds"),
    derived_col = c("dwnlds", "dwnlds", "dwnlds"),
    decision    = factor(c("High", "Medium", "Low"),
                         levels = c("Low", "Medium", "High")),
    decision_id = c(3, 2, 1),
    condition   = c("~ is.na(.x) | .x < 120000",
                    "~ dplyr::between(.x, 120000, 240000)",
                    "~ .x > 240000"),
    auto_accept = c(NA_character_, NA_character_, "~ .x > 1000000"),
    stringsAsFactors = FALSE
  )

  set.seed(42)
  n <- 20000L
  pkgs_df <- data.frame(
    package = paste0("pkg", seq_len(n)),
    dwnlds  = sample(c(NA_integer_, 50000L, 180000L, 500000L, 2000000L),
                     n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  elapsed <- system.time({
    result <- suppressMessages(
      capture.output(res <- rip_cats(met_dec_df, pkgs_df))
    )
  })["elapsed"]

  # Correctness: every row should land in the expected bucket based on
  # its `dwnlds` value.
  expected <- ifelse(
    is.na(pkgs_df$dwnlds) | pkgs_df$dwnlds < 120000, "High",
    ifelse(pkgs_df$dwnlds > 1000000, "Low",       # auto_accept wins
    ifelse(pkgs_df$dwnlds >= 120000 & pkgs_df$dwnlds <= 240000, "Medium",
    ifelse(pkgs_df$dwnlds > 240000, "Low", "High")))
  )
  expect_equal(as.character(res$final_risk_cat), expected)

  # Guardrail: the vectorised implementation must comfortably finish in
  # well under 10s on 20k rows. The previous rowwise() implementation
  # took ~30-60s on the same input on CI hardware. Use a generous ceiling
  # so slow CI runners don't flake, while still catching a regression
  # back to per-row evaluation.
  expect_lt(elapsed, 10)
})


test_that("rip_cats() source does not call dplyr::rowwise()", {
  # Guardrail against reintroducing the rowwise() antipattern in
  # rip_cats(). The case_when() expressions are fully vectorised, so
  # rowwise() is both unnecessary and a large performance cliff for the
  # thousands-of-packages case that val_categorize() feeds in.
  src <- deparse(body(rip_cats))
  expect_false(any(grepl("rowwise", src)))
})


