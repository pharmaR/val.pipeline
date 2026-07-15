
# Tests for extract_risk_drivers() — the helper that identifies which per-metric
# `<metric>_cat` columns tied the aggregated `final_risk` (i.e. drove a package
# out of the lowest-risk category). Used by val_pkg() to populate
# `decision_reason_note` for the "Risk Assessment" decision reason (issue #37).

test_that("extract_risk_drivers() returns metrics matching final_risk (High)", {
  lvl <- c("Low", "Medium", "High")
  row <- data.frame(
    package = "x",
    covr_coverage_cat = factor("High", levels = lvl),
    has_vignettes_cat = factor("Low",  levels = lvl),
    r_cmd_check_errors_cat = factor("High", levels = lvl),
    final_risk = factor("High", levels = lvl)
  )
  expect_equal(extract_risk_drivers(row), "covr_coverage, r_cmd_check_errors")
})

test_that("extract_risk_drivers() returns NA when final_risk is lowest tier", {
  lvl <- c("Low", "Medium", "High")
  row <- data.frame(
    package = "x",
    covr_coverage_cat = factor("Low", levels = lvl),
    final_risk = factor("Low", levels = lvl)
  )
  expect_true(is.na(extract_risk_drivers(row)))
})

test_that("extract_risk_drivers() picks metrics tying current final_risk", {
  lvl <- c("Low", "Medium", "High")
  row <- data.frame(
    package = "x",
    covr_coverage_cat = factor("Medium", levels = lvl),
    dependencies_cat = factor("Low",    levels = lvl),
    news_current_cat = factor("Medium", levels = lvl),
    final_risk = factor("Medium", levels = lvl)
  )
  expect_equal(extract_risk_drivers(row), "covr_coverage, news_current")
})

test_that("extract_risk_drivers() ignores _cataa (auto-accept) columns", {
  lvl <- c("Low", "Medium", "High")
  row <- data.frame(
    package = "x",
    downloads_1yr_cat = factor("Low", levels = lvl),
    downloads_1yr_cataa = TRUE,
    covr_coverage_cat = factor("High", levels = lvl),
    final_risk = factor("High", levels = lvl)
  )
  expect_equal(extract_risk_drivers(row), "covr_coverage")
})

test_that("extract_risk_drivers() handles missing/degenerate input safely", {
  lvl <- c("Low", "Medium", "High")
  expect_true(is.na(extract_risk_drivers(NULL)))
  expect_true(is.na(extract_risk_drivers(data.frame())))
  # no final_risk column at all
  no_fr <- data.frame(covr_coverage_cat = "High")
  expect_true(is.na(extract_risk_drivers(no_fr)))
  # no _cat columns
  no_cat <- data.frame(package = "x", final_risk = factor("High", levels = lvl))
  expect_true(is.na(extract_risk_drivers(no_cat)))
  # final_risk is NA
  row <- data.frame(
    package = "x",
    covr_coverage_cat = factor("High", levels = lvl),
    final_risk = factor(NA, levels = lvl)
  )
  expect_true(is.na(extract_risk_drivers(row)))
})

test_that("extract_risk_drivers() respects a non-default decisions vector", {
  # Custom decision ladder where "Green" is the safe lowest tier.
  lvl <- c("Green", "Yellow", "Red")
  row <- data.frame(
    package = "x",
    m1_cat = factor("Green", levels = lvl),
    m2_cat = factor("Red",   levels = lvl),
    final_risk = factor("Red", levels = lvl)
  )
  expect_equal(
    extract_risk_drivers(row, decisions = lvl),
    "m2"
  )
  # final_risk == "Green" -> NA regardless of _cat contents
  row$final_risk <- factor("Green", levels = lvl)
  expect_true(is.na(extract_risk_drivers(row, decisions = lvl)))
})
