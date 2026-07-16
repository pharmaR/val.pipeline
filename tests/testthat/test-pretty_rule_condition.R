test_that("pretty_rule_condition() renders simple comparisons", {
  expect_equal(pretty_rule_condition("~ .x > 1000000"), "> 1,000,000")
  expect_equal(pretty_rule_condition("~ .x < 25"), "< 25")
  expect_equal(pretty_rule_condition("~ .x >= 100"), "\u2265 100")
  expect_equal(pretty_rule_condition("~ .x <= 5"), "\u2264 5")
  expect_equal(pretty_rule_condition("~ .x == 1"), "= 1")
  expect_equal(pretty_rule_condition("~ .x != 1"), "\u2260 1")
})

test_that("pretty_rule_condition() renders dplyr::between()", {
  expect_equal(pretty_rule_condition("~ dplyr::between(.x, 25, 35)"),
               "25 \u2013 35")
  expect_equal(pretty_rule_condition("~ dplyr::between(.x, 120000, 240000)"),
               "120,000 \u2013 240,000")
})

test_that("pretty_rule_condition() handles NA-or-<cond> composites", {
  expect_equal(pretty_rule_condition("~ is.na(.x) | .x < 120000"),
               "< 120,000 or NA")
  expect_equal(pretty_rule_condition("~ is.na(.x) | .x == 0"),
               "= 0 or NA")
  expect_equal(pretty_rule_condition("~ is.na(.x)"), "NA")
})

test_that("pretty_rule_condition() handles empty / NA inputs", {
  expect_true(is.na(pretty_rule_condition(NA_character_)))
  expect_true(is.na(pretty_rule_condition("")))
})

test_that("pretty_rule_condition() falls through unknown patterns", {
  # Unrecognized expressions come through unchanged (minus the leading ~)
  expect_equal(pretty_rule_condition("~ some_other(.x, 5)"),
               "some_other(.x, 5)")
})

test_that("pretty_rule_condition() rejects non-scalar input", {
  expect_error(
    pretty_rule_condition(c("~ .x > 1", "~ .x < 5")),
    "length-1"
  )
})

test_that("pretty_rule_condition() round-trips every condition in the shipped config", {
  # This is a regression test: no expression from the real 'decide' rule
  # block should fall out unformatted (which would be an ugly surprise in
  # the summary report).
  d <- build_decisions_df(rule_type = "decide")
  rendered <- vapply(d$condition, pretty_rule_condition, character(1))
  expect_false(any(is.na(rendered)))
  # None of the rendered strings should still contain the raw '.x' token
  expect_false(any(grepl("\\.x", rendered)),
               info = paste0("Unformatted conditions: ",
                             paste(rendered[grepl("\\.x", rendered)],
                                   collapse = "; ")))
})
