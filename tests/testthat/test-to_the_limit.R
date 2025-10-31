test_that("to_the_limit extracts upper limits correctly", {
  expect_equal(to_the_limit("~ .x < 120000", low = FALSE), 120000)
  expect_equal(to_the_limit("~ .x <= 120000", low = FALSE), 120000)
  expect_equal(to_the_limit("~ .x > 240000", low = FALSE), Inf)
  expect_equal(to_the_limit("~ .x >= 240000", low = FALSE), Inf)
})

test_that("to_the_limit extracts lower limits correctly", {
  expect_equal(to_the_limit("~ .x < 120000", low = TRUE), -Inf)
  expect_equal(to_the_limit("~ .x <= 120000", low = TRUE), -Inf)
  expect_equal(to_the_limit("~ .x > 240000", low = TRUE), 240000)
  expect_equal(to_the_limit("~ .x >= 240000", low = TRUE), 240000)
})

test_that("to_the_limit handles between conditions", {
  expect_equal(to_the_limit("~ dplyr::between(.x, 120000, 240000)", low = TRUE), 120000)
  expect_equal(to_the_limit("~ dplyr::between(.x, 120000, 240000)", low = FALSE), 240000)
  expect_equal(to_the_limit("~ between(.x, 50, 100)", low = TRUE), 50)
  expect_equal(to_the_limit("~ between(.x, 50, 100)", low = FALSE), 100)
})

test_that("to_the_limit handles equality conditions", {
  expect_equal(to_the_limit("~ .x == 100", low = TRUE), 100)
  expect_equal(to_the_limit("~ .x == 100", low = FALSE), 100)
})

test_that("to_the_limit handles is.na conditions", {
  expect_true(is.na(to_the_limit("~ is.na(.x)", low = TRUE)))
  expect_true(is.na(to_the_limit("~ is.na(.x)", low = FALSE)))
})

test_that("to_the_limit handles unparseable conditions", {
  expect_true(is.na(to_the_limit("~ unknown_function(.x)", low = TRUE)))
  expect_true(is.na(to_the_limit("~ unknown_function(.x)", low = FALSE)))
})

test_that("to_the_limit works with multiple conditions", {
  conditions <- c("~ .x < 100", "~ .x > 200", "~ dplyr::between(.x, 50, 150)")
  result_low <- to_the_limit(conditions, low = TRUE)
  result_high <- to_the_limit(conditions, low = FALSE)
  
  expect_equal(result_low, c(-Inf, 200, 50))
  expect_equal(result_high, c(100, Inf, 150))
})