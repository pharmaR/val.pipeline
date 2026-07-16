test_that("format_runtime_seconds drops zero-valued leading fields", {
  expect_equal(format_runtime_seconds(0), "0s")
  expect_equal(format_runtime_seconds(12), "12s")
  expect_equal(format_runtime_seconds(3 * 60 + 7), "3m 7s")
})

test_that("format_runtime_seconds keeps trailing zero minutes when hours present", {
  expect_equal(format_runtime_seconds(3600), "1h 0m 0s")
  expect_equal(format_runtime_seconds(18 * 3600 + 42 * 60 + 15),
               "18h 42m 15s")
})

test_that("format_runtime_seconds rounds fractional seconds", {
  expect_equal(format_runtime_seconds(0.4), "0s")
  expect_equal(format_runtime_seconds(0.6), "1s")
  expect_equal(format_runtime_seconds(59.5), "1m 0s")
})

test_that("format_runtime_seconds rejects negative / non-finite / non-scalar", {
  expect_error(format_runtime_seconds(-1))
  expect_error(format_runtime_seconds(NA_real_))
  expect_error(format_runtime_seconds(Inf))
  expect_error(format_runtime_seconds(c(1, 2)))
  expect_error(format_runtime_seconds("60"))
})
