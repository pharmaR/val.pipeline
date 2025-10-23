

test_that("%e% returns y when x is empty string", {
  expect_equal("" %e% "default", "default")
  expect_equal("" %e% 1, 1)
  expect_equal("" %e% NULL, NULL)
})

test_that("%e% returns x when x is not empty", {
  expect_equal("hello" %e% "default", "hello")
  expect_equal("test" %e% 1, "test")
  expect_equal("value" %e% NULL, "value")
})

test_that("%e% handles special cases", {
  expect_equal(" " %e% "default", " ")  # space is not empty
  expect_equal("0" %e% "default", "0")  # "0" is not empty
  expect_equal("FALSE" %e% "default", "FALSE")  # "FALSE" is not empty
})

test_that("%e% works with different data types", {
  expect_equal(NA %e% "default", NA)  # NA is not empty string
  expect_equal(5 %e% 10, 5)           # numeric
  expect_equal(TRUE %e% FALSE, TRUE)  # logical
})

test_that("%e% does not work with NULL", {
  expect_error(NULL %e% "default")  
})
