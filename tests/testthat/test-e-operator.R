test_that("%e% returns y when x has pkg_metric_error class", {
  error_obj <- structure("error", class = c("pkg_metric_error", "character"))
  expect_equal(error_obj %e% "default", "default")
  expect_equal(error_obj %e% 1, 1)
  expect_equal(error_obj %e% NULL, NULL)
})

test_that("%e% returns x when x does not have pkg_metric_error class", {
  expect_equal("hello" %e% "default", "hello")
  expect_equal(123 %e% "default", 123)
  expect_equal(TRUE %e% FALSE, TRUE)
})

test_that("%e% handles objects with multiple classes", {
  multi_class_obj <- structure("value", class = c("pkg_metric_error", "other_class"))
  expect_equal(multi_class_obj %e% "fallback", "fallback")
  
  no_error_obj <- structure("value", class = c("other_class", "character"))
  expect_equal(no_error_obj %e% "fallback" |> as.character(), "value")
})

test_that("%e% handles NULL and NA values", {
  null_obj <- NULL
  expect_equal(null_obj %e% "default", NULL)
  
  na_obj <- NA
  expect_equal(na_obj %e% "default", NA)
})
