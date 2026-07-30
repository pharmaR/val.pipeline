test_that("val_build() accepts a `workers` argument", {
  expect_true("workers" %in% names(formals(val_build)))
  expect_equal(formals(val_build)$workers, 1L)
})

test_that("val_pipeline() accepts a `workers` argument", {
  expect_true("workers" %in% names(formals(val_pipeline)))
  expect_equal(formals(val_pipeline)$workers, 1L)
})

test_that("val_build() rejects non-positive-integer `workers`", {
  # Reach the workers check by giving val_build() a bogus prep so it
  # never proceeds past parameter validation. The workers check runs
  # after arg matching but before the assessment loop; giving a bad
  # `workers` should still fire even though the rest of the arguments
  # aren't meaningful.
  expect_error(val_build(workers = 0), "workers")
  expect_error(val_build(workers = -1), "workers")
  expect_error(val_build(workers = NA_integer_), "workers")
  expect_error(val_build(workers = c(1, 2)), "workers")
})
