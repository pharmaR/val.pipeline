
test_that("val_prep_pipeline() validates arguments", {
  expect_error(
    val_prep_pipeline(ref = "invalid"),
    "should be one of"
  )
  expect_error(
    val_prep_pipeline(metric_pkg = "invalid"),
    "should be one of"
  )
  expect_error(
    val_prep_pipeline(val_date = "not-a-date")
  )
})


test_that("val_pipeline() rejects a non-`val_prep` prep argument", {
  expect_error(
    val_pipeline(prep = list(pkgs = "dplyr")),
    "val_prep"
  )
})


test_that("val_build() rejects a non-`val_prep` prep argument", {
  expect_error(
    val_build(prep = list(pkgs = "dplyr")),
    "val_prep"
  )
})
