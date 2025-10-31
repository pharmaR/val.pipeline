
test_that("val_categorize() validates decisions_df structure", {
  invalid_df <- data.frame(wrong = "columns")
  
  expect_error(
    val_categorize(source = "riskscore", decisions_df = invalid_df),
    "decisions_df.*not compliant"
  )
})

test_that("val_categorize() rejects invalid source length", {
  expect_error(
    val_categorize(source = c("riskscore", "other")),
    "Invalid source specified"
  )
})

test_that("val_categorize() rejects unimplemented source types", {
  expect_error(
    val_categorize(source = list(test = "data"))
    # ,"Not yet implemented.*list"
  )
  
  expect_error(
    val_categorize(source = data.frame(test = "data")),
    "Not yet implemented.*data.frame"
  )
  
  expect_error(
    val_categorize(source = "PACKAGES"),
    "Not yet implemented.*PACKAGES"
  )
})

test_that("val_categorize() rejects invalid source", {
  expect_error(
    val_categorize(source = "invalid"),
    "Invalid source specified"
  )
})
