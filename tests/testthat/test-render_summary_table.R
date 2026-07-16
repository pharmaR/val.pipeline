test_that("render_summary_table() returns kable for non-HTML output", {
  # By default (outside a knit context) knitr::is_html_output() is FALSE,
  # so we get the kable fallback.
  df <- data.frame(x = 1:3, y = letters[1:3], stringsAsFactors = FALSE)
  out <- render_summary_table(df)
  expect_s3_class(out, "knitr_kable")
})

test_that("render_summary_table() returns a reactable widget when in HTML mode", {
  skip_if_not_installed("reactable")

  local_mocked_bindings(
    is_html_output = function(...) TRUE,
    .package = "knitr"
  )

  df <- data.frame(x = 1:3, y = letters[1:3], stringsAsFactors = FALSE)
  out <- render_summary_table(df)
  expect_s3_class(out, "htmlwidget")
  expect_s3_class(out, "reactable")
})

test_that("render_summary_table() falls back to kable on an empty data.frame", {
  local_mocked_bindings(
    is_html_output = function(...) TRUE,
    .package = "knitr"
  )
  df <- data.frame(x = integer(0), y = character(0), stringsAsFactors = FALSE)
  out <- render_summary_table(df)
  expect_s3_class(out, "knitr_kable")
})
