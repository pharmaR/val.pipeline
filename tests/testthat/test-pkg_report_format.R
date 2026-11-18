local_report_format_config <- function(value, env = parent.frame()) {
  cfg <- withr::local_tempfile(fileext = ".yml", .local_envir = env)
  lines <- c("default:", "  decisions_lst: [Low, Medium, High]")
  if (!is.null(value)) lines <- c(lines, paste0("  pkg_report_format: ", value))
  writeLines(lines, cfg)
  withr::local_options(val.pipeline.config_path = cfg, .local_envir = env)
}

test_that("shipped config renders package reports as typst", {
  withr::local_options(val.pipeline.config_path = NULL)
  expect_identical(pkg_report_format(), "typst")
})

test_that("pkg_report_format falls back to typst when unset", {
  local_report_format_config(NULL)
  expect_identical(pkg_report_format(), "typst")
})

test_that("pkg_report_format accepts each documented value", {
  for (v in c("html", "gfm", "typst", "all")) {
    local_report_format_config(v)
    expect_identical(pkg_report_format(), v)
  }
})

test_that("pkg_report_format accepts a list of formats", {
  local_report_format_config("[html, typst]")
  expect_identical(pkg_report_format(), c("html", "typst"))

  local_report_format_config("[html, all]")
  expect_identical(pkg_report_format(), "all")
})

test_that("pkg_report_format rejects unknown formats", {
  local_report_format_config("docx")
  expect_error(pkg_report_format(), "Invalid `pkg_report_format`")
})
