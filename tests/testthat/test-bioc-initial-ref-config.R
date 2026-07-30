test_that("default bioc_initial_ref config knob is 'install'", {
  # pull_config() reads from the pre-packaged config.yml unless the user
  # has set VAL_PIPELINE_CONFIG. The default shipped with the package
  # should be "install" (disk-only, no bioconductor.org scraping).
  withr::local_envvar(VAL_PIPELINE_CONFIG = "")
  withr::local_options(val.pipeline.config = NULL)
  expect_equal(pull_config(val = "bioc_initial_ref", rule_type = "default"),
               "install")
})

test_that("bioc_initial_ref accepts the four documented values", {
  cfg <- tempfile(fileext = ".yml")
  on.exit(unlink(cfg), add = TRUE)
  for (v in c("install", "source", "remote", "skip")) {
    writeLines(c(
      "default:",
      "  decisions_lst: [Low, Medium, High]",
      paste0("  bioc_initial_ref: ", v)
    ), cfg)
    withr::local_options(val.pipeline.config = cfg)
    expect_equal(
      pull_config(val = "bioc_initial_ref", rule_type = "default"),
      v
    )
  }
})
