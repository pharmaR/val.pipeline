test_that("default bioc_initial_ref config knob is 'remote'", {
  # pull_config() reads from the pre-packaged config.yml unless the user
  # has set VAL_PIPELINE_CONFIG. The default shipped with the package
  # is "remote" so we run the classic `pkg_bioc_remote` initial pass;
  # air-gapped hosts should either flip this to "install"/"source" or
  # rely on the `bioc_remote_initial_metrics` whitelist below.
  withr::local_envvar(VAL_PIPELINE_CONFIG = "")
  withr::local_options(val.pipeline.config = NULL)
  expect_equal(pull_config(val = "bioc_initial_ref", rule_type = "default"),
               "remote")
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
    withr::local_options(val.pipeline.config_path = cfg)
    expect_equal(
      pull_config(val = "bioc_initial_ref", rule_type = "default"),
      v
    )
  }
})
