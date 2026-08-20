test_that("default bioc_remote_initial_metrics matches the commented-out config default", {
  # `bioc_remote_initial_metrics:` is intentionally commented out in
  # inst/config.yml (see the comment there about pre-0.1.10 behaviour).
  # `pull_config()` therefore returns NULL for the default rule tier,
  # meaning bioc_remote pkgs run every riskmetric assessment. If the
  # whitelist is ever restored to the shipped config, update this test
  # to expect the character vector back.
  withr::local_envvar(VAL_PIPELINE_CONFIG = "")
  withr::local_options(val.pipeline.config_path = NULL)
  out <- pull_config(val = "bioc_remote_initial_metrics", rule_type = "default")
  expect_null(out)
})

test_that("bioc_remote_initial_metrics accepts a user-supplied whitelist", {
  cfg <- tempfile(fileext = ".yml")
  on.exit(unlink(cfg), add = TRUE)
  writeLines(c(
    "default:",
    "  bioc_remote_initial_metrics:",
    "    - assess_reverse_dependencies",
    "  decisions_lst: [Low, Medium, High]"
  ), cfg)
  withr::local_options(val.pipeline.config_path = cfg)
  expect_equal(
    pull_config(val = "bioc_remote_initial_metrics", rule_type = "default"),
    "assess_reverse_dependencies"
  )
})

test_that("bioc_remote_initial_metrics accepts an explicit null (opt-out)", {
  cfg <- tempfile(fileext = ".yml")
  on.exit(unlink(cfg), add = TRUE)
  writeLines(c(
    "default:",
    "  bioc_remote_initial_metrics: ~",
    "  decisions_lst: [Low, Medium, High]"
  ), cfg)
  withr::local_options(val.pipeline.config_path = cfg)
  expect_null(
    pull_config(val = "bioc_remote_initial_metrics", rule_type = "default")
  )
})
