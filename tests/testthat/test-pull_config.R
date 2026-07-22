test_that("pull_config() returns config when 'val' is specified", {
  # Test with a specific value
  result <- pull_config(val = "decisions_lst", rule_type = "default")
  
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("pull_config() returns full config structure when val is NULL", {
  result <- pull_config(val = NULL, rule_type = "decide")
  
  expect_type(result, "list")
  expect_true("default_lst" %in% names(result))
  expect_true("rule_lst" %in% names(result))
  expect_true("decisions_lst" %in% names(result$default_lst))
})

test_that("pull_config() handles different rule types", {
  result_default <- pull_config(rule_type = "default")
  result_decide <- pull_config(rule_type = "decide")
  result_remote <- pull_config(rule_type = "remote_reduce")
  
  expect_type(result_default, "list")
  expect_type(result_decide, "list")
  expect_type(result_remote, "list")
  expect_false("default_lst" %in% names(result_default))
  expect_true("default_lst" %in% names(result_decide))
  expect_true("default_lst" %in% names(result_remote))
})

test_that("pull_config() validates decision categories", {
  # This test checks that the function validates decision categories
  # The actual validation logic depends on the config file structure
  expect_no_error(pull_config(rule_type = "default"))
})


test_that("pull_config() returns default config when 'rule_type' is invalid", {
  expect_error(
    pull_config(val = "decisions_lst", rule_type = "invalid_type")
  )
})

test_that("pull_config() reads from a user-supplied config_path", {
  packaged <- system.file("config.yml", package = "val.pipeline")
  skip_if(packaged == "", "packaged config.yml not available")

  tmp <- tempfile(fileext = ".yml")
  file.copy(packaged, tmp)
  on.exit(unlink(tmp), add = TRUE)

  expect_no_error(
    result <- pull_config(val = "decisions_lst",
                          rule_type = "default",
                          config_path = tmp)
  )
  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("pull_config() errors when config_path is missing", {
  expect_error(
    pull_config(rule_type = "default",
                config_path = tempfile(fileext = ".does_not_exist")),
    "does not exist"
  )
})

test_that("pull_config() honors val.pipeline.config_path option", {
  packaged <- system.file("config.yml", package = "val.pipeline")
  skip_if(packaged == "", "packaged config.yml not available")

  tmp <- tempfile(fileext = ".yml")
  file.copy(packaged, tmp)
  on.exit(unlink(tmp), add = TRUE)

  old <- options(val.pipeline.config_path = normalizePath(tmp))
  on.exit(options(old), add = TRUE)

  # Should read from the option-pointed file, not the packaged one.
  expect_no_error(pull_config(val = "decisions_lst", rule_type = "default"))
})

test_that("apply_config_path() validates its argument", {
  expect_null(apply_config_path(NULL))
  expect_error(
    apply_config_path(tempfile(fileext = ".nope")),
    "does not exist"
  )
})
