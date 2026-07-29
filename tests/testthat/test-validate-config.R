test_that("validate_config() accepts the bundled config", {
  # Reset any user override so we hit the bundled config.
  old <- options(val.pipeline.config_path = NULL)
  on.exit(options(old), add = TRUE)

  res <- expect_no_warning(validate_config())
  expect_true(res$ok)
  expect_equal(res$config_version, CONFIG_VERSION_CURRENT)
  expect_length(res$warnings, 0)
})

.write_yaml <- function(text, dir = tempfile("cfg")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, "config.yml")
  writeLines(text, path)
  path
}

test_that("validate_config() warns when config_version is missing", {
  path <- .write_yaml(c(
    "default:",
    "  decisions_lst: [Low, Medium, High]"
  ))
  expect_warning(res <- validate_config(path), "config_version")
  expect_false(res$ok)
  expect_true(is.na(res$config_version))
})

test_that("validate_config() warns when config_version is too old", {
  path <- .write_yaml(c(
    "default:",
    "  config_version: \"0.0.1\"",
    "  decisions_lst: [Low, Medium, High]"
  ))
  expect_warning(res <- validate_config(path), "older than the minimum")
  expect_false(res$ok)
})

test_that("validate_config() warns on unknown top-level sections", {
  path <- .write_yaml(c(
    "default:",
    "  config_version: \"1.0.0\"",
    "  decisions_lst: [Low, Medium, High]",
    "typo_section:",
    "  something: 1"
  ))
  expect_warning(res <- validate_config(path), "Unknown top-level section")
  expect_false(res$ok)
})

test_that("validate_config() warns on unknown keys under `default`", {
  path <- .write_yaml(c(
    "default:",
    "  config_version: \"1.0.0\"",
    "  decisions_lst: [Low, Medium, High]",
    "  approvd_pkgs: [foo]"   # typo of approved_pkgs
  ))
  expect_warning(res <- validate_config(path), "approvd_pkgs")
  expect_false(res$ok)
})

test_that("validate_config() rejects non-logical `air_gapped`", {
  path <- .write_yaml(c(
    "default:",
    "  config_version: \"1.0.0\"",
    "  air_gapped: \"yes\"",
    "  decisions_lst: [Low, Medium, High]"
  ))
  expect_warning(res <- validate_config(path), "air_gapped")
  expect_false(res$ok)
})

test_that("validate_config() warns on rule blocks with invalid type / cats", {
  path <- .write_yaml(c(
    "default:",
    "  config_version: \"1.0.0\"",
    "  decisions_lst: [Low, Medium, High]",
    "remote_reduce:",
    "  inherits: default",
    "  downloads_1yr:",
    "    cond:",
    "      Bogus: ~ .x < 1",
    "    type: sometimes",
    "    accept_cats: Low"
  ))
  ws <- character(0)
  withCallingHandlers(
    res <- validate_config(path),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(res$ok)
  expect_true(any(grepl("invalid `type", ws)))
  expect_true(any(grepl("not in `decisions_lst`", ws)))
})

test_that("validate_config(strict = TRUE) escalates to error", {
  path <- .write_yaml(c(
    "default:",
    "  decisions_lst: [Low, Medium, High]"
  ))
  expect_error(validate_config(path, strict = TRUE), "config_version")
})

test_that(".val_pipeline_air_gapped_requested() is TRUE when env var is truthy", {
  withr::with_envvar(list(VAL_PIPELINE_INTERNAL_BIOC = "1"), {
    expect_true(.val_pipeline_air_gapped_requested())
  })
  withr::with_envvar(list(VAL_PIPELINE_INTERNAL_BIOC = "yes"), {
    expect_true(.val_pipeline_air_gapped_requested())
  })
  withr::with_envvar(list(VAL_PIPELINE_INTERNAL_BIOC = ""), {
    # And FALSE when the env var is unset and the bundled config has
    # air_gapped: false.
    old <- options(val.pipeline.config_path = NULL)
    on.exit(options(old), add = TRUE)
    expect_false(.val_pipeline_air_gapped_requested())
  })
})

test_that(".val_pipeline_air_gapped_requested() picks up config knob", {
  path <- .write_yaml(c(
    "default:",
    "  config_version: \"1.0.0\"",
    "  air_gapped: true",
    "  decisions_lst: [Low, Medium, High]"
  ))
  withr::with_envvar(list(VAL_PIPELINE_INTERNAL_BIOC = ""), {
    old <- options(val.pipeline.config_path = path)
    on.exit(options(old), add = TRUE)
    expect_true(.val_pipeline_air_gapped_requested())
  })
})
