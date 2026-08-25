# Tests for pull_covr_env_vars() — the Layer A env-var block that
# `val_pkg()` uses to wrap the final `pkg_assess()` call so
# `assess_covr_coverage` runs under a normalized environment. See
# `inst/config.yml` (default: covr_env_vars:) and issue #146.

test_that("pull_covr_env_vars() returns the shipped conservative defaults", {
  env <- pull_covr_env_vars()

  expect_type(env, "character")
  expect_true(!is.null(names(env)))
  # The three defaults documented in NEWS.md and inst/config.yml. Anchor
  # the set exactly so a maintainer who broadens the block has to update
  # this test on purpose (and NEWS).
  expect_setequal(
    names(env),
    c("NOT_CRAN", "TESTTHAT", "_R_CHECK_FORCE_SUGGESTS_")
  )
  expect_identical(unname(env[c("NOT_CRAN", "TESTTHAT")]), c("true", "true"))
  expect_identical(unname(env["_R_CHECK_FORCE_SUGGESTS_"]), "false")
})

test_that("pull_covr_env_vars() honours a config override", {
  cfg <- tempfile(fileext = ".yml")
  on.exit(unlink(cfg), add = TRUE)

  writeLines(
    c(
      "default:",
      "  decisions_lst: [Low, Medium, High]",
      "  covr_env_vars:",
      "    NOT_CRAN: \"true\"",
      "    RUN_SLOW_TESTS: \"true\"",
      "    MY_PKG_FLAG: yes",         # bare YAML boolean -> coerced to "true"
      "  opt_repos:",
      "    CRAN: https://packagemanager.posit.co/cran/latest"
    ),
    cfg
  )

  env <- pull_covr_env_vars(config_path = cfg)

  expect_setequal(names(env), c("NOT_CRAN", "RUN_SLOW_TESTS", "MY_PKG_FLAG"))
  expect_identical(unname(env["NOT_CRAN"]), "true")
  expect_identical(unname(env["RUN_SLOW_TESTS"]), "true")
  # Bare YAML `yes` deserializes as TRUE via {config}; helper must coerce
  # to "true" so `Sys.setenv()` accepts it without a type error.
  expect_identical(unname(env["MY_PKG_FLAG"]), "true")
})

test_that("pull_covr_env_vars() returns an empty named character when the block is absent", {
  cfg <- tempfile(fileext = ".yml")
  on.exit(unlink(cfg), add = TRUE)

  writeLines(
    c(
      "default:",
      "  decisions_lst: [Low, Medium, High]",
      "  opt_repos:",
      "    CRAN: https://packagemanager.posit.co/cran/latest"
    ),
    cfg
  )

  env <- pull_covr_env_vars(config_path = cfg)

  expect_type(env, "character")
  expect_length(env, 0L)
  # Empty *named* vector, so withr::with_envvar() treats it as a no-op
  # rather than throwing on an unnamed input.
  expect_true(!is.null(names(env)))
})

test_that("pull_covr_env_vars() rejects an unnamed / partially-named block", {
  cfg <- tempfile(fileext = ".yml")
  on.exit(unlink(cfg), add = TRUE)

  writeLines(
    c(
      "default:",
      "  decisions_lst: [Low, Medium, High]",
      "  covr_env_vars:",
      "    - \"true\"",
      "    - \"false\"",
      "  opt_repos:",
      "    CRAN: https://packagemanager.posit.co/cran/latest"
    ),
    cfg
  )

  expect_error(
    pull_covr_env_vars(config_path = cfg),
    regexp = "named map",
    fixed = FALSE
  )
})

test_that("withr::with_envvar(pull_covr_env_vars()) does not leak into the parent session", {
  # This is the invariant `val_pkg()` relies on: the env-var overrides
  # are scoped to the `pkg_assess()` call and cleaned up on exit even
  # if the wrapped code errors. Verify with a deliberate error inside
  # the wrapped code.
  restore <- Sys.getenv("NOT_CRAN", unset = NA_character_)

  # Ensure a clean baseline for the assertion:
  if (is.na(restore)) {
    withr::defer(Sys.unsetenv("NOT_CRAN"))
  } else {
    withr::defer(Sys.setenv(NOT_CRAN = restore))
  }
  Sys.unsetenv("NOT_CRAN")

  expect_error(
    withr::with_envvar(
      new = pull_covr_env_vars(),
      code = {
        expect_identical(Sys.getenv("NOT_CRAN"), "true")
        stop("boom")
      }
    ),
    "boom"
  )

  # Env var reverted despite the wrapped code erroring.
  expect_identical(Sys.getenv("NOT_CRAN", unset = NA_character_), NA_character_)
})
