make_fake_pandoc <- function(dir = tempfile("pandoc_dir_")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  exe <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  file.create(file.path(dir, exe))
  Sys.chmod(file.path(dir, exe), mode = "0755")
  normalizePath(dir, winslash = "/", mustWork = TRUE)
}

# Prevent auto-detection paths (RSTUDIO_PANDOC / real system pandoc /
# on-disk Quarto installs) from leaking into any test's resolution.
neutralize_env <- function() {
  withr::local_envvar(
    c(RSTUDIO_PANDOC = "",
      VAL_PIPELINE_PANDOC_DIR = "",
      # Empty PATH so Sys.which("pandoc") + Sys.which("quarto") both
      # return "". Restored by withr on exit.
      PATH = ""),
    .local_envir = parent.frame()
  )
  # Also disable the host-filesystem `/opt/quarto/*/...` fallback so
  # tests never depend on what happens to be installed under /opt.
  withr::local_options(
    list(val.pipeline.quarto_pandoc_probe_glob = ""),
    .local_envir = parent.frame()
  )
}

test_that("resolve_covr_pandoc_dir returns character(0) when pandoc is already on PATH", {
  fake <- make_fake_pandoc()
  withr::local_envvar(c(
    RSTUDIO_PANDOC = "",
    VAL_PIPELINE_PANDOC_DIR = "",
    PATH = fake
  ))
  expect_identical(resolve_covr_pandoc_dir(), character(0))
  expect_identical(pull_covr_path_env(), character(0))
})

test_that("resolve_covr_pandoc_dir honors VAL_PIPELINE_PANDOC_DIR when pandoc is missing", {
  neutralize_env()
  fake <- make_fake_pandoc()
  withr::local_envvar(c(VAL_PIPELINE_PANDOC_DIR = fake))

  expect_identical(resolve_covr_pandoc_dir(), fake)

  path_env <- pull_covr_path_env()
  expect_named(path_env, "PATH")
  expect_true(startsWith(path_env[["PATH"]], fake))
})

test_that("resolve_covr_pandoc_dir ignores VAL_PIPELINE_PANDOC_DIR pointing at a dir with no pandoc", {
  neutralize_env()
  bogus <- withr::local_tempdir()
  withr::local_envvar(c(VAL_PIPELINE_PANDOC_DIR = bogus))
  # No pandoc anywhere else, and the override dir doesn't contain one
  # either -> should fall through to character(0), not "trust" the
  # bogus dir.
  expect_identical(resolve_covr_pandoc_dir(), character(0))
})

test_that("resolve_covr_pandoc_dir honors RSTUDIO_PANDOC when set", {
  neutralize_env()
  fake <- make_fake_pandoc()
  withr::local_envvar(c(RSTUDIO_PANDOC = fake))
  expect_identical(resolve_covr_pandoc_dir(), fake)
})

test_that("resolve_covr_pandoc_dir discovers pandoc bundled next to `quarto` on PATH", {
  neutralize_env()
  # Simulate: <root>/bin/quarto + <root>/bin/tools/x86_64/pandoc
  root <- withr::local_tempdir()
  q_bin <- file.path(root, "bin")
  dir.create(q_bin, recursive = TRUE)
  q_exe <- if (.Platform$OS.type == "windows") "quarto.exe" else "quarto"
  file.create(file.path(q_bin, q_exe))
  Sys.chmod(file.path(q_bin, q_exe), mode = "0755")

  bundled <- file.path(q_bin, "tools", "x86_64")
  dir.create(bundled, recursive = TRUE)
  p_exe <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  file.create(file.path(bundled, p_exe))
  Sys.chmod(file.path(bundled, p_exe), mode = "0755")

  # PATH points at q_bin only so Sys.which("quarto") resolves and
  # Sys.which("pandoc") stays empty (bundled dir is a subdir).
  withr::local_envvar(c(PATH = q_bin))

  got <- resolve_covr_pandoc_dir()
  expect_true(nzchar(got))
  expect_identical(
    normalizePath(got, winslash = "/", mustWork = FALSE),
    normalizePath(bundled, winslash = "/", mustWork = FALSE)
  )
})

test_that("pull_covr_path_env prepends the resolved dir to the current PATH", {
  neutralize_env()
  fake <- make_fake_pandoc()
  base_path <- "/some/existing/path:/another"
  withr::local_envvar(c(
    VAL_PIPELINE_PANDOC_DIR = fake,
    PATH = base_path
  ))

  got <- pull_covr_path_env()
  expect_named(got, "PATH")
  expect_equal(
    got[["PATH"]],
    paste(fake, base_path, sep = .Platform$path.sep)
  )
})

test_that("pull_covr_env_vars() and pull_covr_path_env() compose cleanly", {
  neutralize_env()
  fake <- make_fake_pandoc()
  withr::local_envvar(c(VAL_PIPELINE_PANDOC_DIR = fake))

  merged <- c(pull_covr_env_vars(), pull_covr_path_env())
  expect_true("PATH" %in% names(merged))
  expect_true("NOT_CRAN" %in% names(merged))
  # Duplicate-name check: the covr_env_vars block must not itself set
  # PATH (which would collide with the pandoc augmentation).
  expect_equal(sum(names(merged) == "PATH"), 1L)
})

test_that("resolve_covr_pandoc_dir honors covr_pandoc_dir from config.yml (#167 review)", {
  neutralize_env()
  fake <- make_fake_pandoc()

  cfg_dir <- withr::local_tempdir()
  cfg <- file.path(cfg_dir, "config.yml")
  writeLines(c(
    "default:",
    paste0("  covr_pandoc_dir: ", shQuote(fake))
  ), cfg)

  expect_identical(resolve_covr_pandoc_dir(config_path = cfg), fake)

  path_env <- pull_covr_path_env(config_path = cfg)
  expect_named(path_env, "PATH")
  expect_true(startsWith(path_env[["PATH"]], fake))
})

test_that("resolve_covr_pandoc_dir ignores covr_pandoc_dir pointing at a dir with no pandoc", {
  neutralize_env()
  bogus <- withr::local_tempdir()

  cfg_dir <- withr::local_tempdir()
  cfg <- file.path(cfg_dir, "config.yml")
  writeLines(c(
    "default:",
    paste0("  covr_pandoc_dir: ", shQuote(bogus))
  ), cfg)

  # Bad config value must silently fall through to character(0),
  # never surface a broken PATH prepend.
  expect_identical(resolve_covr_pandoc_dir(config_path = cfg), character(0))
})

test_that("resolve_covr_pandoc_dir rejects override pointing at a *directory* named pandoc (#167 review)", {
  neutralize_env()
  # Emulate an override where the "pandoc" entry is itself a
  # subdirectory rather than a file. Before the review fix,
  # `file.exists()` returned TRUE for a dir and the override was
  # trusted, prepending a directory whose "executable" can't be
  # exec'd.
  bad_dir <- withr::local_tempdir()
  exe_name <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  dir.create(file.path(bad_dir, exe_name))

  withr::local_envvar(c(VAL_PIPELINE_PANDOC_DIR = bad_dir))
  expect_identical(resolve_covr_pandoc_dir(), character(0))
})

test_that("resolve_covr_pandoc_dir picks the highest-versioned Quarto under the probe glob (#167 review)", {
  neutralize_env()
  # Emulate a multi-version Quarto install tree under a tempdir and
  # point the probe option at it. `<root>/<ver>/bin/tools/<arch>/pandoc`
  # mirrors the Posit-Team `/opt/quarto/<ver>/...` layout exactly so
  # the structural version-extraction regex applies.
  root <- withr::local_tempdir()
  vers <- c("1.4.550", "1.8.27", "1.5.10")
  exe_name <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  for (v in vers) {
    d <- file.path(root, v, "bin", "tools", "x86_64")
    dir.create(d, recursive = TRUE)
    file.create(file.path(d, exe_name))
  }

  probe <- file.path(root, "*", "bin", "tools", "*", exe_name)
  withr::local_options(list(val.pipeline.quarto_pandoc_probe_glob = probe))

  got <- resolve_covr_pandoc_dir()
  expected <- file.path(root, "1.8.27", "bin", "tools", "x86_64")
  expect_identical(
    normalizePath(got, winslash = "/", mustWork = FALSE),
    normalizePath(expected, winslash = "/", mustWork = FALSE)
  )
})

test_that("pull_covr_path_env does not emit a trailing separator on empty PATH (#167 review)", {
  neutralize_env()
  fake <- make_fake_pandoc()
  withr::local_envvar(c(
    VAL_PIPELINE_PANDOC_DIR = fake,
    PATH = ""
  ))
  got <- pull_covr_path_env()
  expect_named(got, "PATH")
  # No trailing `:` on POSIX / `;` on Windows -- an empty PATH
  # component gets interpreted as CWD by some `execvp`
  # implementations, silently letting covr's test child pick up
  # binaries from the test working dir.
  expect_identical(got[["PATH"]], fake)
  expect_false(endsWith(got[["PATH"]], .Platform$path.sep))
})
