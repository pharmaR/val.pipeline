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
