# Diagnose the val_build() vs val_pipeline() covr_coverage delta
# ==============================================================
#
# Symptom (see agent thread 2026-09-14):
#   * `val_build(pkg_names = "logrx", ...)` in a workbench job returns
#     `covr_coverage > 90%`.
#   * `val_pipeline(...)` in the same workbench job, with the same
#     hard-pinned CRAN snapshot (`.../cran-r4.5-2026-08-15/latest`),
#     returns `covr_coverage = 62%` for the same logrx 0.4.0 tarball.
#
# The tarball is identical between the two runs, so the delta must
# live in what `val_pipeline()` does BEFORE `val_build()` runs -- most
# likely a `.libPaths()` reshuffle, a differing `pkgType`/`repos`
# option, a different resolved `config_path`, or a different
# `covr_env_vars` block. This script captures a "run-context probe"
# just before each call so an operator can diff the two side-by-side
# without paying for a second full pipeline invocation.
#
# HOW TO USE
# ----------
# 1. Copy this script into the workbench project that reproduces the
#    delta (the one whose config's opt_repos is hard-pinned to
#    2026-08-15). Do NOT edit it against the source tree here -- it
#    needs to see YOUR real config / opt_repos / prep.
# 2. Set MODE below to "val_build" and launch as a workbench job.
#    When it finishes, /tmp/vp_probe_val_build.rds will exist.
# 3. Switch MODE to "val_pipeline" and launch a second workbench
#    job with the same runner settings (same R version, same
#    workers, same libPaths mount). When it finishes,
#    /tmp/vp_probe_val_pipeline.rds will exist.
# 4. Back in an interactive session (or a third job), source the
#    `DIFF ONLY` block at the bottom. It prints a waldo diff of the
#    two probes plus a compact table of packages whose installed
#    version differs between the two runs.
#
# What it captures per run
# ------------------------
#   * `.libPaths()`                            (search order)
#   * `installed.packages()[, Package|Ver|Lib]`(what's on those paths)
#   * `Sys.getenv(<covr-relevant vars>)`       (PATH, HOME, R_LIBS_*,
#                                              RSTUDIO_PANDOC, NOT_CRAN,
#                                              TESTTHAT, locale, ...)
#   * relevant `options()` slots               (repos, pkgType, scipen,
#                                              val.pipeline.config_path,
#                                              val.pipeline.capture_*,
#                                              val.pipeline.covr_*)
#   * `val.pipeline:::pull_covr_env_vars()`    (the block val_pkg
#                                              stitches into with_envvar)
#   * `val.pipeline:::pull_covr_path_env()`    (the pandoc PATH addend)
#   * `val.pipeline:::pull_covr_home_env()`    (the HOME addend, #173)
#   * `val.pipeline:::resolve_covr_pandoc_dir()`
#   * `packageVersion("val.pipeline")` + `find.package("val.pipeline")`
#     (guards against the "which lib slot loaded which version" trap
#     that motivated #179)
#
# The probes fire IMMEDIATELY before the target call, so anything the
# call itself changes (e.g., val_prep_pipeline's `options(repos=...)`,
# any BiocManager install side effect on .libPaths, an rv/renv
# activation triggered by writing pipeline.toml) will be visible in
# the val_pipeline probe but not the val_build probe.

# ---- Config: point at YOUR project ------------------------------------------
# All four of these MUST be filled in from your workbench project's
# environment. The script does not read your live config -- it expects
# the caller to have already sourced whatever sets `opt_repos`, `out`,
# `ref`, `metric_pkg` etc. above this file.

MODE      <- "val_build"     # <-- switch to "val_pipeline" for the second job
PROBE_DIR <- "/tmp"          # where the .rds probe files land
# opt_repos, out, ref, metric_pkg are expected to already exist in the
# calling session -- same as your normal launcher script does.

# ---- Probe helper -----------------------------------------------------------
probe_run_context <- function(label) {
  # Non-val.pipeline env we know from prior triage matters
  env_names <- c("PATH", "HOME", "R_LIBS_SITE", "R_LIBS_USER", "R_LIBS",
                 "RSTUDIO_PANDOC", "RSTUDIO", "RSTUDIO_USER_IDENTITY",
                 "NOT_CRAN", "TESTTHAT", "_R_CHECK_FORCE_SUGGESTS_",
                 "NOT_ON_CRAN", "RUN_SLOW_TESTS", "CI", "GITHUB_ACTIONS",
                 "LANG", "LC_ALL", "LC_CTYPE", "LC_COLLATE",
                 "RENV_PROJECT", "RENV_CONFIG_AUTOLOADER_ENABLED",
                 "VAL_PIPELINE_PANDOC_DIR", "VAL_PIPELINE_BIOC_REPOS")
  # options() slots that gate covr / config behavior
  opt_names <- c("repos", "pkgType", "scipen", "encoding",
                 "val.pipeline.config_path",
                 "val.pipeline.log_file",
                 "val.pipeline.log_level",
                 "val.pipeline.capture_covr_skip_report",
                 "val.pipeline.covr_skip_report_threshold",
                 "val.pipeline.propagate_libpaths",
                 "val.pipeline.bioc_repos")
  # unexported helpers -- guard with tryCatch so the probe still saves
  # something useful if val.pipeline changes shape.
  pull_or_err <- function(fname, ...) {
    fn <- tryCatch(get(fname, envir = asNamespace("val.pipeline")),
                   error = function(e) NULL)
    if (is.null(fn)) return(paste0("<no ", fname, ">"))
    tryCatch(fn(...), error = function(e) paste0("<error: ",
                                                 conditionMessage(e), "\">"))
  }
  # installed.packages() as a data.frame keyed by pkg so we can merge
  # across two probes without positional surprises.
  ip <- as.data.frame(installed.packages()[, c("Package", "Version",
                                                "LibPath"),
                                            drop = FALSE],
                      stringsAsFactors = FALSE)

  list(
    label                     = label,
    when                      = Sys.time(),
    interactive               = interactive(),
    r_version                 = R.version.string,
    val_pipeline_version      = as.character(utils::packageVersion(
                                    "val.pipeline")),
    val_pipeline_lib          = tryCatch(find.package("val.pipeline"),
                                         error = function(e) NA_character_),
    libPaths                  = .libPaths(),
    installed                 = ip,
    getenv                    = Sys.getenv(env_names),
    getlocale                 = Sys.getlocale(),
    options                   = options()[opt_names],
    pull_covr_env_vars        = pull_or_err("pull_covr_env_vars"),
    pull_covr_path_env        = pull_or_err("pull_covr_path_env"),
    pull_covr_home_env        = pull_or_err("pull_covr_home_env"),
    resolve_covr_pandoc_dir   = pull_or_err("resolve_covr_pandoc_dir"),
    rmarkdown_pandoc_available = tryCatch(
      isTRUE(rmarkdown::pandoc_available()),
      error = function(e) paste0("<error: ", conditionMessage(e), ">"))
  )
}

# ---- Fire the target run, with the probe immediately before it -------------
stopifnot(MODE %in% c("val_build", "val_pipeline"))
probe_path <- file.path(PROBE_DIR, paste0("vp_probe_", MODE, ".rds"))

cat("\n=== vp_probe: MODE = ", MODE, " ===\n", sep = "")
cat("Probe target: ", probe_path, "\n", sep = "")
cat("val.pipeline: ", as.character(utils::packageVersion("val.pipeline")),
    " @ ", tryCatch(find.package("val.pipeline"),
                    error = function(e) "<lookup failed>"), "\n", sep = "")

if (MODE == "val_build") {
  saveRDS(probe_run_context("val_build"), probe_path)
  cat("Probe saved. Firing val_build() ...\n\n")
  qual <- val.pipeline::val_build(
    pkg_names          = c("logrx"),
    replace            = TRUE,
    workers            = 1,
    ref                = ref,
    metric_pkg         = metric_pkg,
    deps               = NULL,
    deps_recursive     = NULL,
    rev_deps           = NULL,
    rev_deps_recursive = NULL,
    val_date           = Sys.Date(),
    out                = out,
    opt_repos          = opt_repos,
    verbose            = "normal",
    config_path        = file.path(getwd(), "config.yml"),
    finalize           = FALSE
  )
} else {
  saveRDS(probe_run_context("val_pipeline_pre"), probe_path)
  cat("Probe saved. Firing val_pipeline() ...\n\n")
  qual <- val.pipeline::val_pipeline(
    # === Fill in EXACTLY what your production launcher passes ===
    # Keeping args explicit here so this script is self-documenting;
    # do NOT pass anything you don't normally pass, or the delta
    # you're chasing will get contaminated.
    pkg_names          = c("logrx"),
    replace            = TRUE,
    workers            = 1,
    ref                = ref,
    metric_pkg         = metric_pkg,
    deps               = NULL,
    deps_recursive     = NULL,
    rev_deps           = NULL,
    rev_deps_recursive = NULL,
    val_date           = Sys.Date(),
    out                = out,
    verbose            = "normal",
    config_path        = file.path(getwd(), "config.yml"),
    finalize           = FALSE
  )
}

cat("\n=== vp_probe done. Now switch MODE and rerun. ===\n")


# ---- DIFF ONLY (run in a third session after both probes exist) ------------
# Comment the block above and un-comment this block, then source the
# file, to render the diff. Or just copy-paste this block into an
# interactive REPL -- it doesn't need val.pipeline loaded.
if (FALSE) {
  a <- readRDS(file.path("/tmp", "vp_probe_val_build.rds"))
  b <- readRDS(file.path("/tmp", "vp_probe_val_pipeline.rds"))

  cat("\n--- SCALAR / VECTOR CONTEXT DIFF ---\n")
  # Drop installed.packages() from the waldo diff -- too big; handled
  # separately below.
  strip_installed <- function(x) x[setdiff(names(x), "installed")]
  print(waldo::compare(strip_installed(a), strip_installed(b),
                       max_diffs = Inf))

  cat("\n--- INSTALLED PKGS: version mismatches (or presence delta) ---\n")
  ai <- a$installed[, c("Package", "Version")]
  bi <- b$installed[, c("Package", "Version")]
  names(ai)[2] <- "Version.val_build"
  names(bi)[2] <- "Version.val_pipeline"
  diff_pkgs <- merge(ai, bi, by = "Package", all = TRUE)
  diff_pkgs <- diff_pkgs[with(diff_pkgs,
    is.na(Version.val_build) | is.na(Version.val_pipeline) |
      Version.val_build != Version.val_pipeline), ]
  # Sort by whether it's a logrx dep for readability (fill this in
  # from tools::package_dependencies("logrx", recursive = TRUE) if
  # you want to focus).
  diff_pkgs <- diff_pkgs[order(diff_pkgs$Package), ]
  print(diff_pkgs, row.names = FALSE, max = 400)

  cat("\n--- LIBPATH ORDER DIFF ---\n")
  cat("val_build .libPaths():\n"); print(a$libPaths)
  cat("\nval_pipeline .libPaths():\n"); print(b$libPaths)
}
