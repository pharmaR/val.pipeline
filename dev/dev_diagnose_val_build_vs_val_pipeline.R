# Diagnose the val_build() vs val_pipeline() covr_coverage delta
# ==============================================================
#
# Symptom (see agent thread 2026-09-14):
#   * `val_build(pkg_names = "logrx", ...)` in a workbench job returns
#     `covr_coverage > 90%`.
#   * `val_pipeline(...)` in the SAME workbench job, with the same
#     hard-pinned CRAN snapshot (`.../cran-r4.5-2026-08-15/latest`),
#     returns `covr_coverage = 62%` for the same logrx 0.4.0 tarball.
#
# The tarball is identical between the two runs, so the delta must
# live in what `val_pipeline()` does to session state BEFORE it hands
# off to `val_build()`. Concretely, `val_pipeline()`'s only prelude is
# `val_prep_pipeline()` -- which does:
#     * configure_bioc_repositories_if_requested()
#     * pull_config("opt_repos"); options(repos = ...)
#     * update_opt_repos() (no-op when URL is date-pinned, as in the
#       reproducer)
#     * options(repos = ..., pkgType = "source", scipen = 999)
#     * write_pipeline_toml() (filesystem only)
# and returns a `val_prep` object. Whichever of those steps shifts
# `.libPaths()`, an env var, an `options()` slot, or `covr_env_vars`
# is what the covr subprocess in `val_pkg()` will see differently.
#
# **This script never fires a full assessment.** It probes the
# session state once BEFORE val_prep_pipeline() (that's the state a
# bare `val_build()` launcher would call into) and once AFTER
# val_prep_pipeline() but before it would call val_build() (that's
# the state val_pipeline() would call into). Then it diffs them and
# prints the delta. Runs in ~30s in a workbench job. No covr, no
# per-package assessment, no 1545-pkg 40-hour marathon.
#
# HOW TO USE
# ----------
# 1. Copy this script into the workbench project that reproduces the
#    delta (the one whose config's `opt_repos` is hard-pinned to
#    2026-08-15). Do NOT edit it against the source tree here -- it
#    needs to see YOUR real config / opt_repos / working directory.
# 2. Source your usual launcher setup (whatever defines `opt_repos`,
#    `out`, `ref`, `metric_pkg` for your regular val_build script).
# 3. Source THIS file. It writes a single combined probe file to
#    /tmp/vp_probe_pair.rds and prints the diff on stdout. Nothing
#    to switch between jobs; nothing to wait hours for.
# 4. Ping the agent with the diff output.

# ---- Config: adjust ONLY if you know why -----------------------------------
PROBE_DIR   <- "/tmp"
CONFIG_PATH <- file.path(getwd(), "config.yml")  # same one val_pipeline reads

# ---- Probe helper -----------------------------------------------------------
probe_run_context <- function(label) {
  env_names <- c("PATH", "HOME", "R_LIBS_SITE", "R_LIBS_USER", "R_LIBS",
                 "RSTUDIO_PANDOC", "RSTUDIO", "RSTUDIO_USER_IDENTITY",
                 "NOT_CRAN", "TESTTHAT", "_R_CHECK_FORCE_SUGGESTS_",
                 "NOT_ON_CRAN", "RUN_SLOW_TESTS", "CI", "GITHUB_ACTIONS",
                 "LANG", "LC_ALL", "LC_CTYPE", "LC_COLLATE",
                 "RENV_PROJECT", "RENV_CONFIG_AUTOLOADER_ENABLED",
                 "VAL_PIPELINE_PANDOC_DIR", "VAL_PIPELINE_BIOC_REPOS")
  opt_names <- c("repos", "pkgType", "scipen", "encoding",
                 "val.pipeline.config_path",
                 "val.pipeline.log_file",
                 "val.pipeline.log_level",
                 "val.pipeline.capture_covr_skip_report",
                 "val.pipeline.covr_skip_report_threshold",
                 "val.pipeline.propagate_libpaths",
                 "val.pipeline.bioc_repos")

  # Guard unexported helpers so a missing internal doesn't kill the probe.
  pull_or_err <- function(fname, ...) {
    fn <- tryCatch(get(fname, envir = asNamespace("val.pipeline")),
                   error = function(e) NULL)
    if (is.null(fn)) return(paste0("<no ", fname, ">"))
    tryCatch(fn(...),
             error = function(e) paste0("<error: ",
                                        conditionMessage(e), ">"))
  }

  ip <- as.data.frame(
    installed.packages()[, c("Package", "Version", "LibPath"), drop = FALSE],
    stringsAsFactors = FALSE
  )

  list(
    label                      = label,
    when                       = Sys.time(),
    interactive                = interactive(),
    r_version                  = R.version.string,
    val_pipeline_version       = as.character(utils::packageVersion(
                                     "val.pipeline")),
    val_pipeline_lib           = tryCatch(find.package("val.pipeline"),
                                          error = function(e) NA_character_),
    libPaths                   = .libPaths(),
    installed                  = ip,
    getenv                     = Sys.getenv(env_names),
    getlocale                  = Sys.getlocale(),
    options                    = options()[opt_names],
    pull_covr_env_vars         = pull_or_err("pull_covr_env_vars"),
    pull_covr_path_env         = pull_or_err("pull_covr_path_env"),
    pull_covr_home_env         = pull_or_err("pull_covr_home_env"),
    resolve_covr_pandoc_dir    = pull_or_err("resolve_covr_pandoc_dir"),
    rmarkdown_pandoc_available = tryCatch(
      isTRUE(rmarkdown::pandoc_available()),
      error = function(e) paste0("<error: ", conditionMessage(e), ">"))
  )
}

# ---- Snapshot 1: pre-prep (bare val_build() would call into this) ----------
cat("\n=== vp_probe: snapshot 1 (BEFORE val_prep_pipeline) ===\n")
before <- probe_run_context("val_build_state")

# ---- val_prep_pipeline() -- cheap, no covr, no per-pkg assessment -----------
#
# val_prep_pipeline() does NOT take `pkg_names`. It resolves the whole
# candidate universe from `{riskscore}` metadata (has_website,
# has_maintainer, etc. -- fast metrics; no covr), then filters via
# `remote_reduce` config and resolves the dep tree. That's the exact
# session-state prelude val_pipeline() runs before val_build(). It's
# minutes for a ~1.5k pkg universe, not the days a full assessment
# would take.
#
# Match your launcher's val_pipeline() args as closely as possible. If
# you pass `freeze_opt_repos = TRUE` (or any other non-default arg) to
# val_pipeline() in production, mirror it here.
cat("\n=== vp_probe: running val_prep_pipeline() ",
    "(candidate reduction + dep tree only; no covr) ===\n", sep = "")
prep <- val.pipeline::val_prep_pipeline(
  val_date          = Sys.Date(),
  out               = out,
  verbose           = "normal",
  config_path       = CONFIG_PATH
  # freeze_opt_repos = TRUE,   # <-- add if your launcher passes it
  # ref              = "source",
  # metric_pkg       = "riskmetric",
)

# ---- Snapshot 2: post-prep (val_pipeline() would call val_build here) ------
cat("\n=== vp_probe: snapshot 2 (AFTER val_prep_pipeline, pre-val_build) ===\n")
after <- probe_run_context("val_pipeline_state")

pair_path <- file.path(PROBE_DIR, "vp_probe_pair.rds")
saveRDS(list(before = before, after = after, prep_val_date = prep$val_date,
             prep_opt_repos = prep$opt_repos,
             prep_n_pkgs = length(prep$pkgs)),
        pair_path)
cat("\nProbe pair saved to: ", pair_path, "\n", sep = "")

# ---- Diff --------------------------------------------------------------------
strip_installed <- function(x) x[setdiff(names(x), "installed")]

cat("\n\n============================================================\n")
cat("SCALAR / VECTOR CONTEXT DIFF (before -> after val_prep_pipeline)\n")
cat("============================================================\n")
if (requireNamespace("waldo", quietly = TRUE)) {
  print(waldo::compare(strip_installed(before), strip_installed(after),
                       max_diffs = Inf))
} else {
  # Waldo not available -- fall back to a plain field-by-field diff.
  a <- strip_installed(before)
  b <- strip_installed(after)
  for (nm in union(names(a), names(b))) {
    if (!identical(a[[nm]], b[[nm]])) {
      cat("\n!! ", nm, "\n", sep = "")
      cat("  BEFORE: "); print(a[[nm]])
      cat("  AFTER : "); print(b[[nm]])
    }
  }
}

cat("\n\n============================================================\n")
cat("INSTALLED PKGS: version mismatches or presence delta\n")
cat("(only relevant if val_prep_pipeline touched .libPaths / installed)\n")
cat("============================================================\n")
ai <- before$installed[, c("Package", "LibPath", "Version"), drop = FALSE]
bi <- after$installed[,  c("Package", "LibPath", "Version"), drop = FALSE]
ai <- as.data.frame(ai, stringsAsFactors = FALSE)
bi <- as.data.frame(bi, stringsAsFactors = FALSE)
names(ai)[3] <- "Version.before"
names(bi)[3] <- "Version.after"
# Merge on (Package, LibPath) — installed.packages() has one row per
# pkg PER library slot, so a Package-only merge Cartesian-products
# any pkg that appears in >1 slot (e.g. `.libPaths()` position 1 and
# 3), yielding phantom "before != after" rows that are just row
# reordering artifacts.
diff_pkgs <- merge(ai, bi, by = c("Package", "LibPath"), all = TRUE)
diff_pkgs <- diff_pkgs[with(diff_pkgs,
  is.na(Version.before) | is.na(Version.after) |
    Version.before != Version.after), ]
diff_pkgs <- diff_pkgs[order(diff_pkgs$Package), ]
if (nrow(diff_pkgs) == 0L) {
  cat("(no installed-pkg changes -- val_prep_pipeline did not shift the ",
      "library layout)\n", sep = "")
} else {
  print(diff_pkgs, row.names = FALSE, max = 400)
}

cat("\n\n============================================================\n")
cat("LIBPATH ORDER (before -> after)\n")
cat("============================================================\n")
cat("BEFORE .libPaths():\n"); print(before$libPaths)
cat("\nAFTER  .libPaths():\n"); print(after$libPaths)

cat("\n=== vp_probe done. Send the output above to the agent. ===\n")
