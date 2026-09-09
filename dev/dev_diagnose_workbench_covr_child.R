# -----------------------------------------------------------------------------
# dev/dev_diagnose_workbench_covr_child.R
#
# Purpose:
#   Confirm whether val_pkg's PATH prepend (`pull_covr_path_env()`) is
#   actually visible to covr's `R CMD BATCH --vanilla` child *inside a
#   workbench job*, and whether rmarkdown finds pandoc there.
#
# What it does:
#   1. Locates the logrx source tarball, extracts to /tmp/vp<n>/.
#   2. Overrides `covr:::run_commands` with a version that also writes a
#      subprocess-side fingerprint (PATH, RSTUDIO_PANDOC, .libPaths(),
#      Sys.which('pandoc'), rmarkdown::pandoc_available()) into a log
#      file *before* running the test commands. Original test commands
#      still run — coverage measurement is unaffected.
#   3. Wraps the covr call in the SAME `withr::with_envvar()` layer
#      val_pkg uses, so we exercise the real code path.
#   4. Prints the child-side fingerprint and per-file coverage side by
#      side, so we can see:
#         - did our PATH prepend reach the child? (child_path)
#         - did rmarkdown *see* pandoc there? (child_pandoc_available)
#         - which files lost coverage?
#
# How to run:
#   1. Launch as a Workbench Local Job so we reproduce the 59% context.
#   2. Also run in an interactive session to confirm the 90% context.
#   3. Compare the two /tmp/vp<n>/covr_child_fingerprint.rds files.
#
# All output lands in /tmp/. Nothing touches /data/pm/ or the package
# source tree.
# -----------------------------------------------------------------------------

# ---- Choose an isolated /tmp/vp<n>/ ------------------------------------------
pick_vp_dir <- function() {
  for (i in 1:999) {
    d <- sprintf("/tmp/vp%d", i)
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE)
      return(d)
    }
  }
  stop("Could not find a free /tmp/vp<n>/ slot")
}
vp_dir <- pick_vp_dir()
cat("Using vp_dir =", vp_dir, "\n")

# ---- Confirm required packages are visible ----------------------------------
stopifnot(requireNamespace("covr",       quietly = TRUE))
stopifnot(requireNamespace("rmarkdown",  quietly = TRUE))
stopifnot(requireNamespace("withr",      quietly = TRUE))
stopifnot(requireNamespace("riskmetric", quietly = TRUE))
suppressWarnings(suppressMessages(library(val.pipeline)))

# ---- Fetch logrx source tarball ---------------------------------------------
cat("\nDownloading logrx source ...\n")
opt_repos <- getOption("repos")
if (is.null(opt_repos) || !"CRAN" %in% names(opt_repos)) {
  opt_repos <- c(CRAN = "https://cloud.r-project.org")
  options(repos = opt_repos)
}
tarball_dir <- file.path(vp_dir, "tarball")
dir.create(tarball_dir)
tarball <- utils::download.packages(
  "logrx", destdir = tarball_dir, type = "source", quiet = TRUE
)[1, 2]
cat("  tarball:", tarball, "\n")

src_dir <- file.path(vp_dir, "src")
dir.create(src_dir)
utils::untar(tarball, exdir = src_dir)
pkg_source_path <- file.path(src_dir, "logrx")
stopifnot(dir.exists(pkg_source_path))
cat("  source :", pkg_source_path, "\n")

# ---- Fingerprint log file the covr child will write --------------------------
child_fp_log <- file.path(vp_dir, "covr_child_fingerprint.log")

# ---- Monkey-patch covr:::run_commands to also log child env ------------------
# The idea: prepend a small R block to the outfile that records the
# subprocess's actual view of PATH / RSTUDIO_PANDOC / .libPaths() /
# Sys.which('pandoc') / rmarkdown::pandoc_available(). Then let the
# original test commands run. All original semantics preserved; we just
# tack on a fingerprint at the top.
orig_run_commands <- covr:::run_commands
patched_run_commands <- function(pkg, lib, commands) {
  probe <- c(
    "cat(sprintf('== covr-child fingerprint (pid=%s) ==\\n', Sys.getpid()),",
    "    file = ", deparse(child_fp_log), ", append = TRUE)",
    "cat(sprintf('PATH               = %s\\n', Sys.getenv('PATH')),",
    "    file = ", deparse(child_fp_log), ", append = TRUE)",
    "cat(sprintf('RSTUDIO_PANDOC     = %s\\n', Sys.getenv('RSTUDIO_PANDOC', unset = '<unset>')),",
    "    file = ", deparse(child_fp_log), ", append = TRUE)",
    "cat(sprintf('R_LIBS_SITE        = %s\\n', Sys.getenv('R_LIBS_SITE', unset = '<unset>')),",
    "    file = ", deparse(child_fp_log), ", append = TRUE)",
    "cat(sprintf('R_LIBS             = %s\\n', Sys.getenv('R_LIBS', unset = '<unset>')),",
    "    file = ", deparse(child_fp_log), ", append = TRUE)",
    "cat(sprintf('.libPaths()        = %s\\n', paste(.libPaths(), collapse = ':')),",
    "    file = ", deparse(child_fp_log), ", append = TRUE)",
    "cat(sprintf('Sys.which(pandoc)  = %s\\n', Sys.which('pandoc')),",
    "    file = ", deparse(child_fp_log), ", append = TRUE)",
    "cat(sprintf('pandoc_available() = %s\\n',",
    "            tryCatch(rmarkdown::pandoc_available(), error = function(e) paste('<err>', conditionMessage(e)))),",
    "    file = ", deparse(child_fp_log), ", append = TRUE)",
    "cat(sprintf('pandoc_version()   = %s\\n',",
    "            tryCatch(as.character(rmarkdown::pandoc_version()), error = function(e) paste('<err>', conditionMessage(e)))),",
    "    file = ", deparse(child_fp_log), ", append = TRUE)",
    "cat('==\\n\\n', file = ", deparse(child_fp_log), ", append = TRUE)"
  )
  orig_run_commands(pkg, lib, c(probe, commands))
}
utils::assignInNamespace("run_commands", patched_run_commands, ns = "covr")
cat("\ncovr:::run_commands patched to log child fingerprint to:\n  ",
    child_fp_log, "\n", sep = "")

# ---- Parent-side fingerprint for comparison ---------------------------------
parent_fp <- list(
  label                    = if (interactive()) "interactive" else "job",
  PATH_before_with_envvar  = Sys.getenv("PATH"),
  RSTUDIO_PANDOC           = Sys.getenv("RSTUDIO_PANDOC", unset = "<unset>"),
  R_LIBS_SITE_before       = Sys.getenv("R_LIBS_SITE",    unset = "<unset>"),
  libpaths_before          = .libPaths(),
  pandoc_dir_resolver      = tryCatch(val.pipeline:::resolve_covr_pandoc_dir(),
                                      error = function(e) paste("<err>", conditionMessage(e))),
  covr_path_env_planned    = tryCatch(val.pipeline:::pull_covr_path_env(),
                                      error = function(e) paste("<err>", conditionMessage(e))),
  parent_rmarkdown_pandoc  = tryCatch(rmarkdown::pandoc_available(),
                                      error = function(e) paste("<err>", conditionMessage(e)))
)
cat("\n---- Parent-side fingerprint ----\n")
str(parent_fp, max.level = 1)

# ---- Also mirror .libPaths() into R_LIBS_SITE (matches val_build behavior) --
# val_build does this when propagate_libpaths = TRUE.
new_r_libs_site <- paste(.libPaths(), collapse = .Platform$path.sep)
old_r_libs_site <- Sys.getenv("R_LIBS_SITE", unset = NA_character_)

# ---- Run covr the way val_pkg does ------------------------------------------
cat("\n---- Running covr::package_coverage the val_pkg way ----\n")
t0 <- Sys.time()
res <- withr::with_envvar(
  new = c(
    R_LIBS_SITE = new_r_libs_site,
    val.pipeline:::pull_covr_env_vars(),
    val.pipeline:::pull_covr_path_env(),
    # #173: `pull_covr_home_env()` is the fix this diagnostic
    # motivated, so include it here to mirror val_pkg's actual
    # `with_envvar()` block. Without it, `rmarkdown::pandoc_available()`
    # in the parent-side probe below can *error* (not return FALSE)
    # in headless workbench-job contexts where HOME is unset, and
    # the whole script blows up before ever reaching the child
    # fingerprint the diagnostic is meant to capture.
    val.pipeline:::pull_covr_home_env()
  ),
  code = {
    # Confirm what the parent sees INSIDE with_envvar (this is what the
    # covr child inherits from).
    cat("Inside with_envvar():\n")
    cat("  PATH             = ", Sys.getenv("PATH"),        "\n", sep = "")
    cat("  HOME             = ", Sys.getenv("HOME", unset = "<unset>"), "\n", sep = "")
    cat("  RSTUDIO_PANDOC   = ", Sys.getenv("RSTUDIO_PANDOC", unset = "<unset>"), "\n", sep = "")
    cat("  R_LIBS_SITE      = ", Sys.getenv("R_LIBS_SITE"),  "\n", sep = "")
    cat("  Sys.which(pandoc)= ", Sys.which("pandoc"),        "\n", sep = "")
    cat("  pandoc_available = ",
        tryCatch(rmarkdown::pandoc_available(),
                 error = function(e) paste("<err>", conditionMessage(e))),
        "\n", sep = "")

    # This is exactly what riskmetric's pkg_ref_cache.covr_coverage.pkg_source does.
    expr <- bquote(tools::testInstalledPackage(.("logrx"), types = "tests"))
    covr::package_coverage(
      path = pkg_source_path,
      type = "none",
      code = deparse(expr)
    )
  }
)
t1 <- Sys.time()
cat("elapsed:", format(difftime(t1, t0, units = "secs")), "\n")

# ---- Report -----------------------------------------------------------------
cat("\n---- Coverage result ----\n")
cat("Overall percent_coverage =", covr::percent_coverage(res), "\n\n")
cat("Per-file coverage:\n")
print(covr::coverage_to_list(res)$filecoverage)

cat("\n---- Child-side fingerprint (from covr subprocess) ----\n")
if (file.exists(child_fp_log)) {
  cat(readLines(child_fp_log), sep = "\n")
} else {
  cat("(no child fingerprint log written -- covr subprocess may have failed to launch)\n")
}

# ---- Persist everything so we can diff interactive vs job -------------------
out_bundle <- file.path(vp_dir, sprintf("covr_child_bundle_%s.rds", parent_fp$label))
saveRDS(
  list(
    parent_fp    = parent_fp,
    child_fp_log = readLines(child_fp_log, warn = FALSE),
    coverage_pct = covr::percent_coverage(res),
    filecoverage = covr::coverage_to_list(res)$filecoverage,
    elapsed_secs = as.numeric(difftime(t1, t0, units = "secs"))
  ),
  out_bundle
)
cat("\nSaved bundle to:", out_bundle, "\n")
cat("Also, child fingerprint log at:", child_fp_log, "\n")
cat("\nRun this script in BOTH an interactive session AND a workbench job,\n",
    "then compare /tmp/vp<n>/covr_child_bundle_*.rds bundles.\n", sep = "")
