# -----------------------------------------------------------------------------
# dev/dev_diagnose_workbench_delta.R
#
# Purpose:
#   Compare the R execution *environment* between an interactive RStudio
#   session and a Posit Workbench job to explain why the same `val_build()`
#   call for `logrx` yields ~90% coverage interactively but ~59% in a job.
#
# How to use:
#   1. Open TWO fresh R sessions on the same box:
#        (a) an interactive RStudio session,
#        (b) a Workbench "Local Job" (or the same script launched via
#            rstudioapi::jobRunScript()) that runs THIS FILE.
#      For (b), copy this file's path and launch it as a job.
#   2. In each context, `source("dev/dev_diagnose_workbench_delta.R")`.
#      The script writes a fingerprint RDS to /tmp/vp_env_fingerprint_<label>.rds
#      and prints a summary to stdout.
#   3. After both have run, diff the two fingerprints with:
#        source("dev/dev_diagnose_workbench_delta.R"); vp_diff_fingerprints()
#      (Any R session with access to /tmp/ can run the diff.)
#
# The script also inspects the latest logrx assessment RDS on disk so we can
# see:
#   - `attr(p, "covr_skip_report")$totals`  -> n_error / n_fail vs n_skip
#   - `attr(p, "covr_skip_report")$top_reasons`
#   - per-file coverage breakdown from `p$covr_coverage`
#
# Nothing here writes into `/data/pm/` or the package tree. All artifacts
# land in /tmp/.
# -----------------------------------------------------------------------------

vp_env_fingerprint <- function(label = if (interactive()) "interactive" else "job") {
  list(
    label               = label,
    interactive         = interactive(),
    r_version           = as.character(getRversion()),
    which_pandoc        = unname(Sys.which("pandoc")),
    which_quarto        = unname(Sys.which("quarto")),
    RSTUDIO_PANDOC      = Sys.getenv("RSTUDIO_PANDOC",      unset = "<unset>"),
    QUARTO_PANDOC       = Sys.getenv("QUARTO_PANDOC",       unset = "<unset>"),
    PATH                = Sys.getenv("PATH"),
    R_LIBS_USER         = Sys.getenv("R_LIBS_USER",         unset = "<unset>"),
    R_LIBS_SITE         = Sys.getenv("R_LIBS_SITE",         unset = "<unset>"),
    R_LIBS              = Sys.getenv("R_LIBS",              unset = "<unset>"),
    LANG                = Sys.getenv("LANG",                unset = "<unset>"),
    LC_ALL              = Sys.getenv("LC_ALL",              unset = "<unset>"),
    RSTUDIO             = Sys.getenv("RSTUDIO",             unset = "<unset>"),
    RSTUDIO_SESSION_ID  = Sys.getenv("RSTUDIO_SESSION_ID",  unset = "<unset>"),
    RSTUDIO_USER_IDENTITY = Sys.getenv("RSTUDIO_USER_IDENTITY", unset = "<unset>"),
    libpaths            = .libPaths(),
    pandoc_dir_resolver = tryCatch(
      val.pipeline:::resolve_covr_pandoc_dir(),
      error = function(e) paste("<error>", conditionMessage(e))
    ),
    covr_path_env       = tryCatch(
      val.pipeline:::pull_covr_path_env(),
      error = function(e) paste("<error>", conditionMessage(e))
    ),
    rmarkdown_pandoc_available = tryCatch(
      rmarkdown::pandoc_available(),
      error = function(e) paste("<error>", conditionMessage(e))
    ),
    rmarkdown_pandoc_version = tryCatch(
      as.character(rmarkdown::pandoc_version()),
      error = function(e) paste("<error>", conditionMessage(e))
    ),
    sys_time            = format(Sys.time(), tz = "UTC", "%Y-%m-%d %H:%M:%S UTC")
  )
}


#' Print the fingerprint in a human-scannable form (long env vars folded).
vp_print_fingerprint <- function(fp) {
  cat("\n==== env_fingerprint: ", fp$label, " ============================\n", sep = "")
  cat(sprintf("  interactive()                 : %s\n", fp$interactive))
  cat(sprintf("  R version                     : %s\n", fp$r_version))
  cat(sprintf("  Sys.which('pandoc')           : %s\n", fp$which_pandoc))
  cat(sprintf("  Sys.which('quarto')           : %s\n", fp$which_quarto))
  cat(sprintf("  RSTUDIO_PANDOC                : %s\n", fp$RSTUDIO_PANDOC))
  cat(sprintf("  QUARTO_PANDOC                 : %s\n", fp$QUARTO_PANDOC))
  cat(sprintf("  RSTUDIO                       : %s\n", fp$RSTUDIO))
  cat(sprintf("  RSTUDIO_SESSION_ID            : %s\n", fp$RSTUDIO_SESSION_ID))
  cat(sprintf("  LANG                          : %s\n", fp$LANG))
  cat(sprintf("  LC_ALL                        : %s\n", fp$LC_ALL))
  cat(sprintf("  R_LIBS                        : %s\n", fp$R_LIBS))
  cat(sprintf("  R_LIBS_USER                   : %s\n", fp$R_LIBS_USER))
  cat(sprintf("  R_LIBS_SITE                   : %s\n", fp$R_LIBS_SITE))
  cat("  PATH:\n")
  for (p in strsplit(fp$PATH, .Platform$path.sep, fixed = TRUE)[[1]]) {
    cat(sprintf("    %s\n", p))
  }
  cat("  .libPaths():\n")
  for (p in fp$libpaths) cat(sprintf("    %s\n", p))
  cat(sprintf("  resolve_covr_pandoc_dir()     : %s\n",
              if (length(fp$pandoc_dir_resolver) == 0L) "<empty>"
              else paste(fp$pandoc_dir_resolver, collapse = ", ")))
  cat(sprintf("  pull_covr_path_env()['PATH']  : %s\n",
              if (length(fp$covr_path_env) == 0L) "<empty>"
              else substr(unname(fp$covr_path_env["PATH"]), 1, 200)))
  cat(sprintf("  rmarkdown::pandoc_available() : %s\n", fp$rmarkdown_pandoc_available))
  cat(sprintf("  rmarkdown::pandoc_version()   : %s\n", fp$rmarkdown_pandoc_version))
  cat(sprintf("  sys_time                      : %s\n", fp$sys_time))
  cat("=========================================================\n\n")
}


#' Load whichever of the two fingerprints exist and diff them side-by-side.
vp_diff_fingerprints <- function(dir = "/tmp") {
  fs <- list.files(dir, pattern = "^vp_env_fingerprint_.*\\.rds$",
                   full.names = TRUE)
  if (length(fs) < 2L) {
    cat("Need at least 2 fingerprint RDS files in", dir,
        "-- found:", length(fs), "\n")
    for (f in fs) cat("  ", f, "\n")
    return(invisible(NULL))
  }
  # Load newest 2 by mtime
  fs <- fs[order(file.info(fs)$mtime, decreasing = TRUE)][1:2]
  a  <- readRDS(fs[[1]]); b <- readRDS(fs[[2]])
  cat("Diffing (newest first):\n  A:", fs[[1]], "(label=", a$label, ")\n",
      " B:", fs[[2]], "(label=", b$label, ")\n\n", sep = "")

  keys <- union(names(a), names(b))
  for (k in keys) {
    av <- a[[k]]; bv <- b[[k]]
    same <- identical(av, bv)
    tag  <- if (same) "  ==" else "  !!"
    if (same) next
    cat(sprintf("%s %s\n", tag, k))
    cat("     A:", paste(utils::head(as.character(av), 6L), collapse = " | "), "\n")
    cat("     B:", paste(utils::head(as.character(bv), 6L), collapse = " | "), "\n")
  }
  invisible(list(a = a, b = b))
}


#' Inspect the on-disk logrx assessment artifact and print the totals +
#' per-file coverage.
vp_inspect_logrx_artifact <- function(
  path = "/data/pm/riskassessments/R_4.5.2/20260908/assessed/logrx_0.4.0_assessments.rds"
) {
  if (!file.exists(path)) {
    cat("No artifact at:", path, "\n")
    # Try to find the newest logrx assessment on disk under /data/pm/
    root <- "/data/pm/riskassessments"
    if (dir.exists(root)) {
      hits <- list.files(root, pattern = "^logrx_.*_assessments\\.rds$",
                         recursive = TRUE, full.names = TRUE)
      if (length(hits)) {
        hits <- hits[order(file.info(hits)$mtime, decreasing = TRUE)]
        cat("Candidates found (newest first):\n")
        for (h in utils::head(hits, 5L)) cat("  ", h, "\n")
      }
    }
    return(invisible(NULL))
  }
  p <- readRDS(path)
  cat("\n==== logrx artifact:", path, "==\n")
  cat("  mtime:", format(file.info(path)$mtime), "\n\n")

  csr <- attr(p, "covr_skip_report")
  cat("attr(p, 'covr_skip_report')$totals:\n")
  print(csr$totals)
  cat("\nattr(p, 'covr_skip_report')$top_reasons:\n")
  print(csr$top_reasons)

  cvt <- attr(p, "covr_caveat")
  cat("\nattr(p, 'covr_caveat'):\n")
  print(cvt)

  cov <- p$covr_coverage
  cat("\np$covr_coverage class:", paste(class(cov), collapse = "/"), "\n")
  if (inherits(cov, "coverage")) {
    cat("Overall percent_coverage:", covr::percent_coverage(cov), "\n\n")
    cat("Per-file coverage:\n")
    print(covr::coverage_to_list(cov)$filecoverage)
  } else if (is.list(cov) && !is.null(cov$file)) {
    cat("Per-file coverage (list column):\n")
    print(cov$file)
  } else {
    utils::str(cov, max.level = 2L)
  }
  invisible(p)
}


# -----------------------------------------------------------------------------
# Run when sourced
# -----------------------------------------------------------------------------
.label <- if (interactive()) "interactive" else "job"
.fp    <- vp_env_fingerprint(.label)
vp_print_fingerprint(.fp)

.out <- file.path("/tmp", sprintf("vp_env_fingerprint_%s.rds", .label))
saveRDS(.fp, .out)
cat("Saved fingerprint to:", .out, "\n\n")

cat("--- Inspecting logrx artifact ---\n")
vp_inspect_logrx_artifact()

cat("\nWhen both contexts have run, diff with:\n",
    "  source('dev/dev_diagnose_workbench_delta.R')\n",
    "  vp_diff_fingerprints()\n", sep = "")
