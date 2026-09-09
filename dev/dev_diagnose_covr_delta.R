# dev/dev_diagnose_covr_delta.R
#
# Purpose ---------------------------------------------------------------------
# Diagnose *why* `riskmetric`'s `covr_coverage` for a given package comes in
# well below the coverage reported by the package's own CI (e.g. `logrx` at
# ~89% on the pharmaverse GH Actions run vs. ~58% in a `val.pipeline`
# assessment). The suspicion (see conversation with Aaron 2026-09-07) is
# NOT that riskmetric mis-computes the metric — it's that `riskmetric`'s
# custom `covr::package_coverage(type = "none", code = "tools::testInstalledPackage(...)")`
# invocation **silently swallows test-file errors** (a deliberate choice in
# `riskmetric:::pkg_ref_cache.covr_coverage.pkg_source`) so any test file
# that dies during `setup` — usually because a `Suggests:` dep or a system
# binary like pandoc is missing in the covr sandbox — loses its whole
# coverage contribution with no signal in the assessment.
#
# This script reproduces both invocations back-to-back against the SAME
# extracted source tree of the target package, using the same env-var
# normalization `val_pkg()` uses (`pull_covr_env_vars()` -> `NOT_CRAN=true`,
# `TESTTHAT=true`, ...), and dumps:
#
#   1. A capability probe: which Suggests deps are on `.libPaths()`,
#      whether `pandoc` is on `PATH`.
#   2. The "loud" reference run: `covr::package_coverage(path = SRC,
#      type = "tests", quiet = FALSE, clean = FALSE)` — i.e. what
#      `admiralci`'s workflow does. Any test-file failure raises a
#      `covr_error` via `show_failures()` and we log the offending file
#      + error body.
#   3. The riskmetric-style run: `covr::package_coverage(path = SRC,
#      type = "none", code = "tools::testInstalledPackage(name, types='tests')",
#      quiet = FALSE, clean = FALSE)` — coverage number for direct
#      apples-to-apples comparison.
#   4. Per-file coverage tables for both, side by side, so files whose
#      coverage collapses from ~1.0 to 0 in the riskmetric-style run pin
#      the failing test file.
#
# Usage -----------------------------------------------------------------------
#   1. Optionally edit `PKG` / `SRC_DIR` below. Defaults target the
#      currently-installed `logrx` sources via `download.packages()` into
#      a temp dir, mirroring how `val_pkg()` gets the source.
#   2. Run from the val.pipeline repo root:
#          /opt/R/4.5.2/bin/Rscript --no-init-file \
#            dev/dev_diagnose_covr_delta.R
#   3. Coverage numbers + a per-file table + the location of the full log
#      print at the end. Paste the tail of the log back to the assistant
#      if you want a second pass.
#
# Not shipped with the package (`.Rbuildignore`'d under dev/).
# ---------------------------------------------------------------------------

# ---- EDIT ME ----------------------------------------------------------------
PKG        <- "logrx"
# Leave SRC_DIR = NULL to download the current CRAN tarball into tempdir()
# and extract it (this is what val.pipeline does under the hood). Set it
# to an already-extracted source tree to skip the download step (e.g. a
# fresh `git clone` of pharmaverse/logrx if you want to diff dev vs. CRAN).
SRC_DIR    <- NULL
# Repos to pull the tarball from when SRC_DIR is NULL.
REPOS      <- c(CRAN = "https://cloud.r-project.org")
# Extra library paths to PREPEND to `.libPaths()` before the capability
# probe runs. Point this at your `rv` / `renv` library dir if the driver
# R session that sources this script doesn't already have it on the
# search path -- otherwise every Suggests dep installed there shows up
# as "missing" in the probe (see val.pipeline #169). Set to
# `character(0)` (default) to leave `.libPaths()` untouched.
#
# Common values:
#   - Sys.getenv("R_LIBS_USER")     # user default
#   - "/data/rv/current"            # site rv install
#   - renv::paths$library()         # renv project library (interactive R)
EXTRA_LIB_PATHS <- character(0)
# When TRUE, mirror the *driver session's* `.libPaths()` into
# `R_LIBS_SITE` for the duration of each `covr::package_coverage()`
# call so the spawned Rscript subprocess sees the same library search
# order (mirrors val_build()'s `propagate_libpaths` mechanism, see
# `R/val_build.R` around L210 -- restored on exit via
# `withr::with_envvar`). A subprocess NEVER inherits an interactive
# `.libPaths()` from the parent -- it rebuilds from
# `R_LIBS_SITE`/`R_LIBS_USER`/site defaults -- so without this, a
# Suggests dep sitting only in a session-added libPath will be
# invisible to covr's test-run subprocess and any
# `skip_if_not_installed()` guard for it will silently fire even
# though the driver session (and this script's capability probe) can
# see it just fine. Set FALSE to reproduce the un-mirrored behavior.
PROPAGATE_LIBPATHS <- getOption("val.pipeline.propagate_libpaths", TRUE)
# ---------------------------------------------------------------------------

# ---- Pandoc discovery (dev-only) ------------------------------------------
# Some pkgs' tests (e.g. `logrx::axecute()`) need pandoc on PATH; the whole
# point of this script is to reproduce that failure mode, so ensure pandoc
# is available before covr spawns its test subprocesses. When run via
# `Rscript` from a terminal, this R process only sees the shell's PATH at
# launch, so setting env in an interactive session earlier won't carry
# over -- do the discovery here instead. No-op when pandoc is already on
# PATH. Mirrors the auto-detect logic that `resolve_covr_pandoc_dir()`
# performs at production call sites (see R/utils.R, #167).
if (!nzchar(Sys.which("pandoc"))) {
  q <- Sys.glob("/opt/quarto/*/bin/tools/*/pandoc")
  q <- q[file.exists(q)]
  if (length(q)) {
    # Highest quarto version wins.
    vers <- sub("^/opt/quarto/([^/]+)/.*$", "\\1", q)
    ord  <- order(numeric_version(vers, strict = FALSE), decreasing = TRUE)
    Sys.setenv(PATH = paste(
      dirname(q[ord[1]]),
      Sys.getenv("PATH"),
      sep = .Platform$path.sep
    ))
    message("--> Prepended bundled Quarto pandoc dir to PATH: ",
            dirname(q[ord[1]]))
  } else {
    message("--> pandoc not on PATH and no /opt/quarto/*/bin/tools/*/pandoc ",
            "found; the loud + riskmetric-style runs will both under-report ",
            "coverage on pandoc-dependent test files. Install pandoc (or ",
            "adjust the discovery glob above) and re-run.")
  }
}

# devtools::load_all(quiet = TRUE)

# ---- Library resolution ---------------------------------------------------
# Prepend any user-supplied EXTRA_LIB_PATHS onto `.libPaths()` for the
# rest of this session so the capability probe below (and any
# `find.package()` in the covr driver) can see packages the shell that
# launched R didn't already put on the search path. Common trigger:
# running via `Rscript --no-init-file` (which skips `.Rprofile` and
# therefore renv/rv activation) against an rv-provisioned library --
# the packages exist on disk but `.libPaths()` doesn't know about
# them. See val.pipeline #169.
if (length(EXTRA_LIB_PATHS)) {
  keep <- vapply(EXTRA_LIB_PATHS, dir.exists, logical(1))
  if (any(!keep)) {
    message("--> Skipping non-existent EXTRA_LIB_PATHS entries: ",
            paste(EXTRA_LIB_PATHS[!keep], collapse = ", "))
  }
  if (any(keep)) {
    .libPaths(c(EXTRA_LIB_PATHS[keep], .libPaths()))
    message("--> Prepended to .libPaths(): ",
            paste(EXTRA_LIB_PATHS[keep], collapse = ", "))
  }
}

log_path <- tempfile(
  pattern = paste0("covr_delta_", PKG, "_"),
  tmpdir  = "/tmp",
  fileext = ".log"
)
message("--> Full diagnostic log: ", log_path)
log_con <- file(log_path, open = "wt")
tee <- function(...) {
  msg <- paste0(..., collapse = "")
  cat(msg, file = log_con, sep = "")
  cat(msg)
}
on.exit(close(log_con), add = TRUE)

hdr <- function(x) tee("\n\n==== ", x, " ", strrep("=", max(0, 70 - nchar(x))), "\n")

# ---- 0. Source ------------------------------------------------------------

if (is.null(SRC_DIR)) {
  hdr(paste0("Downloading & extracting `", PKG, "` source tarball"))
  tmp <- tempfile(paste0(PKG, "_src_"))
  dir.create(tmp)
  utils::download.packages(
    pkgs = PKG, destdir = tmp, type = "source", repos = REPOS
  )
  tarball <- list.files(tmp, pattern = "\\.tar\\.gz$", full.names = TRUE)[1]
  stopifnot("Tarball download failed" = length(tarball) == 1 && file.exists(tarball))
  utils::untar(tarball, exdir = tmp)
  SRC_DIR <- file.path(tmp, PKG)
  tee("  Tarball : ", tarball, "\n")
  tee("  Extracted: ", SRC_DIR, "\n")
}
stopifnot("SRC_DIR does not exist" = dir.exists(SRC_DIR))
tee("  Using source tree: ", SRC_DIR, "\n")

# ---- 1. Capability probe --------------------------------------------------

hdr("Library search order (.libPaths() + env)")
tee("  .libPaths():\n")
for (i in seq_along(.libPaths())) {
  tee(sprintf("    [%d] %s\n", i, .libPaths()[i]))
}
tee("  R_LIBS_USER = ",
    Sys.getenv("R_LIBS_USER", unset = "<unset>"), "\n")
tee("  R_LIBS_SITE = ",
    Sys.getenv("R_LIBS_SITE", unset = "<unset>"), "\n")
tee("  R_LIBS      = ",
    Sys.getenv("R_LIBS",      unset = "<unset>"), "\n")
tee("  RENV_PROJECT = ",
    Sys.getenv("RENV_PROJECT", unset = "<unset>"), "\n")
if (nzchar(Sys.getenv("RENV_PROJECT")) &&
    requireNamespace("renv", quietly = TRUE)) {
  tee("  renv::paths$library() = ",
      tryCatch(renv::paths$library(), error = function(e) "<error>"), "\n")
}
tee("\n  NOTE: if a Suggests dep below is FALSE but you *know* it's\n")
tee("        installed, the odds are that its lib dir isn't on the\n")
tee("        search order above. Set EXTRA_LIB_PATHS at the top of\n")
tee("        this script to prepend it, then re-run.\n")

hdr("Capability probe")

desc <- read.dcf(file.path(SRC_DIR, "DESCRIPTION"))
imports <- unlist(strsplit(gsub("\\s*\\([^)]*\\)", "",
                                desc[, "Imports"] %||% ""), ","))
suggests <- unlist(strsplit(gsub("\\s*\\([^)]*\\)", "",
                                 desc[, "Suggests"] %||% ""), ","))
imports  <- trimws(imports)
suggests <- trimws(suggests)
imports  <- imports[nzchar(imports)]
suggests <- suggests[nzchar(suggests)]

has_pkg <- function(p) length(find.package(p, quiet = TRUE)) > 0
imports_status <- vapply(imports, has_pkg, logical(1))
suggests_status <- vapply(suggests, has_pkg, logical(1))

tee("  Imports  (should ALL be TRUE):\n")
for (n in names(imports_status)) {
  tee("    ", format(n, width = 22), " ", imports_status[[n]], "\n")
}
tee("  Suggests (missing here => test-file setup blocks can error):\n")
for (n in names(suggests_status)) {
  tee("    ", format(n, width = 22), " ", suggests_status[[n]], "\n")
}

pandoc_path <- Sys.which("pandoc")
tee("  pandoc on PATH: ",
    if (nzchar(pandoc_path)) pandoc_path else "**NOT FOUND**", "\n")

if (requireNamespace("rmarkdown", quietly = TRUE)) {
  rm_pandoc <- tryCatch(rmarkdown::pandoc_available(),
                        error = function(e) FALSE)
  tee("  rmarkdown::pandoc_available(): ", rm_pandoc, "\n")
}

missing_suggests <- names(suggests_status)[!suggests_status]
if (length(missing_suggests)) {
  tee("\n  !! Missing Suggests: ", paste(missing_suggests, collapse = ", "), "\n")
  tee("     These are the most likely drivers of the coverage delta.\n")
  tee("     Install with e.g.:\n")
  tee("       install.packages(c(",
      paste(paste0("\"", missing_suggests, "\""), collapse = ", "),
      "))\n")
}

# ---- 2. Env-var normalization (mirror val_pkg()) --------------------------

hdr("Env-var normalization (via pull_covr_env_vars())")

covr_env <- tryCatch(pull_covr_env_vars(), error = function(e) {
  tee("  pull_covr_env_vars() failed: ", conditionMessage(e), "\n")
  character(0)
})
for (n in names(covr_env)) {
  tee("    ", format(n, width = 28), " = ", covr_env[[n]], "\n")
}

# ---- 2b. Propagate .libPaths() into R_LIBS_SITE ---------------------------
# Mirror the driver session's `.libPaths()` into `R_LIBS_SITE` for the
# duration of the covr calls below, so the Rscript subprocess covr
# spawns to run the test suite sees the same library search order as
# this driver session. This is exactly what `val_build()` does when
# `propagate_libpaths = TRUE` (see R/val_build.R L210, gated on
# `getOption("val.pipeline.propagate_libpaths", TRUE)`) -- reused here
# so `Suggests:` deps that were only visible after the EXTRA_LIB_PATHS
# prepend above (or after RStudio activated renv on session start,
# etc.) don't silently vanish once covr spawns its child. Without this
# mirror, a `skip_if_not_installed("readr")` in a test file will fire
# in the child even when the driver session's `.libPaths()` *can* see
# `readr`, because the child rebuilds its search order from env vars +
# site defaults.
if (isTRUE(PROPAGATE_LIBPATHS)) {
  covr_env <- c(
    covr_env,
    R_LIBS_SITE = paste(.libPaths(), collapse = .Platform$path.sep)
  )
  tee("\n  --> propagate_libpaths = TRUE: mirroring .libPaths() into\n")
  tee("      R_LIBS_SITE for covr subprocesses (mirrors val_build()).\n")
  tee("      R_LIBS_SITE = ", covr_env[["R_LIBS_SITE"]], "\n")
} else {
  tee("\n  --> propagate_libpaths = FALSE: R_LIBS_SITE left untouched.\n")
  tee("      Any Suggests only visible on the driver .libPaths() will be\n")
  tee("      invisible to covr's test-run subprocess.\n")
}

# ---- 3. Loud reference run (covr default `type = 'tests'`) ---------------

hdr("Loud run: covr::package_coverage(type = 'tests') -- admiralci-style")

t0 <- Sys.time()
loud <- withr::with_envvar(
  new = covr_env,
  code = tryCatch(
    covr::package_coverage(
      path       = SRC_DIR,
      type       = "tests",
      quiet      = FALSE,
      clean      = FALSE,
      pre_clean  = TRUE
    ),
    error = function(e) {
      tee("\n  !! package_coverage(type='tests') threw:\n")
      tee("     ", conditionMessage(e), "\n")
      # covr wraps show_failures() errors as class 'covr_error'; the message
      # body is the tail of the failing .fail file, which pins the offending
      # test file.
      e
    }
  )
)
tee("  elapsed: ",
    format(round(difftime(Sys.time(), t0, units = "secs"), 1)), "\n")

loud_pct <- if (inherits(loud, "coverage")) {
  covr::percent_coverage(loud)
} else NA_real_
tee("  covr::percent_coverage()  = ", format(loud_pct, digits = 4), "\n")

if (inherits(loud, "coverage")) {
  loud_list <- covr::coverage_to_list(loud)
  tee("  covr::coverage_to_list()$totalcoverage = ",
      format(loud_list$totalcoverage, digits = 4), "\n")
}

# ---- 4. riskmetric-style run (type = 'none' + custom code) ---------------

hdr("riskmetric-style run: type = 'none', code = testInstalledPackage()")

# Verbatim of `riskmetric:::pkg_ref_cache.covr_coverage.pkg_source`:
#   expr <- bquote(tools::testInstalledPackage(.(pkg_name), types = 'tests'))
#   covr::package_coverage(path = path, type = "none", code = deparse(expr))
expr <- bquote(tools::testInstalledPackage(.(PKG), types = "tests"))
rm_code <- deparse(expr)
tee("  code = ", paste(rm_code, collapse = "\n         "), "\n")

t0 <- Sys.time()
rm_cov <- withr::with_envvar(
  new = covr_env,
  code = tryCatch(
    covr::package_coverage(
      path      = SRC_DIR,
      type      = "none",
      code      = rm_code,
      quiet     = FALSE,
      clean     = FALSE,
      pre_clean = TRUE
    ),
    error = function(e) {
      tee("\n  !! riskmetric-style package_coverage() threw:\n")
      tee("     ", conditionMessage(e), "\n")
      e
    }
  )
)
tee("  elapsed: ",
    format(round(difftime(Sys.time(), t0, units = "secs"), 1)), "\n")

rm_pct <- if (inherits(rm_cov, "coverage")) {
  covr::percent_coverage(rm_cov)
} else NA_real_
tee("  covr::percent_coverage()  = ", format(rm_pct, digits = 4), "\n")

if (inherits(rm_cov, "coverage")) {
  rm_list <- covr::coverage_to_list(rm_cov)
  tee("  covr::coverage_to_list()$totalcoverage = ",
      format(rm_list$totalcoverage, digits = 4), "\n")
}

# ---- 5. Side-by-side per-file coverage ------------------------------------

hdr("Per-file coverage: loud vs. riskmetric-style")

if (inherits(loud, "coverage") && inherits(rm_cov, "coverage")) {
  loud_file  <- covr::coverage_to_list(loud)$filecoverage
  rm_file    <- covr::coverage_to_list(rm_cov)$filecoverage
  files      <- sort(unique(c(names(loud_file), names(rm_file))))
  cmp <- data.frame(
    file          = files,
    loud_pct      = as.numeric(loud_file[files]),
    riskmetric_pct = as.numeric(rm_file[files]),
    stringsAsFactors = FALSE
  )
  cmp$delta_pp <- cmp$loud_pct - cmp$riskmetric_pct
  cmp <- cmp[order(-abs(cmp$delta_pp)), , drop = FALSE]

  tee("  Ranked by |delta| (positive = loud is higher, i.e. riskmetric lost coverage on that file):\n\n")
  fmt <- function(x) formatC(x, digits = 1, format = "f", width = 6)
  tee(sprintf("    %-40s %8s %8s %8s\n",
              "file", "loud", "rmetric", "delta"))
  tee(sprintf("    %-40s %8s %8s %8s\n",
              strrep("-", 40), "------", "------", "------"))
  for (i in seq_len(nrow(cmp))) {
    tee(sprintf("    %-40s %8s %8s %8s\n",
                substr(cmp$file[i], 1, 40),
                fmt(cmp$loud_pct[i]),
                fmt(cmp$riskmetric_pct[i]),
                fmt(cmp$delta_pp[i])))
  }

  collapsed <- cmp[!is.na(cmp$delta_pp) & cmp$delta_pp >= 20, , drop = FALSE]
  if (nrow(collapsed) > 0) {
    tee("\n  Files whose coverage collapsed by >= 20pp under riskmetric-style:\n")
    for (i in seq_len(nrow(collapsed))) {
      tee("    ", collapsed$file[i], " (", fmt(collapsed$loud_pct[i]),
          " -> ", fmt(collapsed$riskmetric_pct[i]), ")\n")
    }
    tee("\n  --> Grep `tests/testthat/test-*.R` for `library(<setup dep>)` or\n")
    tee("      `rmarkdown::render`/`pandoc` calls; the failing file is almost\n")
    tee("      certainly one that exercises those R/ files.\n")
  }
} else {
  tee("  Skipped -- one of the two runs did not produce a coverage object.\n")
}

# ---- 5b. val.pipeline path (val_build with workers=2) ---------------------
# Full-fat single-pkg val.pipeline run, so we can verify that the
# multisession + propagate_libpaths + val_pkg() code path lands the
# same coverage number as the two bare covr runs above. `workers = 2`
# with a single-pkg input forces the future.apply multisession
# branch to actually spawn (workers > 1L in val_build.R), even
# though the second worker sits idle -- the point is to reproduce
# the parallel-mode env-var + libpath propagation chain end-to-end.
# `deps = NULL, deps_recursive = FALSE` skips the dep tree so the
# run stays fast; `finalize = TRUE` runs val_build's collation half
# so `qual_assessments.rds` gets written (val_build alone only
# emits per-package `assessed/<pkg>_<ver>_assess_record.rds`).
#
# When the number here MATCHES the loud + riskmetric-style runs
# above, the whole propagate_libpaths path is confirmed working
# on this host. When it DIVERGES, the delta pins the issue to
# something val.pipeline is doing on top of the raw covr call --
# usually a libpath that made it onto the parent .libPaths() but
# didn't reach the worker's R_LIBS_SITE.

hdr("val.pipeline path: val_build(workers=2, deps=NULL) end-to-end")

# Per-run output dir under /tmp/. Auto-increments `vp<N>` so a
# fresh source() lands in a new dir (`/tmp/vp1`, then `/tmp/vp2`,
# ...) and old runs stay browsable without clobber. Deliberately
# NOT tempfile()-based -- those pick a random `RtmpXXXXX/` subdir
# that's cleaned up when R exits and is a nuisance to look up
# later; `/tmp/vp<N>` is easy to `cd` into from a shell.
next_vp_dir <- function(base = "/tmp") {
  existing <- list.files(base, pattern = "^vp[0-9]+$",
                         include.dirs = TRUE, full.names = FALSE)
  ns <- suppressWarnings(as.integer(sub("^vp", "", existing)))
  ns <- ns[!is.na(ns)]
  next_n <- if (length(ns) == 0L) 1L else max(ns) + 1L
  file.path(base, paste0("vp", next_n))
}
vb_out <- next_vp_dir()
dir.create(vb_out, recursive = TRUE, showWarnings = FALSE)
tee("  out dir: ", vb_out, "\n")

t0 <- Sys.time()
vb <- tryCatch(
  val_build(
    pkg_names       = PKG,
    ref             = "source",
    deps            = NULL,
    deps_recursive  = FALSE,
    workers         = 2L,
    out             = vb_out,
    # `opt_repos` must match a mirror that actually serves the pkg,
    # or `available.packages()` returns an empty frame and
    # `resolve_pkg_tree(deps = NULL)` filters PKG out (via
    # `filter(Package %in% avail_pkgs$Package)`) -- val_build then
    # enters the assessment loop with 0 pkgs, writes no
    # `assess_record.rds`, and val_finalize aborts with "No
    # `_assess_record.rds` files found ...". Reuse the same
    # `REPOS` string as the top-of-script tarball download so the
    # two legs of the script talk to the same mirror.
    opt_repos       = REPOS,
    finalize        = TRUE,       # collate qual_assessments.rds
    verbose         = "normal",
    propagate_libpaths = PROPAGATE_LIBPATHS,
    mem_watchdog    = FALSE
  ),
  error = function(e) {
    tee("\n  !! val_build() threw:\n     ", conditionMessage(e), "\n")
    e
  }
)
tee("  elapsed: ",
    format(round(difftime(Sys.time(), t0, units = "secs"), 1)), "\n")

# Recover the covr_coverage number. Preferred source is the
# collated `qual_assessments.rds` (finalize=TRUE); fall back to
# the per-package `assessed/<pkg>_<ver>_assess_record.rds` written
# by val_pkg() itself so we still get a number when the collation
# leg silently no-ops. val_build() writes under
# `<out>/R_<ver>/<val_date>/`.
vb_pct <- NA_real_
qa_path <- NA_character_
vb_log_path <- NA_character_
if (!inherits(vb, "error")) {
  dated_dirs <- list.dirs(vb_out, recursive = TRUE)

  # Preferred: collated qual_assessments.rds
  qa_candidates <- file.path(dated_dirs, "qual_assessments.rds")
  qa_candidates <- qa_candidates[file.exists(qa_candidates)]
  if (length(qa_candidates)) {
    qa_path <- qa_candidates[1]
    tee("  qual_assessments.rds: ", qa_path, "\n")
    qa <- tryCatch(readRDS(qa_path), error = function(e) NULL)
    if (!is.null(qa) && PKG %in% names(qa)) {
      pa <- qa[[PKG]]
      cov <- tryCatch(pkg_assessment_covr_pct(pa),
                      error = function(e) NA_real_)
      vb_pct <- as.numeric(cov)
    }
  }

  # Fallback: per-pkg assessed record, in case finalize silently skipped
  if (is.na(vb_pct)) {
    ar_candidates <- unlist(lapply(
      dated_dirs,
      function(d) list.files(
        file.path(d, "assessed"),
        pattern = paste0("^", PKG, "_.*_assess_record\\.rds$"),
        full.names = TRUE
      )
    ))
    ar_candidates <- ar_candidates[file.exists(ar_candidates)]
    if (length(ar_candidates)) {
      tee("  fallback: reading assessed/*_assess_record.rds\n")
      ar <- tryCatch(readRDS(ar_candidates[1]), error = function(e) NULL)
      pa <- if (is.list(ar) && !is.null(ar$assessment)) ar$assessment else ar
      cov <- tryCatch(pkg_assessment_covr_pct(pa),
                      error = function(e) NA_real_)
      vb_pct <- as.numeric(cov)
    }
  }

  # Locate the val_pipeline.log for post-mortem, regardless of outcome.
  log_candidates <- file.path(dated_dirs, "val_pipeline.log")
  log_candidates <- log_candidates[file.exists(log_candidates)]
  if (length(log_candidates)) vb_log_path <- log_candidates[1]
}
tee("  val_build covr_coverage = ", format(vb_pct, digits = 4), "\n")

# Post-mortem: if we couldn't recover a coverage number, dump the
# val_pipeline.log tail so the operator has something to look at
# beyond "1.6s and no output". The log lives in the same
# `<out>/R_<ver>/<val_date>/` tree; we located it above.
if (is.na(vb_pct)) {
  if (!is.na(vb_log_path)) {
    tee("\n  !! No covr_coverage recovered. Tail of ", vb_log_path, ":\n")
    log_lines <- tryCatch(readLines(vb_log_path, warn = FALSE),
                          error = function(e) character(0))
    tail_n <- min(length(log_lines), 60L)
    if (tail_n > 0L) {
      tee(paste0("    | ", tail(log_lines, tail_n), collapse = "\n"), "\n")
    } else {
      tee("    (log file present but empty)\n")
    }
  } else {
    tee("\n  !! No val_pipeline.log found under ", vb_out, ".\n")
    tee("     val_build() likely short-circuited before init_val_log().\n")
    tee("     Common causes: available.packages() returned empty (no\n")
    tee("     network / bad opt_repos), or resolve_pkg_tree() couldn't\n")
    tee("     find `", PKG, "` in the configured repos. Try passing\n")
    tee("     opt_repos = c(CRAN = \"https://cloud.r-project.org\").\n")
  }
}

# Reconciliation. If the val_build number differs materially from
# the two bare covr runs above, the delta pins the issue to the
# val.pipeline overlay (multisession env-var propagation, worker
# libpath inheritance, ...) rather than to the covr harness
# itself.
if (!is.na(vb_pct) && !is.na(loud_pct)) {
  vb_delta <- loud_pct - vb_pct
  tee(sprintf("  Delta vs. loud reference: %+0.2fpp\n", vb_delta))
  if (abs(vb_delta) >= 5) {
    tee("  !! val.pipeline path diverges from the loud reference by >= 5pp.\n")
    tee("     Cross-check the val_pipeline.log in ", vb_out, " for the\n")
    tee("     'Mirrored .libPaths() into R_LIBS_SITE' echo -- its\n")
    tee("     R_LIBS_SITE value is what the worker's covr subprocess\n")
    tee("     actually inherits. See val.pipeline #171.\n")
  }
}


# ---- 6. Summary -----------------------------------------------------------

hdr("Summary")
tee("  Package                 : ", PKG, "\n")
tee("  Source tree             : ", SRC_DIR, "\n")
tee("  Loud (covr default)     : ", format(loud_pct, digits = 4), "%\n")
tee("  riskmetric-style        : ", format(rm_pct,   digits = 4), "%\n")
tee("  val.pipeline path       : ", format(vb_pct,   digits = 4), "%\n")
tee("  Missing Suggests        : ",
    if (length(missing_suggests)) paste(missing_suggests, collapse = ", ")
    else "(none)", "\n")
tee("  pandoc on PATH          : ",
    if (nzchar(pandoc_path)) pandoc_path else "**NOT FOUND**", "\n")
tee("  val_build out dir       : ", vb_out, "\n")
tee("\n  Full log: ", log_path, "\n")

message("\nDone. Log: ", log_path)

# Open the log at the end for convenience. Prefer RStudio's source pane
# if we're in one; fall back to `file.show()` in a plain R session.
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  try(rstudioapi::navigateToFile(log_path), silent = TRUE)
} else {
  try(file.show(log_path, title = basename(log_path)), silent = TRUE)
}
