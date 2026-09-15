# dev/dev_probe_run_progress.R
#
# Mid-run progress probe for a val_build() / val_pipeline() invocation.
#
# Motivating scenario: you kicked off a val_build(replace = TRUE, ...)
# rerun on ~N packages a few hours ago and want to know how many are
# done, how many are left, and roughly when it'll finish. Because
# `replace = TRUE` overwrites the assessed/<pkg>_<ver>_meta.rds file
# in place, you CAN'T just count files -- pre-existing metas from a
# prior run look identical on disk. This script filters by file
# mtime instead: any meta touched at or after your run kickoff
# counts as "done in this run".
#
# ---- USAGE ----------------------------------------------------------
#
# Two knobs the script needs; supply them either as objects already in
# your global env before sourcing, or edit the defaults below:
#
#   pkgs         - character vector, the pkg_names you passed to val_build().
#   run_started  - POSIXct or a "YYYY-MM-DD HH:MM:SS" string, the moment
#                  the run started. If you kicked it off with a launcher
#                  that echoed a start-time to the log, use that.
#                  Fallback: `Sys.time() - as.difftime(hrs, units = 'hours')`
#                  with `hrs` matching your best estimate.
#
# Layout knobs (typically inherited from your launcher's env):
#
#   out          - top-level output tree, e.g. "/data/pm/riskassessments".
#   val_date     - Date or "YYYY-MM-DD" string that val_build() used to
#                  compute its `val_dir` (folder is `R_<Rver>/<YYYYMMDD>/`).
#
# ---- WHAT YOU GET ---------------------------------------------------
#
# 1. Console summary: cohort size, done, remaining, throughput,
#    linear ETA to finish.
# 2. `progress` data.frame in your global env with per-pkg status
#    (done / no_meta / pre_existing_meta_not_touched).
# 3. `remaining_pkgs` vector -- the still-to-run subset.
#
# Note on caveats:
# - A pkg that errored out early enough to skip the meta write shows as
#   `no_meta`. Cross-check `val_dir/val_pipeline.log` for skip/error
#   lines to tell "crashed" from "queued".
# - If the run has cycled workers between packages faster than mtime
#   resolution (unlikely at real-world assessment times), adjacent
#   pkgs may share the same second. Not a problem for cohort-level
#   counting, just don't over-index on ordering by mtime.
#
# =====================================================================

# ---- Defaults / expected inputs -------------------------------------

if (!exists("pkgs")) {
  stop(
    "Set `pkgs` in your global env (the vector you passed to val_build) ",
    "before sourcing this script."
  )
}

if (!exists("run_started")) {
  stop(
    "Set `run_started` in your global env, e.g.\n",
    "  run_started <- as.POSIXct('2026-09-14 12:00:00')\n",
    "or, if you only know duration:\n",
    "  run_started <- Sys.time() - as.difftime(6.5, units = 'hours')"
  )
}
if (is.character(run_started)) run_started <- as.POSIXct(run_started)

if (!exists("out")) {
  stop("Set `out` (top-level output tree) in your global env.")
}
if (!exists("val_date")) {
  stop("Set `val_date` (the run's val_date) in your global env.")
}

# ---- Resolve val_dir ------------------------------------------------

val_date_txt <- gsub("-", "", format(as.Date(val_date)))
val_dir      <- file.path(out, paste0("R_", getRversion()), val_date_txt)
assessed     <- file.path(val_dir, "assessed")

if (!dir.exists(assessed)) {
  stop(
    "assessed/ dir does not exist: ", assessed, "\n",
    "Check `out` and `val_date` -- did the run actually kick off?"
  )
}

cat("Probing:\n")
cat("  val_dir     : ", val_dir, "\n", sep = "")
cat("  run_started : ", format(run_started), "\n", sep = "")
cat("  now         : ", format(Sys.time()), "\n", sep = "")
elapsed_hr <- as.numeric(difftime(Sys.time(), run_started, units = "hours"))
cat("  elapsed     : ", sprintf("%.2f hr", elapsed_hr), "\n\n", sep = "")

# ---- Classify each cohort pkg by meta-file mtime --------------------

# Build one row per cohort pkg. `meta_path` may be missing (no meta yet
# on disk) or present-but-stale (pre-existing from a prior run). We
# glob rather than reconstruct <pkg>_<ver>_meta.rds because we don't
# necessarily know the version pinned for this run's opt_repos here.
progress <- do.call(rbind, lapply(pkgs, function(pkg) {
  candidates <- list.files(
    assessed,
    pattern = paste0("^", gsub("([.^$|()\\\\+*?{}])", "\\\\\\1", pkg),
                     "_[^_]+_meta\\.rds$"),
    full.names = TRUE
  )
  if (length(candidates) == 0L) {
    return(data.frame(
      package        = pkg,
      status         = "no_meta",
      meta_path      = NA_character_,
      meta_mtime     = as.POSIXct(NA),
      stringsAsFactors = FALSE
    ))
  }
  # Take the newest matching meta -- if a prior run and this run
  # produced different versions, we want the freshest.
  info    <- file.info(candidates)
  newest  <- rownames(info)[which.max(info$mtime)]
  mt      <- info[newest, "mtime"]
  status  <- if (mt >= run_started) "done" else "pre_existing_meta_not_touched"
  data.frame(
    package    = pkg,
    status     = status,
    meta_path  = newest,
    meta_mtime = mt,
    stringsAsFactors = FALSE
  )
}))

# ---- Summary --------------------------------------------------------

n_total     <- nrow(progress)
n_done      <- sum(progress$status == "done")
n_pre       <- sum(progress$status == "pre_existing_meta_not_touched")
n_no_meta   <- sum(progress$status == "no_meta")
n_remaining <- n_total - n_done   # anything not touched this run

throughput  <- n_done / max(elapsed_hr, 1e-6)   # pkgs / hr
eta_hr      <- if (throughput > 0) n_remaining / throughput else NA_real_

cat("== Cohort progress =====================================\n")
cat(sprintf("  Total in cohort              : %d\n",  n_total))
cat(sprintf("  Done this run (touched)      : %d\n",  n_done))
cat(sprintf("  Pre-existing meta, not touched: %d\n", n_pre))
cat(sprintf("  No meta on disk at all       : %d\n",  n_no_meta))
cat(sprintf("  Remaining to run             : %d\n",  n_remaining))
cat("\n")
cat(sprintf("  Throughput                   : %.2f pkgs/hr\n", throughput))
if (!is.na(eta_hr)) {
  eta_when <- Sys.time() + as.difftime(eta_hr, units = "hours")
  cat(sprintf("  Linear ETA to finish         : %.2f hr (%s)\n",
              eta_hr, format(eta_when, "%Y-%m-%d %H:%M %Z")))
}
cat("========================================================\n")

# ---- Expose vectors for follow-up -----------------------------------

remaining_pkgs <- progress$package[progress$status != "done"]

# 20 most-recently-touched, so you can see "what's the worker on now?"
if (n_done > 0L) {
  cat("\nMost recent 20 completions this run:\n")
  done_sub <- progress[progress$status == "done", ]
  done_sub <- done_sub[order(done_sub$meta_mtime, decreasing = TRUE), ]
  print(utils::head(
    data.frame(
      package = done_sub$package,
      mtime   = format(done_sub$meta_mtime, "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    ),
    20
  ), row.names = FALSE)
}

if (length(remaining_pkgs) > 0L) {
  cat("\nFirst 20 remaining:\n")
  print(utils::head(remaining_pkgs, 20))
}

invisible(progress)
