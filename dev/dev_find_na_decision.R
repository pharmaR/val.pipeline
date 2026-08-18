# dev/dev_find_na_decision.R
#
# Fast triage for the
#   Error in if (!is_dep_skip && pkg_meta$decision != decisions[1]) { :
#     missing value where TRUE/FALSE needed
# blowup at the tail of a serial-mode val_build() rerun with
# replace = FALSE. Root cause is a cached _meta.rds bundle whose
# `decision` field is NA_character_, usually produced by a prior run
# where val_decision() couldn't categorize the pkg (empty viable-metric
# set, malformed assessment, etc.).
#
# Point `val_dir` at the run directory (the one containing
# `assessed/`, `qual_metadata.rds` etc.) and source. Prints:
#   * how many _meta.rds bundles are on disk
#   * which ones have NA / NULL decision (the offenders)
#   * a summary of what other key fields look like on those bundles,
#     to help decide whether to delete + re-assess or hand-patch.
#
# No writes. Safe to re-run.

val_dir <- "/data/shared/riskassessments/R_4.5.2/20260813"  # <-- adjust

# ---- 1. Load every _meta.rds and record decision / final_decision. ----
assessed <- file.path(val_dir, "assessed")
stopifnot("assessed/ dir not found under val_dir" = dir.exists(assessed))

files <- list.files(assessed, pattern = "_meta\\.rds$", full.names = TRUE)
cat("Found", length(files), "_meta.rds file(s) under", assessed, "\n\n")

status <- lapply(files, function(f) {
  m <- tryCatch(readRDS(f), error = function(e) list(.err = conditionMessage(e)))
  list(
    file            = basename(f),
    unreadable      = isTRUE(!is.null(m$.err)),
    err_msg         = m$.err %||% NA_character_,
    has_decision    = !is.null(m$decision),
    decision        = if (is.null(m$decision)) NA_character_
                      else as.character(m$decision)[1],
    final_decision  = if (is.null(m$final_decision)) NA_character_
                      else as.character(m$final_decision)[1],
    decision_reason = if (is.null(m$decision_reason)) NA_character_
                      else as.character(m$decision_reason)[1],
    pkg             = m$pkg %||% NA_character_,
    ver             = m$ver %||% NA_character_
  )
})
`%||%` <- function(x, y) if (is.null(x)) y else x
df <- do.call(rbind.data.frame, lapply(status, as.data.frame,
                                       stringsAsFactors = FALSE))

# ---- 2. The offenders. ----
bad <- df$unreadable | !df$has_decision | is.na(df$decision)
cat("Bundles with NA / NULL / unreadable decision:", sum(bad), "\n\n")

if (any(bad)) {
  print(df[bad, c("file", "pkg", "ver", "decision", "final_decision",
                  "decision_reason", "unreadable", "err_msg")],
        row.names = FALSE)
} else {
  cat("None found. If val_build() still blows up on the comparison,\n",
      "the NA must be arriving from decisions[1] instead of pkg_meta$decision.\n",
      "Check `pull_config(val = 'decisions_lst', rule_type = 'default')`\n",
      "returns a character vector without NA.\n", sep = "")
}

# ---- 3. Options once you have the offender list. ----
#
# A. Delete offending _meta.rds file(s) so the next `val_pipeline(replace = FALSE)`
#    re-assesses just those pkgs:
#
#      to_delete <- file.path(assessed, df$file[bad])
#      # inspect first
#      to_delete
#      # then, only when you're sure:
#      # file.remove(to_delete)
#
# B. Hand-patch the decision in-place (e.g. mark as High so the run finishes;
#    reject_iteration() in val_finalize() will still propagate to dependents).
#    Only do this when you know the pkg genuinely couldn't be assessed:
#
#      for (f in file.path(assessed, df$file[bad])) {
#        m <- readRDS(f)
#        m$decision              <- "High"
#        m$decision_reason       <- m$decision_reason %||% "Error"
#        m$decision_reason_note  <- m$decision_reason_note %||%
#          "Manually patched from NA decision (see dev_find_na_decision.R)."
#        # leave final_* fields alone; val_finalize()/reject_iteration()
#        # will fill them in.
#        saveRDS(m, f)
#      }
