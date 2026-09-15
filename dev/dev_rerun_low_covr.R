# dev/dev_rerun_low_covr.R
#
# Surgical re-run of the low-covr cohort under the pkgType fix
# (pharmaR/val.pipeline#182). Reads dev/low_covr.rds (the
# qual_metadata + qual_assessments merge for pkgs whose covr_coverage
# came in below the acceptance threshold on the affected run) and
# drives `val_build()` against a filtered high-yield / low-cost
# subset with `replace = TRUE`.
#
# `dev/low_covr.rds` is gitignored (contains internal package
# accounting) -- do NOT commit any downstream artifact derived from
# it either.
#
# USAGE
# -----
# Source your usual launcher first so `opt_repos`, `out`,
# `metric_pkg`, `ref`, `val_date`, `deps`, `deps_recursive`, and any
# workbench-specific env plumbing are set. Then:
#
#   source("dev/dev_rerun_low_covr.R")
#
# Adjust the FILTER CONFIG block below if you want a wider or
# narrower subset. Defaults target the 60-65% band + anything above
# 40% coverage that runs in under 15 min -- the "highest-yield,
# lowest-cost" slice.
#
# Every pkg in the resulting set has `replace = TRUE` in the
# val_build call, so its existing `_meta.rds` / `_assess_record.rds`
# / `_assessments.rds` bundle under `<val_dir>/assessed/` gets
# overwritten in place. Adjust `VAL_DATE_OVERRIDE` if you want to
# land the re-run in a different dated folder than your launcher's
# `val_date`.

# =============================================================================
# FILTER CONFIG -- edit here if you want a different slice
# =============================================================================

MIN_COVR   <- 40    # keep pkgs with existing covr_coverage strictly above this
MAX_RT_MIN <- 15    # ...AND whose prior assessment_runtime_mins is below this
# Optional hard exclusions -- pkgs you already know are legitimately
# under-tested and won't cross threshold no matter what.
EXCLUDE_PKGS <- character(0)   # e.g. c("crs", "refund", "lavaan")

VAL_DATE_OVERRIDE <- NULL   # set to as.Date("2026-09-09") etc. to override

# Optional: cap the set for a first-cut sanity run. Set to NULL to
# drop the cap.
MAX_PKGS <- NULL   # e.g. 20 for a quick pilot

# =============================================================================
# Load + filter
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

lc_path <- "dev/low_covr.rds"
if (!file.exists(lc_path)) {
  stop("Can't find ", lc_path, ". Run this from the val.pipeline repo root.")
}
lc <- readRDS(lc_path)
lc$rt_min <- as.numeric(lc$assessment_runtime_mins)

cohort <- lc |>
  filter(covr_coverage > MIN_COVR,
         rt_min <= MAX_RT_MIN,
         !package %in% EXCLUDE_PKGS)

if (!is.null(MAX_PKGS) && nrow(cohort) > MAX_PKGS) {
  # Prefer the pkgs closest to the 65% threshold first -- best odds of
  # crossing it, best signal that the fix worked.
  cohort <- cohort |>
    arrange(desc(covr_coverage)) |>
    slice_head(n = MAX_PKGS)
}

cat("\n=====================================================\n")
cat("Low-covr re-run cohort selection\n")
cat("=====================================================\n")
cat("Original set (below-threshold pkgs):    ", nrow(lc), "\n")
cat("Filter: covr_coverage > ", MIN_COVR,
    " AND runtime <= ", MAX_RT_MIN, " min",
    if (length(EXCLUDE_PKGS)) paste0(" AND !exclude(", length(EXCLUDE_PKGS), " pkgs)") else "",
    if (!is.null(MAX_PKGS)) paste0(" AND top-", MAX_PKGS, " by covr desc") else "",
    "\n", sep = "")
cat("Selected cohort:                        ", nrow(cohort), " pkgs\n\n")

cat("Coverage band breakdown of selected cohort:\n")
brks <- c(MIN_COVR, 50, 55, 60, 65)
cohort |>
  mutate(band = cut(covr_coverage, breaks = brks, include.lowest = FALSE,
                    right = TRUE)) |>
  count(band) |>
  print()

serial_min <- sum(cohort$rt_min, na.rm = TRUE)
cat("\nExpected serial time (sum of prior runtimes): ",
    round(serial_min, 1), " min = ", round(serial_min / 60, 1), " hr\n")

for (w in c(1, 3, 5, 8, 10)) {
  naive_hr <- serial_min / w / 60
  cushioned_hr <- naive_hr * 1.2
  floor_hr <- max(cohort$rt_min, na.rm = TRUE) / 60
  cat(sprintf("  workers=%2d -> naive %.1f hr, w/ 20%% cushion %.1f hr (floor: %.1f hr)\n",
              w, naive_hr, cushioned_hr, floor_hr))
}

# =============================================================================
# Guardrail: confirm we're on a fix-branch val.pipeline
# =============================================================================
vp_ver <- as.character(utils::packageVersion("val.pipeline"))
cat("\nval.pipeline version loaded: ", vp_ver, "\n")
cat("val.pipeline install path  : ", find.package("val.pipeline"), "\n")
if (utils::compareVersion(vp_ver, "0.1.60") < 0) {
  warning("val.pipeline version < 0.1.60. The pkgType fix from #182 is not ",
          "loaded; this re-run will reproduce the same bad numbers. ",
          "Install the ac-181-gate-pkgtype-on-ref branch first.",
          call. = FALSE, immediate. = TRUE)
}

# =============================================================================
# Pre-flight: verify launcher globals are in scope
# =============================================================================
required <- c("opt_repos", "out", "metric_pkg", "ref",
              "deps", "deps_recursive")
missing_vars <- required[!vapply(required, exists, logical(1))]
if (length(missing_vars)) {
  stop("Launcher globals not set: ", paste(missing_vars, collapse = ", "),
       ". Source your launcher first, then re-run this script.")
}
val_date <- if (!is.null(VAL_DATE_OVERRIDE)) VAL_DATE_OVERRIDE else {
  if (exists("val_date")) get("val_date") else Sys.Date()
}
workers <- if (exists("workers")) get("workers") else 5L
config_path <- if (exists("config_path")) get("config_path") else NULL

cat("\nRe-run settings:\n")
cat("  val_date         : ", format(val_date), "\n")
cat("  workers          : ", workers, "\n")
cat("  ref              : ", ref, "\n")
cat("  out              : ", out, "\n")
cat("  config_path      : ",
    if (is.null(config_path)) "<val_build default>" else config_path, "\n\n")

# =============================================================================
# GO / NO-GO prompt (interactive sessions only)
# =============================================================================
if (interactive()) {
  ans <- readline("Proceed with val_build() on the selected cohort? [y/N] ")
  if (!tolower(trimws(ans)) %in% c("y", "yes")) {
    message("Aborted by user.")
    return(invisible(NULL))
  }
}

# =============================================================================
# Drive val_build()
# =============================================================================
# Deliberately NOT passing `prep = ...` -- this is a targeted rebuild of a
# hand-picked pkg set, not a full-universe run. `deps = NULL, deps_recursive =
# NULL` matches the pattern in your launcher's "Re-run & re-finalize" block:
# each pkg is assessed on its own without pulling its dep tree back into the
# candidate set (the dep tree is already installed via rv on your setup).
#
# `finalize = FALSE` because report rendering + qualified-list emission are
# val_pipeline()-scope work you'll do separately in a live session once
# happy with the numbers.

qual_rerun <- val.pipeline::val_build(
  pkg_names          = cohort$package,
  ref                = ref,
  metric_pkg         = metric_pkg,
  deps               = NULL,
  deps_recursive     = NULL,
  rev_deps           = NULL,
  rev_deps_recursive = NULL,
  val_date           = val_date,
  out                = out,
  opt_repos          = opt_repos,
  replace            = TRUE,
  workers            = workers,
  config_path        = config_path,
  prep               = NULL,
  propagate_libpaths = TRUE,
  mem_watchdog       = TRUE,
  finalize           = FALSE,
  verbose            = "normal"
)

cat("\n=====================================================\n")
cat("Re-run complete. Comparing old vs new covr_coverage.\n")
cat("=====================================================\n")

# =============================================================================
# Post-run compare: quick before / after table
# =============================================================================
val_date_txt <- gsub("-", "", format(as.Date(val_date)))
val_dir <- file.path(out, paste0("R_", getRversion()), val_date_txt)
assessed <- file.path(val_dir, "assessed")

diff_rows <- lapply(seq_len(nrow(cohort)), function(i) {
  pkg <- cohort$package[i]
  ver <- cohort$version[i]
  meta_path <- file.path(assessed, paste0(pkg, "_", ver, "_meta.rds"))
  if (!file.exists(meta_path)) {
    return(data.frame(package = pkg, version = ver,
                      covr_before = cohort$covr_coverage[i],
                      covr_after  = NA_real_,
                      delta       = NA_real_,
                      stringsAsFactors = FALSE))
  }
  m <- tryCatch(readRDS(meta_path), error = function(e) NULL)
  covr_after <- if (!is.null(m) && "covr_coverage" %in% names(m)) {
    as.numeric(m$covr_coverage)
  } else NA_real_
  data.frame(package = pkg, version = ver,
             covr_before = cohort$covr_coverage[i],
             covr_after  = covr_after,
             delta       = covr_after - cohort$covr_coverage[i],
             stringsAsFactors = FALSE)
})
compare_df <- do.call(rbind, diff_rows) |>
  arrange(desc(delta))

cat("Delta summary (nrow = ", nrow(compare_df), "):\n", sep = "")
print(summary(compare_df$delta))

cat("\nCrossed the 65% threshold: ",
    sum(compare_df$covr_after > 65, na.rm = TRUE),
    " / ", nrow(compare_df), "\n", sep = "")

cat("\nTop 20 improvements:\n")
compare_df |> head(20) |> print(row.names = FALSE)

cat("\nRegressions (delta < 0), if any:\n")
regressions <- compare_df |> filter(delta < 0)
if (nrow(regressions) == 0L) {
  cat("(none)\n")
} else {
  print(regressions, row.names = FALSE)
}

# Persist the compare df alongside the run for later review. Kept in
# dev/ (gitignored via low_covr.rds sibling entry -- add an
# equivalent line if you want to preserve this file across pulls).
out_path <- file.path("dev", paste0("low_covr_rerun_compare_",
                                    val_date_txt, ".rds"))
saveRDS(compare_df, out_path)
cat("\nSaved before/after compare table to: ", out_path, "\n")
