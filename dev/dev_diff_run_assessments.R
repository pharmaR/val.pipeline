# dev/dev_diff_run_assessments.R
#
# Before/after diff for two val_build() runs on an overlapping set of
# packages. Motivating scenario (2026-09-14): a `pkgType = "both"` fix
# is expected to lift covr_coverage on some packages but may regress
# others -- we want a thorough, sortable per-package delta so the
# regressions can be inspected individually and, ideally, clustered
# into a mechanism.
#
# ---- USAGE ----------------------------------------------------------
#
# Set these in your global env before sourcing:
#
#   old_qa   - data.frame OR path to old qual_assessments RDS.
#   new_qa   - data.frame OR path to new qual_assessments RDS,
#              OR path to a val_dir/ (the script will walk
#              assessed/*_meta.rds and reconstruct a qual_assessments-
#              shaped frame). This last form is what you want if you
#              ran with `finalize = FALSE` and the top-level
#              qual_assessments.rds hasn't been regenerated yet.
#   old_qm   - (optional) data.frame OR path to old qual_metadata RDS.
#              If supplied AND new_qm is too, we also diff
#              `final_decision`.
#   new_qm   - (optional) same shape.
#   pkgs     - (optional) restrict the diff to this subset of packages.
#              If not supplied, we diff the full intersection.
#   out_diff - (optional) path to save the joined diff frame as an
#              RDS. Defaults to
#              `dev/covr_diff_<yyyymmdd_hhmmss>.rds` (gitignored per
#              existing pattern in .gitignore).
#
# ---- OUTPUTS --------------------------------------------------------
#
# Console:
#   1. Cohort sizes (old only / new only / shared).
#   2. Coverage-band shift histogram (old band -> new band).
#   3. Improved / regressed / unchanged counts w/ Delta thresholds.
#   4. Top-20 regressions (largest -delta on covr_coverage).
#   5. Top-20 improvements (largest +delta on covr_coverage).
#   6. Metric column presence diff (any metric column added/dropped
#      between the two runs -- catches config drift silently).
#   7. Decision-category shifts (if qm supplied on both sides).
#
# Globals left in your env:
#   diff_df       - the full per-package joined diff frame.
#   regressed_df  - subset where covr_coverage dropped by >= 5 pp.
#   improved_df   - subset where covr_coverage rose by >= 5 pp.
#
# =====================================================================

# ---- Load helpers ---------------------------------------------------

.read_maybe <- function(x, what) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    return(readRDS(x))
  }
  stop("Cannot resolve `", what, "` -- pass a data.frame or path to an RDS.")
}

.reconstruct_qa_from_assessed <- function(val_dir) {
  assessed <- file.path(val_dir, "assessed")
  if (!dir.exists(assessed)) {
    stop("No assessed/ under: ", val_dir)
  }
  meta_files <- list.files(assessed, pattern = "_meta\\.rds$",
                           full.names = TRUE)
  if (length(meta_files) == 0L) {
    stop("No _meta.rds files under: ", assessed)
  }
  cat("Reconstructing qual_assessments from ", length(meta_files),
      " meta files in ", assessed, " ...\n", sep = "")
  rows <- lapply(meta_files, function(f) {
    m <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(m) || !is.data.frame(m)) return(NULL)
    m
  })
  rows <- Filter(Negate(is.null), rows)
  # dplyr::bind_rows would be nicer but avoid deps; base rbind with
  # fill-NA-by-name is the poor man's version.
  all_cols <- unique(unlist(lapply(rows, names)))
  do.call(rbind, lapply(rows, function(r) {
    miss <- setdiff(all_cols, names(r))
    if (length(miss)) r[miss] <- NA
    r[, all_cols, drop = FALSE]
  }))
}

# ---- Resolve inputs -------------------------------------------------

if (!exists("old_qa")) stop("Set `old_qa` in your global env.")
if (!exists("new_qa")) stop("Set `new_qa` in your global env.")

old_qa_df <- .read_maybe(old_qa, "old_qa")

# `new_qa` might be a val_dir path -- detect by looking for assessed/.
new_qa_df <- if (is.character(new_qa) && length(new_qa) == 1L &&
                 dir.exists(new_qa)) {
  .reconstruct_qa_from_assessed(new_qa)
} else {
  .read_maybe(new_qa, "new_qa")
}

if (!"package" %in% names(old_qa_df) || !"package" %in% names(new_qa_df)) {
  stop("Both frames must have a `package` column.")
}
if (!"covr_coverage" %in% names(old_qa_df) ||
    !"covr_coverage" %in% names(new_qa_df)) {
  stop("Both frames must have a `covr_coverage` column.")
}

old_qm_df <- if (exists("old_qm")) .read_maybe(old_qm, "old_qm") else NULL
new_qm_df <- if (exists("new_qm")) .read_maybe(new_qm, "new_qm") else NULL

# Optional cohort filter
if (exists("pkgs") && length(pkgs)) {
  old_qa_df <- old_qa_df[old_qa_df$package %in% pkgs, , drop = FALSE]
  new_qa_df <- new_qa_df[new_qa_df$package %in% pkgs, , drop = FALSE]
}

# ---- Cohort membership ---------------------------------------------

old_pkgs   <- unique(old_qa_df$package)
new_pkgs   <- unique(new_qa_df$package)
shared     <- intersect(old_pkgs, new_pkgs)
only_old   <- setdiff(old_pkgs, new_pkgs)
only_new   <- setdiff(new_pkgs, old_pkgs)

cat("== Cohort membership ==================================\n")
cat(sprintf("  In old only        : %d\n", length(only_old)))
cat(sprintf("  In new only        : %d\n", length(only_new)))
cat(sprintf("  In both (shared)   : %d\n", length(shared)))
cat("=======================================================\n\n")

# ---- Metric column presence diff -----------------------------------

old_cols <- setdiff(names(old_qa_df), c("package", "version"))
new_cols <- setdiff(names(new_qa_df), c("package", "version"))
dropped_cols <- setdiff(old_cols, new_cols)
added_cols   <- setdiff(new_cols, old_cols)
if (length(dropped_cols) || length(added_cols)) {
  cat("== Metric column presence diff ========================\n")
  if (length(dropped_cols)) {
    cat("  Dropped in new: ", paste(dropped_cols, collapse = ", "), "\n")
  }
  if (length(added_cols)) {
    cat("  Added in new  : ", paste(added_cols, collapse = ", "), "\n")
  }
  cat("=======================================================\n\n")
}

# ---- Per-package joined diff ---------------------------------------

pick_latest <- function(df, key = "package") {
  # Some packages may appear in >1 row if versions changed across runs.
  # Keep the row with the largest `version` per package.
  if (!"version" %in% names(df)) return(df)
  df$.pkgorder <- package_version(df$version, strict = FALSE)
  ord <- order(df$package, df$.pkgorder, decreasing = c(FALSE, TRUE),
               method = "radix")
  df <- df[ord, , drop = FALSE]
  df <- df[!duplicated(df$package), , drop = FALSE]
  df$.pkgorder <- NULL
  df
}

old_flat <- pick_latest(old_qa_df)
new_flat <- pick_latest(new_qa_df)

old_flat <- old_flat[old_flat$package %in% shared, , drop = FALSE]
new_flat <- new_flat[new_flat$package %in% shared, , drop = FALSE]

# Pull the columns we care about; NA-safe numeric coerce for covr.
to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

diff_df <- data.frame(
  package         = old_flat$package,
  version_old     = if ("version" %in% names(old_flat)) old_flat$version else NA,
  version_new     = new_flat$version[match(old_flat$package, new_flat$package)],
  covr_old        = to_num(old_flat$covr_coverage),
  covr_new        = to_num(new_flat$covr_coverage[
    match(old_flat$package, new_flat$package)
  ]),
  stringsAsFactors = FALSE
)
diff_df$version_changed <- !is.na(diff_df$version_old) &
                           !is.na(diff_df$version_new) &
                           diff_df$version_old != diff_df$version_new
diff_df$covr_delta <- diff_df$covr_new - diff_df$covr_old
# Categorize (base R, no dplyr dep):
diff_df$covr_status <- ifelse(
  is.na(diff_df$covr_new) & is.na(diff_df$covr_old), "both_na",
  ifelse(is.na(diff_df$covr_old), "new_only",
  ifelse(is.na(diff_df$covr_new), "lost_metric",
  ifelse(abs(diff_df$covr_delta) < 1e-6, "unchanged",
  ifelse(diff_df$covr_delta > 0, "improved", "regressed"))))
)

# ---- Band shift ----------------------------------------------------

band <- function(x) {
  cut(x, breaks = c(-Inf, 0, 40, 65, 80, 90, 100 + 1e-9),
      labels = c("NA/0", "0-40", "40-65", "65-80", "80-90", "90-100"),
      right = FALSE)
}
diff_df$band_old <- band(diff_df$covr_old)
diff_df$band_new <- band(diff_df$covr_new)

cat("== Coverage-band shift (rows = old band, cols = new band) =====\n")
band_shift <- table(diff_df$band_old, diff_df$band_new,
                    useNA = "ifany", dnn = c("old", "new"))
print(band_shift)
cat("\n")

# ---- Delta summary counts ------------------------------------------

thresh <- 5.0
n_improved_big   <- sum(diff_df$covr_delta >=  thresh, na.rm = TRUE)
n_improved_small <- sum(diff_df$covr_delta >  0 &
                        diff_df$covr_delta <  thresh, na.rm = TRUE)
n_unchanged      <- sum(diff_df$covr_status == "unchanged", na.rm = TRUE)
n_regressed_small<- sum(diff_df$covr_delta <  0 &
                        diff_df$covr_delta > -thresh, na.rm = TRUE)
n_regressed_big  <- sum(diff_df$covr_delta <= -thresh, na.rm = TRUE)
n_new_only       <- sum(diff_df$covr_status == "new_only", na.rm = TRUE)
n_lost           <- sum(diff_df$covr_status == "lost_metric", na.rm = TRUE)

cat("== Covr delta summary (shared pkgs) ===================\n")
cat(sprintf("  Improved  >=  %.0f pp : %d\n", thresh, n_improved_big))
cat(sprintf("  Improved  <   %.0f pp : %d\n", thresh, n_improved_small))
cat(sprintf("  Unchanged             : %d\n", n_unchanged))
cat(sprintf("  Regressed <   %.0f pp : %d\n", thresh, n_regressed_small))
cat(sprintf("  Regressed >=  %.0f pp : %d\n", thresh, n_regressed_big))
cat(sprintf("  Gained metric (NA->#) : %d\n", n_new_only))
cat(sprintf("  Lost metric   (#->NA) : %d\n", n_lost))
cat("=======================================================\n\n")

# ---- Top regressions / improvements --------------------------------

.disp <- function(df) {
  df <- df[, c("package", "version_old", "version_new",
               "covr_old", "covr_new", "covr_delta",
               "version_changed"), drop = FALSE]
  df$covr_old   <- round(df$covr_old,   2)
  df$covr_new   <- round(df$covr_new,   2)
  df$covr_delta <- round(df$covr_delta, 2)
  df
}

regressed_df <- diff_df[!is.na(diff_df$covr_delta) &
                        diff_df$covr_delta <= -thresh, , drop = FALSE]
regressed_df <- regressed_df[order(regressed_df$covr_delta), , drop = FALSE]

improved_df <- diff_df[!is.na(diff_df$covr_delta) &
                       diff_df$covr_delta >= thresh, , drop = FALSE]
improved_df <- improved_df[order(-improved_df$covr_delta), , drop = FALSE]

cat("== Top 20 regressions (largest -delta) ================\n")
if (nrow(regressed_df) == 0L) {
  cat("  (none)\n")
} else {
  print(.disp(utils::head(regressed_df, 20)), row.names = FALSE)
}
cat("\n")

cat("== Top 20 improvements (largest +delta) ===============\n")
if (nrow(improved_df) == 0L) {
  cat("  (none)\n")
} else {
  print(.disp(utils::head(improved_df, 20)), row.names = FALSE)
}
cat("\n")

# ---- Optional: final_decision shifts -------------------------------

if (!is.null(old_qm_df) && !is.null(new_qm_df) &&
    "final_decision" %in% names(old_qm_df) &&
    "final_decision" %in% names(new_qm_df)) {
  om <- old_qm_df[, c("package", "final_decision"), drop = FALSE]
  nm <- new_qm_df[, c("package", "final_decision"), drop = FALSE]
  names(om)[2] <- "decision_old"
  names(nm)[2] <- "decision_new"
  dec <- merge(om, nm, by = "package", all = FALSE)
  dec <- dec[dec$package %in% shared, , drop = FALSE]

  cat("== Decision-category shift (old -> new) ===============\n")
  dec_tab <- table(dec$decision_old, dec$decision_new,
                   useNA = "ifany", dnn = c("old", "new"))
  print(dec_tab)
  cat("=======================================================\n\n")

  # Merge into diff_df for saved output
  diff_df <- merge(diff_df, dec, by = "package", all.x = TRUE)
}

# ---- Persist diff frame --------------------------------------------

if (!exists("out_diff")) {
  out_diff <- file.path(
    "dev",
    paste0("covr_diff_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  )
}
saveRDS(diff_df, out_diff)
cat("Saved full diff frame to: ", out_diff, "\n", sep = "")
cat("  ", nrow(diff_df), " rows x ", ncol(diff_df),
    " cols. Globals: diff_df, regressed_df, improved_df.\n", sep = "")

invisible(diff_df)
