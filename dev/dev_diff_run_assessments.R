# dev/dev_diff_run_assessments.R
#
# Before/after diff for two val_build() runs on an overlapping set of
# packages. Motivating scenario (2026-09-14): the #181 `pkgType='both'`
# fix is expected to lift covr_coverage on some packages but may
# regress others -- we want:
#
#   1. Console summary (band-shift matrix, delta buckets, top-20 lists).
#   2. Self-contained HTML report to share with colleagues.
#   3. Two rev-dep-weighted views on top of the raw delta lists:
#      - "Unlockers": pkgs that CROSSED 65% upward, ranked by rev-dep
#        count -- a pkg with many rev-deps that just qualified unlocks
#        the biggest downstream slice of the qualified set.
#      - "Blockers": pkgs still BELOW 65% (either regressed or never
#        crossed), ranked by rev-dep count -- these are the most
#        valuable investigation targets because unblocking them
#        propagates the most.
#
# ---- USAGE ----------------------------------------------------------
#
# Set these in your global env before sourcing:
#
#   old_qa  - data.frame OR path to old qual_assessments RDS.
#   new_qa  - data.frame OR path to new qual_assessments RDS,
#             OR path to a val_dir/ (script walks
#             assessed/*_meta.rds and reconstructs a
#             qual_assessments-shaped frame -- important for runs
#             kicked off with `finalize = FALSE`).
#   old_qm  - (optional) data.frame OR path to old qual_metadata RDS.
#   new_qm  - (optional) same shape, for decision-category diff.
#   pkgs    - (optional) restrict diff to this cohort.
#   out_dir - (optional) output directory for the HTML + RDS.
#             Defaults to `dev/`.
#   run_tag - (optional) short string appended to output filenames.
#             Defaults to a timestamp.
#
# Rev-dep source (in order):
#   1. `reverse_dependencies` numeric column on new_qa (preferred --
#      it's what val.pipeline itself uses for scheduling and it
#      reflects the run's own opt_repos snapshot).
#   2. Same column on old_qa if new_qa doesn't carry it.
#   3. Otherwise, computed on the fly against the union universe by
#      counting how many pkgs in qual_assessments list each pkg in
#      their Depends / Imports / LinkingTo (requires those columns
#      on qm; skipped w/ warning if not present).
#
# ---- OUTPUTS --------------------------------------------------------
#
# Console: cohort membership, column-presence diff, band-shift matrix,
# delta bucket counts, top-20 regressions, top-20 improvements,
# top-20 unlockers, top-20 blockers.
#
# Files:
#   <out_dir>/covr_diff_<run_tag>.html   - self-contained report.
#   <out_dir>/covr_diff_<run_tag>.rds    - full diff frame.
#
# Globals: diff_df, regressed_df, improved_df, unlockers_df, blockers_df.
#
# =====================================================================

# ---- Load helpers ---------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

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

new_qa_df <- if (is.character(new_qa) && length(new_qa) == 1L &&
                 dir.exists(new_qa)) {
  .reconstruct_qa_from_assessed(new_qa)
} else {
  .read_maybe(new_qa, "new_qa")
}

if (!"package" %in% names(old_qa_df) || !"package" %in% names(new_qa_df)) {
  stop("Both qa frames must have a `package` column.")
}
if (!"covr_coverage" %in% names(old_qa_df) ||
    !"covr_coverage" %in% names(new_qa_df)) {
  stop("Both qa frames must have a `covr_coverage` column.")
}

old_qm_df <- if (exists("old_qm")) .read_maybe(old_qm, "old_qm") else NULL
new_qm_df <- if (exists("new_qm")) .read_maybe(new_qm, "new_qm") else NULL

if (exists("pkgs") && length(pkgs)) {
  old_qa_df <- old_qa_df[old_qa_df$package %in% pkgs, , drop = FALSE]
  new_qa_df <- new_qa_df[new_qa_df$package %in% pkgs, , drop = FALSE]
}

if (!exists("out_dir")) out_dir <- "dev"
if (!exists("run_tag")) run_tag <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

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
  if (length(dropped_cols)) cat("  Dropped in new: ",
                                paste(dropped_cols, collapse = ", "), "\n")
  if (length(added_cols))   cat("  Added in new  : ",
                                paste(added_cols, collapse = ", "), "\n")
  cat("=======================================================\n\n")
}

# ---- Per-package joined diff ---------------------------------------

pick_latest <- function(df) {
  if (!"version" %in% names(df)) return(df)
  df$.pkgorder <- package_version(df$version, strict = FALSE)
  ord <- order(df$package, df$.pkgorder,
               decreasing = c(FALSE, TRUE), method = "radix")
  df <- df[ord, , drop = FALSE]
  df <- df[!duplicated(df$package), , drop = FALSE]
  df$.pkgorder <- NULL
  df
}

old_flat <- pick_latest(old_qa_df)
new_flat <- pick_latest(new_qa_df)

old_flat <- old_flat[old_flat$package %in% shared, , drop = FALSE]
new_flat <- new_flat[new_flat$package %in% shared, , drop = FALSE]

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Resolve rev-dep count. Preference: new_qa$reverse_dependencies ->
# old_qa$reverse_dependencies -> qm-derived from Depends/Imports.
rev_dep_from_col <- function(df, key = "package") {
  if (!"reverse_dependencies" %in% names(df)) return(NULL)
  x <- to_num(df$reverse_dependencies)
  setNames(x, df[[key]])
}
rd_map <- rev_dep_from_col(new_flat)
if (is.null(rd_map)) rd_map <- rev_dep_from_col(old_flat)
if (is.null(rd_map)) {
  # Fallback: derive from qm Depends/Imports/LinkingTo list-cols or
  # comma-separated strings. This is coarser (union universe, not
  # opt_repos-scoped) but usable.
  qm_src <- new_qm_df %||% old_qm_df
  if (!is.null(qm_src) &&
      any(c("depends", "imports", "linking_to", "linkingto")
          %in% tolower(names(qm_src)))) {
    coalesce_deps <- function(row) {
      pieces <- c()
      for (col in intersect(c("depends", "imports",
                              "linking_to", "linkingto"),
                            tolower(names(qm_src)))) {
        v <- row[[names(qm_src)[tolower(names(qm_src)) == col][1]]]
        if (is.list(v)) v <- unlist(v)
        if (is.character(v))
          pieces <- c(pieces,
                      trimws(unlist(strsplit(v, "[,;]"))))
      }
      unique(pieces[nzchar(pieces) & pieces != "R"])
    }
    counts <- integer(nrow(qm_src))
    all_deps <- lapply(seq_len(nrow(qm_src)),
                       function(i) coalesce_deps(qm_src[i, , drop = FALSE]))
    dep_tbl <- table(unlist(all_deps))
    rd_map <- as.integer(dep_tbl)
    names(rd_map) <- names(dep_tbl)
    cat("Note: rev_deps derived from qm Depends/Imports fallback ",
        "(coarser than opt_repos-scoped counts).\n", sep = "")
  }
}
if (is.null(rd_map)) {
  cat("Warning: no rev-dep source available; unlocker/blocker ",
      "tables will show n_rev_deps = NA.\n", sep = "")
  rd_map <- setNames(rep(NA_integer_, length(shared)), shared)
}

diff_df <- data.frame(
  package         = old_flat$package,
  version_old     = if ("version" %in% names(old_flat)) old_flat$version else NA,
  version_new     = new_flat$version[match(old_flat$package, new_flat$package)],
  covr_old        = to_num(old_flat$covr_coverage),
  covr_new        = to_num(new_flat$covr_coverage[
    match(old_flat$package, new_flat$package)
  ]),
  n_rev_deps      = as.integer(rd_map[old_flat$package]),
  stringsAsFactors = FALSE
)
diff_df$version_changed <- !is.na(diff_df$version_old) &
                           !is.na(diff_df$version_new) &
                           diff_df$version_old != diff_df$version_new
diff_df$covr_delta <- diff_df$covr_new - diff_df$covr_old
diff_df$covr_status <- ifelse(
  is.na(diff_df$covr_new) & is.na(diff_df$covr_old), "both_na",
  ifelse(is.na(diff_df$covr_old), "new_only",
  ifelse(is.na(diff_df$covr_new), "lost_metric",
  ifelse(abs(diff_df$covr_delta) < 1e-6, "unchanged",
  ifelse(diff_df$covr_delta > 0, "improved", "regressed"))))
)

# 65% threshold crossings
THR <- 65
diff_df$crossed_up   <- !is.na(diff_df$covr_old) & !is.na(diff_df$covr_new) &
                       diff_df$covr_old <  THR & diff_df$covr_new >= THR
diff_df$crossed_down <- !is.na(diff_df$covr_old) & !is.na(diff_df$covr_new) &
                       diff_df$covr_old >= THR & diff_df$covr_new <  THR
diff_df$below_after  <- !is.na(diff_df$covr_new) & diff_df$covr_new < THR

# ---- Band shift ----------------------------------------------------

band <- function(x) {
  cut(x, breaks = c(-Inf, 0, 40, 65, 80, 90, 100 + 1e-9),
      labels = c("NA/0", "0-40", "40-65", "65-80", "80-90", "90-100"),
      right = FALSE)
}
diff_df$band_old <- band(diff_df$covr_old)
diff_df$band_new <- band(diff_df$covr_new)

cat("== Coverage-band shift (rows = old, cols = new) =======\n")
band_shift <- table(diff_df$band_old, diff_df$band_new,
                    useNA = "ifany", dnn = c("old", "new"))
print(band_shift)
cat("\n")

# ---- Delta summary counts ------------------------------------------

thresh <- 5.0
counts <- c(
  improved_big    = sum(diff_df$covr_delta >=  thresh, na.rm = TRUE),
  improved_small  = sum(diff_df$covr_delta >   0 &
                        diff_df$covr_delta <   thresh, na.rm = TRUE),
  unchanged       = sum(diff_df$covr_status == "unchanged", na.rm = TRUE),
  regressed_small = sum(diff_df$covr_delta <   0 &
                        diff_df$covr_delta > -thresh, na.rm = TRUE),
  regressed_big   = sum(diff_df$covr_delta <= -thresh, na.rm = TRUE),
  crossed_up      = sum(diff_df$crossed_up,   na.rm = TRUE),
  crossed_down    = sum(diff_df$crossed_down, na.rm = TRUE),
  new_only        = sum(diff_df$covr_status == "new_only",    na.rm = TRUE),
  lost_metric     = sum(diff_df$covr_status == "lost_metric", na.rm = TRUE)
)

cat("== Covr delta summary (shared pkgs) ===================\n")
cat(sprintf("  Improved  >=  %.0f pp : %d\n", thresh, counts["improved_big"]))
cat(sprintf("  Improved  <   %.0f pp : %d\n", thresh, counts["improved_small"]))
cat(sprintf("  Unchanged             : %d\n", counts["unchanged"]))
cat(sprintf("  Regressed <   %.0f pp : %d\n", thresh, counts["regressed_small"]))
cat(sprintf("  Regressed >=  %.0f pp : %d\n", thresh, counts["regressed_big"]))
cat(sprintf("  Crossed UP over %d%% : %d\n", THR, counts["crossed_up"]))
cat(sprintf("  Crossed DOWN under %d%%: %d\n", THR, counts["crossed_down"]))
cat(sprintf("  Gained metric (NA->#) : %d\n", counts["new_only"]))
cat(sprintf("  Lost metric   (#->NA) : %d\n", counts["lost_metric"]))
cat("=======================================================\n\n")

# ---- Top tables ----------------------------------------------------

disp_cols <- c("package", "version_old", "version_new",
               "covr_old", "covr_new", "covr_delta",
               "n_rev_deps", "version_changed")
.round_disp <- function(df) {
  df <- df[, intersect(disp_cols, names(df)), drop = FALSE]
  for (nm in c("covr_old", "covr_new", "covr_delta")) {
    if (nm %in% names(df)) df[[nm]] <- round(df[[nm]], 2)
  }
  df
}

regressed_df <- diff_df[!is.na(diff_df$covr_delta) &
                        diff_df$covr_delta <= -thresh, , drop = FALSE]
regressed_df <- regressed_df[order(regressed_df$covr_delta), , drop = FALSE]

improved_df <- diff_df[!is.na(diff_df$covr_delta) &
                       diff_df$covr_delta >= thresh, , drop = FALSE]
improved_df <- improved_df[order(-improved_df$covr_delta), , drop = FALSE]

# NEW: unlockers = crossed 65 upward, ranked by rev-dep count
unlockers_df <- diff_df[diff_df$crossed_up, , drop = FALSE]
unlockers_df <- unlockers_df[order(-unlockers_df$n_rev_deps,
                                   -unlockers_df$covr_delta), , drop = FALSE]

# NEW: blockers = still below 65% after run, ranked by rev-dep count.
# Includes both "regressed across 65" AND "was already below and stayed
# below" -- either way, unblocking these propagates the most.
blockers_df <- diff_df[diff_df$below_after, , drop = FALSE]
blockers_df <- blockers_df[order(-blockers_df$n_rev_deps,
                                 blockers_df$covr_new), , drop = FALSE]

cat("== Top 20 regressions (largest -delta) ================\n")
if (nrow(regressed_df) == 0L) cat("  (none)\n") else
  print(.round_disp(utils::head(regressed_df, 20)), row.names = FALSE)
cat("\n")

cat("== Top 20 improvements (largest +delta) ===============\n")
if (nrow(improved_df) == 0L) cat("  (none)\n") else
  print(.round_disp(utils::head(improved_df, 20)), row.names = FALSE)
cat("\n")

cat(sprintf("== Top 20 UNLOCKERS (crossed %d%%, by rev-dep count) ==\n", THR))
if (nrow(unlockers_df) == 0L) cat("  (none)\n") else
  print(.round_disp(utils::head(unlockers_df, 20)), row.names = FALSE)
cat("\n")

cat(sprintf("== Top 20 BLOCKERS (still < %d%%, by rev-dep count) ===\n", THR))
if (nrow(blockers_df) == 0L) cat("  (none)\n") else
  print(.round_disp(utils::head(blockers_df, 20)), row.names = FALSE)
cat("\n")

# ---- Optional decision-category shift ------------------------------

# qual_metadata keys on `pkg` (not `package` like qual_assessments does).
# Accept either; user-supplied frames from unrelated sources might use
# `pkg_name` too. First match wins.
.qm_key_col <- function(df) {
  cands <- c("package", "pkg", "pkg_name")
  hit <- intersect(cands, names(df))
  if (length(hit) == 0L) return(NA_character_)
  hit[[1L]]
}

dec_tab <- NULL
if (!is.null(old_qm_df) && !is.null(new_qm_df) &&
    "final_decision" %in% names(old_qm_df) &&
    "final_decision" %in% names(new_qm_df)) {
  old_key <- .qm_key_col(old_qm_df)
  new_key <- .qm_key_col(new_qm_df)
  if (is.na(old_key) || is.na(new_key)) {
    cat("Warning: qm frames lack a recognizable key column ",
        "(none of package/pkg/pkg_name found); skipping decision diff.\n",
        sep = "")
  } else {
    om <- data.frame(
      package       = old_qm_df[[old_key]],
      decision_old  = old_qm_df$final_decision,
      stringsAsFactors = FALSE
    )
    nm <- data.frame(
      package       = new_qm_df[[new_key]],
      decision_new  = new_qm_df$final_decision,
      stringsAsFactors = FALSE
    )
    dec <- merge(om, nm, by = "package", all = FALSE)
    dec <- dec[dec$package %in% shared, , drop = FALSE]
    cat("== Decision-category shift (old -> new) ===============\n")
    dec_tab <- table(dec$decision_old, dec$decision_new,
                     useNA = "ifany", dnn = c("old", "new"))
    print(dec_tab)
    cat("=======================================================\n\n")
    diff_df <- merge(diff_df, dec, by = "package", all.x = TRUE)
  }
}

# ---- Persist frame -------------------------------------------------

rds_path <- file.path(out_dir, paste0("covr_diff_", run_tag, ".rds"))
saveRDS(diff_df, rds_path)
cat("Saved full diff frame to: ", rds_path, "\n", sep = "")

# ---- HTML report ---------------------------------------------------

.html_escape <- function(x) {
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x
}

.tbl_html <- function(df, empty_msg = "(none)") {
  if (is.null(df) || nrow(df) == 0L) {
    return(paste0("<p class='empty'>", empty_msg, "</p>"))
  }
  cols <- names(df)
  header <- paste0("<tr>", paste0("<th>", .html_escape(cols), "</th>",
                                  collapse = ""), "</tr>")
  cells <- apply(df, 1, function(row) {
    tds <- vapply(seq_along(row), function(j) {
      val <- row[[j]]
      cls <- ""
      if (cols[j] == "covr_delta" && suppressWarnings(!is.na(as.numeric(val)))) {
        d <- as.numeric(val)
        cls <- if (d > 0) " class='pos'" else if (d < 0) " class='neg'" else ""
      }
      paste0("<td", cls, ">", .html_escape(format(val)), "</td>")
    }, character(1))
    paste0("<tr>", paste0(tds, collapse = ""), "</tr>")
  })
  paste0("<table class='diff'>",
         "<thead>", header, "</thead>",
         "<tbody>", paste0(cells, collapse = ""), "</tbody>",
         "</table>")
}

.matrix_html <- function(m, caption = "") {
  if (is.null(m)) return("")
  rn <- rownames(m); cn <- colnames(m)
  hdr <- paste0("<tr><th>", .html_escape(caption), "</th>",
                paste0("<th>", .html_escape(cn), "</th>", collapse = ""),
                "</tr>")
  rows <- vapply(seq_len(nrow(m)), function(i) {
    tds <- paste0("<td>", m[i, ], "</td>", collapse = "")
    paste0("<tr><th class='rowh'>", .html_escape(rn[i]), "</th>",
           tds, "</tr>")
  }, character(1))
  paste0("<table class='matrix'>", hdr, paste0(rows, collapse = ""),
         "</table>")
}

# Small summary panel of counts
count_rows <- data.frame(
  metric = c(
    sprintf("Improved >= %.0f pp",   thresh),
    sprintf("Improved <  %.0f pp",   thresh),
    "Unchanged",
    sprintf("Regressed < %.0f pp",   thresh),
    sprintf("Regressed >= %.0f pp",  thresh),
    sprintf("Crossed UP over %d%%",  THR),
    sprintf("Crossed DOWN under %d%%", THR),
    "Gained metric (NA -> #)",
    "Lost metric (# -> NA)"
  ),
  n = c(counts["improved_big"],  counts["improved_small"],
        counts["unchanged"],
        counts["regressed_small"], counts["regressed_big"],
        counts["crossed_up"], counts["crossed_down"],
        counts["new_only"], counts["lost_metric"]),
  stringsAsFactors = FALSE
)

html <- paste0(
  "<!DOCTYPE html><html><head><meta charset='utf-8'>",
  "<title>val.pipeline covr diff -- ", .html_escape(run_tag), "</title>",
  "<style>",
  "body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;",
  "max-width:1200px;margin:24px auto;padding:0 16px;color:#222;}",
  "h1{font-size:22px;} h2{font-size:17px;margin-top:32px;",
  "border-bottom:1px solid #ddd;padding-bottom:4px;}",
  "table.diff,table.matrix{border-collapse:collapse;font-size:13px;",
  "margin-top:8px;}",
  "table.diff th,table.diff td,table.matrix th,table.matrix td{",
  "border:1px solid #ddd;padding:4px 8px;text-align:right;}",
  "table.diff th,table.matrix th{background:#f4f4f4;text-align:center;}",
  "table.diff td:first-child,table.diff th:first-child,",
  "table.matrix th.rowh{text-align:left;font-weight:600;background:#fafafa;}",
  "td.pos{color:#0a7d1e;font-weight:600;}",
  "td.neg{color:#c1272d;font-weight:600;}",
  ".summary{display:flex;flex-wrap:wrap;gap:16px;margin-top:12px;}",
  ".summary .card{background:#f7f7f7;padding:8px 12px;border-radius:4px;",
  "font-size:13px;} .empty{color:#888;font-style:italic;}",
  "code{background:#f0f0f0;padding:1px 4px;border-radius:3px;}",
  ".context{background:#fffbe6;border-left:4px solid #f0c419;",
  "padding:12px 16px;margin-top:16px;border-radius:2px;font-size:14px;}",
  ".context h3{margin-top:0;font-size:15px;} ",
  ".context h4{font-size:14px;margin-bottom:4px;} ",
  ".context table{font-size:13px;border-collapse:collapse;margin:6px 0;}",
  ".context table th,.context table td{border:1px solid #e0d18a;",
  "padding:3px 8px;text-align:left;background:#fff;}",
  ".context ol,.context ul{margin:6px 0 6px 20px;}",
  ".context li{margin:4px 0;}",
  "</style></head><body>",
  "<h1>val.pipeline covr_coverage diff</h1>",
  "<div class='summary'>",
    "<div class='card'>Run tag: <code>", .html_escape(run_tag), "</code></div>",
    "<div class='card'>Shared pkgs: <b>", length(shared), "</b></div>",
    "<div class='card'>Old only: ", length(only_old), "</div>",
    "<div class='card'>New only: ", length(only_new), "</div>",
    "<div class='card'>Threshold: <b>", THR, "%</b></div>",
  "</div>",

  # ---- Context block: what changed + mechanism -----------------------
  "<div class='context'>",
  "<h3>Context: what changed between these two runs</h3>",
  "<p><b>The only pipeline-side change is <code>options(pkgType)</code>:",
  " it flipped from <code>\"source\"</code> in the old run to ",
  "<code>\"both\"</code> in the new run.</b> No metric definitions, ",
  "no thresholds, no decision rules, no <code>{riskmetric}</code> ",
  "version changes, no dep-graph logic changes. Every delta below ",
  "reflects the covr_coverage number val.pipeline recorded for the ",
  "same target package under those two session states.</p>",

  "<h4>Why the number varies at all -- covr doesn't care about the tarball</h4>",
  "<p><code>covr::package_coverage()</code> installs the target package ",
  "with instrumentation, spawns a fresh R subprocess ",
  "(<code>R CMD BATCH --vanilla</code>), runs the test suite, and reads ",
  "back which lines the tests executed. The target's own code -- ",
  "<code>.R</code> files, <code>.c</code> files, tests -- is byte-",
  "identical whether the tarball was fetched as source or binary. The ",
  "numerator and denominator of the coverage ratio are the same in ",
  "both cases.</p>",

  "<h4>Where the delta actually comes from: dependency install success</h4>",
  "<p>The variance lives in whether <b>test-time dependencies install ",
  "successfully</b>. <code>install.packages()</code> respects ",
  "<code>getOption(\"pkgType\")</code> when resolving deps:</p>",
  "<table>",
  "<thead><tr><th>pkgType</th><th>Behavior</th>",
  "<th>Common failure mode</th></tr></thead>",
  "<tbody>",
  "<tr><td><code>\"source\"</code></td>",
  "<td>Every dep compiled from source</td>",
  "<td>Missing system libs (<code>libcurl-dev</code>, ",
  "<code>libxml2-dev</code>, <code>libgdal-dev</code>, <code>gsl</code>, ",
  "...); PPM binary-only mirror gaps; longer/flakier installs</td></tr>",
  "<tr><td><code>\"both\"</code></td>",
  "<td>Binary preferred, source fallback</td>",
  "<td>Very reliable on PPM-configured Workbench (that's what PPM is for)",
  "</td></tr>",
  "<tr><td><code>\"binary\"</code></td>",
  "<td>Binary only, no fallback</td>",
  "<td>Fails hard on any pkg with no binary -- at least doesn't fail ",
  "silently</td></tr>",
  "</tbody></table>",

  "<p>When a test-time dep fails to install, the affected test file ",
  "typically fails to <code>library()</code>-load. testthat catches the ",
  "load error and marks those test blocks as errored, so covr sees only ",
  "the code exercised by tests that <em>did</em> run and the reported ",
  "percentage drops. <b>Crucially, covr doesn't fail. It reports the ",
  "reduced number as if it were the truth.</b> That's why the regression ",
  "was silent until we ran a controlled A/B.</p>",

  "<h4>Two amplifiers specific to val.pipeline + PPM</h4>",
  "<ol>",
  "<li><b><code>available.packages()</code> filtering.</b> ",
  "<code>configure_riskmetric_offline_if_requested()</code> builds an ",
  "offline pkg cache using <code>available.packages(type = getOption(\"pkgType\"))</code>. ",
  "Under <code>\"source\"</code>, binary-only entries in a PPM mirror ",
  "drop out of the cache entirely -- riskmetric can't even see they ",
  "exist, so downstream install probes for them fail before they ",
  "start.</li>",
  "<li><b>PPM binary mirrors are the workbench norm.</b> When R asks a ",
  "binary-serving PPM URL for source of a pkg that only exists as a ",
  "binary snapshot, it gets nothing. <code>pkgType = \"both\"</code> ",
  "lets install.packages take the binary and move on; <code>\"source\"</code> ",
  "treats the pkg as unavailable.</li>",
  "</ol>",

  "<h4>Why the number varies <em>so much</em>, not just a little</h4>",
  "<p>Under <code>\"source\"</code>, one broken dep can knock out ",
  "several test files, which can knock out entire tested modules, ",
  "easily dropping 30+ percentage points on a package with a ",
  "mid-sized dep tree. Under <code>\"both\"</code>, all the binaries ",
  "flow through cleanly and the target's tests actually run. That's ",
  "the mechanism behind the &gt;20 pp swings you see in the regression ",
  "and improvement lists below.</p>",

  "<h4>Diagnostic tips when inspecting a specific regressor</h4>",
  "<ol>",
  "<li>Open <code>assessed/&lt;pkg&gt;_&lt;ver&gt;_assess_record.rds</code> ",
  "and look at the <code>covr</code> slot. If there are <code>simpleError</code> ",
  "entries with messages like &quot;there is no package called 'X'&quot;, ",
  "that's the smoking gun: X failed to install as a dep and its test file ",
  "bailed.</li>",
  "<li>Search <code>val_pipeline.log</code> for <code>installation failed</code> ",
  "or <code>non-zero exit status</code> near the timestamp of the regressed ",
  "package. Source-compile failures typically log ",
  "<code>ERROR: compilation failed for package 'X'</code> before the pkg is ",
  "silently marked unavailable.</li>",
  "<li>If a regressor is <em>not</em> a system-lib victim (deps install ",
  "fine both ways) but still shows lower covr under <code>\"both\"</code>, ",
  "flag it -- something more interesting is happening than dep-install ",
  "damage, and it's worth digging in.</li>",
  "</ol>",
  "</div>",
  "<h2>Delta summary</h2>",
  .tbl_html(count_rows),
  "<h2>Coverage-band shift (rows = old, cols = new)</h2>",
  .matrix_html(band_shift, caption = "old \\ new"),
  if (!is.null(dec_tab))
    paste0("<h2>Final decision shift (old -> new)</h2>",
           .matrix_html(dec_tab, caption = "old \\ new"))
  else "",
  "<h2>Top 20 regressions (largest -delta)</h2>",
  .tbl_html(.round_disp(utils::head(regressed_df, 20))),
  "<h2>Top 20 improvements (largest +delta)</h2>",
  .tbl_html(.round_disp(utils::head(improved_df, 20))),
  "<h2>Top 20 UNLOCKERS -- crossed ", THR, "% upward, by rev-dep count</h2>",
  "<p>Packages that just qualified. Higher rev-dep count = larger ",
  "downstream slice of the qualified set this pkg unlocks.</p>",
  .tbl_html(.round_disp(utils::head(unlockers_df, 20)),
            empty_msg = "No packages crossed the threshold upward."),
  "<h2>Top 20 BLOCKERS -- still below ", THR, "% after run, by rev-dep count</h2>",
  "<p>Packages that remain unqualified. Ranked by rev-dep count so the ",
  "biggest downstream blockers surface first -- these are the highest-",
  "value investigation targets.</p>",
  .tbl_html(.round_disp(utils::head(blockers_df, 20)),
            empty_msg = "No packages are below the threshold."),
  "<h2>Notes</h2>",
  "<ul>",
  "<li>Rev-dep counts sourced from <code>reverse_dependencies</code> on ",
  "qual_assessments where available; falls back to computing from qm's ",
  "Depends/Imports/LinkingTo columns.</li>",
  "<li><code>version_changed = TRUE</code> flags rows where an upstream ",
  "version bump between runs may account for the delta -- separate from ",
  "any pipeline-side change.</li>",
  "</ul>",
  "</body></html>"
)

html_path <- file.path(out_dir, paste0("covr_diff_", run_tag, ".html"))
writeLines(html, html_path)
cat("Wrote HTML report to    : ", html_path, "\n", sep = "")
cat("Globals: diff_df, regressed_df, improved_df, unlockers_df, blockers_df.\n")

invisible(diff_df)
