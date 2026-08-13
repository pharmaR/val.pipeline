#' Finalize a `val_build()` Run
#'
#' Takes a run directory whose per-package `_assess_record.rds` and
#' `_meta.rds` files have already been produced by [val_build()] and
#' produces the run-level artifacts on top of them:
#'
#' \enumerate{
#'   \item `qual_assessments.rds`  — every package's riskmetric record,
#'     `dplyr::bind_rows()`-collated from `assessed/*_assess_record.rds`.
#'   \item `qual_metadata0.rds`    — interim per-package qualification
#'     frame, `dplyr::bind_rows()`-collated from `assessed/*_meta.rds`.
#'     Kept as a separate file so the pre-propagation state remains
#'     inspectable for debugging decision-graph issues.
#'   \item `qual_metadata.rds`     — final per-package qualification
#'     frame after [reject_iteration()] propagates dep-driven
#'     downgrades (a package inherits its worst-off dependency's
#'     decision).
#'   \item Rewritten `assessed/*_meta.rds` files for every package
#'     whose `decision` changed under propagation, so per-package
#'     reports pick up the new `final_decision` /
#'     `final_decision_reason` / `final_decision_reason_note`.
#'   \item `timings.csv`           — long-format per-phase timings
#'     built in the same streaming pass as `qual_metadata0.rds`. Feed
#'     this to [val_timings_summary()] for profiling.
#'   \item `qualified-<src>.txt` / `blocklist-<src>.txt` (opt-out via
#'     `write_qualified_lists = FALSE`) — per-source PPM provisioning
#'     files via [write_qualified_pkg_lists()].
#'   \item HTML + PDF summary report (opt-out via `render_report =
#'     FALSE`) — high-level run summary via [val_pipeline_report()].
#' }
#'
#' `val_finalize()` is the tail half of what used to be a monolithic
#' [val_build()] before val.pipeline v0.1.21. Splitting it out means:
#'
#' \itemize{
#'   \item A `val_build()` that hangs / gets killed / OOMs *after* the
#'     per-package assessment loop but before the collation runs can
#'     now be recovered from disk without re-running the 40h
#'     assessment: fresh session, `val_finalize("<val_dir>")`, done.
#'   \item Ad-hoc callers who want to poke at the raw per-package RDS
#'     files (e.g. selectively rerun a handful of packages via
#'     [val_pkg()]) can now regenerate the collated artifacts on
#'     demand.
#' }
#'
#' See #101 for the split rationale and the hang symptom that
#' motivated it.
#'
#' @param val_dir Character(1). Path to a run directory produced by
#'   [val_build()] (`R_<ver>/<YYYYMMDD>/`) that contains an
#'   `assessed/` subdirectory with per-package `*_assess_record.rds`
#'   and `*_meta.rds` files. Must exist.
#' @param deps Character(1). Dependency edge set to propagate
#'   downgrades along. `"depends"` (default) covers Depends + Imports
#'   + LinkingTo (matches [val_build()]'s default and how the
#'   `_meta.rds` files were built). `"suggests"` propagates through
#'   Suggests too. Same semantics as [reject_iteration()]'s `deps`
#'   arg.
#' @param val_start POSIXct or `NULL`. Wall-clock start time of the
#'   original `val_build()` invocation, used to render a "Build Xh Xm
#'   Xs" summary line and to compute `pipeline_runtime` for the
#'   summary report. When `NULL` (default; the case when
#'   `val_finalize()` is called ad-hoc from a fresh session), the
#'   wall-clock line and `pipeline_runtime` are skipped.
#' @param n_candidates Integer(1) or `NULL`. Pre-filter candidate-set
#'   size, forwarded to [val_pipeline_report()]. When `NULL`
#'   (default), the report auto-derives it from
#'   `pre_filtered_pkg_metrics.rds` in `val_dir` if present.
#' @param write_qualified_lists Logical(1). When `TRUE` (default),
#'   calls [write_qualified_pkg_lists()] to produce the per-source
#'   `qualified-<src>.txt` / `blocklist-<src>.txt` files. Failure is
#'   downgraded to a warning; the collated qual_* RDS files are
#'   already on disk regardless.
#' @param render_report Logical(1). When `TRUE` (default), calls
#'   [val_pipeline_report()] to render the HTML + PDF summary report.
#'   Failure is downgraded to a warning.
#' @param verbose Console verbosity control. One of `"quiet"`,
#'   `"minimal"`, `"normal"` (default), or `"verbose"`. See the
#'   `val.pipeline` verbosity docs for tier definitions.
#' @param config_path Optional path to a user-supplied `config.yml`.
#'   When provided, every internal [pull_config()] call in this run
#'   reads from that file. When `NULL` (default), `val_finalize()`
#'   uses the `config.yml` copy that [val_build()] snapshotted next
#'   to `val_dir` (if present) — matching the config the assessment
#'   loop actually ran under. Falls back to the packaged config when
#'   neither is present.
#'
#' @return Invisibly, a list with element `val_dir` (character path).
#'
#' @importFrom dplyr as_tibble bind_rows filter
#' @importFrom purrr compact imap list_flatten list_rbind map pwalk walk
#' @importFrom glue glue
#' @importFrom utils capture.output write.csv
#'
#' @param prep Optional `val_prep` object returned by
#'   [val_prep_pipeline()]. When supplied, its fields become the
#'   defaults for `val_dir`, `val_start`, `n_candidates`, `deps`,
#'   `config_path`, and `verbose` — so
#'   `val_finalize(prep = prep)` is the recommended recovery
#'   one-liner after a two-phase run
#'   (`val_pipeline(prep = prep, finalize = FALSE)`). Any argument
#'   passed explicitly to `val_finalize()` overrides the corresponding
#'   `prep` field. When `NULL` (default), every argument comes from
#'   its usual place.
#'
#' @examples
#' \dontrun{
#' # Preferred recovery flow — one object drives both phases:
#' prep <- val_prep_pipeline()
#' val_pipeline(prep = prep, finalize = FALSE)
#' # ...same or a fresh session, later:
#' val_finalize(prep = prep)
#'
#' # Ad-hoc, when all you have is the run directory on disk:
#' val_finalize("/data/pm/riskassessments/R_4.5.2/20260721")
#'
#' # Skip the PPM provisioning files + summary report for a quick
#' # collate-only pass (useful when iterating on decision logic):
#' val_finalize("...", write_qualified_lists = FALSE, render_report = FALSE)
#' }
#'
#' @export
val_finalize <- function(
    val_dir = NULL,
    deps = "depends",
    val_start = NULL,
    n_candidates = NULL,
    write_qualified_lists = TRUE,
    render_report = TRUE,
    verbose = NULL,
    config_path = NULL,
    prep = NULL
) {
  # Populate defaults from `prep` where the caller left them at the
  # function-default sentinel. Explicit args always win. `deps`'s
  # sentinel is the default "depends" — using missing() would be more
  # rigorous but adds noise for a rare corner (caller passing
  # deps = "depends" explicitly with a prep$deps = "suggests"). If
  # that ever bites, switch to missing()-based detection.
  if (!is.null(prep)) {
    if (!inherits(prep, "val_prep")) {
      stop("`prep` must be a `val_prep` object returned by ",
           "val_prep_pipeline().", call. = FALSE)
    }
    if (is.null(val_dir))       val_dir      <- prep$val_dir
    if (is.null(val_start))     val_start    <- prep$val_start
    if (is.null(n_candidates))  n_candidates <- prep$n_candidates
    if (is.null(config_path))   config_path  <- prep$config_path
    if (is.null(verbose))       verbose      <- prep$verbose
    if (identical(deps, "depends") && !is.null(prep$deps)) {
      deps <- prep$deps
    }
  }

  if (is.null(val_dir)) {
    stop("`val_dir` must be supplied (either directly or via `prep`).",
         call. = FALSE)
  }
  stopifnot(
    is.character(val_dir), length(val_dir) == 1L, nzchar(val_dir)
  )
  if (!dir.exists(val_dir)) {
    stop("val_dir does not exist: ", val_dir, call. = FALSE)
  }
  assessed <- file.path(val_dir, "assessed")
  if (!dir.exists(assessed)) {
    stop("val_dir has no 'assessed/' subdirectory (nothing to collate): ",
         val_dir, call. = FALSE)
  }
  # `deps` may be NULL (no dep propagation) or a scalar string like
  # "depends" / "suggests"; matches val_build()'s pre-#101 contract.
  if (!is.null(deps)) {
    stopifnot(is.character(deps), length(deps) == 1L, !is.na(deps))
  }
  stopifnot(is.logical(write_qualified_lists),
            length(write_qualified_lists) == 1L, !is.na(write_qualified_lists))
  stopifnot(is.logical(render_report),
            length(render_report) == 1L, !is.na(render_report))
  if (!is.null(val_start) && !inherits(val_start, "POSIXt")) {
    stop("val_start must be a POSIXt (or NULL).", call. = FALSE)
  }

  apply_verbose(verbose)

  # Prefer the config.yml that val_build() snapshotted next to val_dir
  # (matches what the assessment loop ran under) when the caller didn't
  # pass one explicitly. Falls back to the packaged config through
  # resolve_config_path().
  effective_cfg <- if (is.null(config_path)) {
    snap <- file.path(val_dir, "config.yml")
    if (file.exists(snap)) snap else NULL
  } else {
    config_path
  }
  old_cfg <- options()["val.pipeline.config_path"]
  on.exit(options(old_cfg), add = TRUE)
  apply_config_path(effective_cfg)

  # Log everything into the same val_pipeline.log val_build() opened
  # (init_val_log always appends).
  log_file <- file.path(val_dir, "val_pipeline.log")
  init_val_log(
    log_file,
    header = paste0("\n=== val_finalize() @ ",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                    " (R ", getRversion(), ") ===\n")
  )
  old_log_opts <- options(val.pipeline.log_file = log_file)
  on.exit(options(old_log_opts), add = TRUE)

  decisions <- pull_config(val = "decisions_lst", rule_type = "default")

  # Top-level banner so an operator watching the log knows val_finalize()
  # actually started and against which run directory.
  val_msg(paste0("\n\n== val_finalize() @ ",
                 format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                 " ==\n",
                 "    val_dir: ", val_dir, "\n",
                 "    deps:    ", (if (is.null(deps)) "<none>" else deps), "\n"),
          min_level = "normal")

  #
  # ---- Collate Assessment files into DF ----
  #
  record_files <- list.files(assessed, pattern = "_assess_record.rds$")
  if (length(record_files) == 0L) {
    stop("No `_assess_record.rds` files found under ", assessed,
         " to collate into `qual_assessments.rds`.", call. = FALSE)
  }
  val_msg(paste0("\n--> [1/5] Collating ", length(record_files),
                 " `_assess_record.rds` file(s) into ",
                 "qual_assessments.rds ...\n"),
          min_level = "normal")
  t_collate_assess <- Sys.time()
  assessment_bundle <- purrr::map(record_files, function(file){
    readRDS(file.path(assessed, file))
  }) |>
    dplyr::bind_rows()
  qual_assessments_file <- file.path(val_dir, "qual_assessments.rds")
  saveRDS(assessment_bundle, qual_assessments_file)
  val_msg(paste0("    Saved ", nrow(assessment_bundle), " assessment record(s) to ",
                 qual_assessments_file, " (",
                 format(round(difftime(Sys.time(), t_collate_assess, units = "secs"), 1)),
                 ").\n"),
          min_level = "minimal")
  rm(assessment_bundle)
  invisible(gc(verbose = FALSE))

  #
  # ---- Collate Pkg Meta into DF (streaming) ----
  #
  meta_files <- list.files(assessed, pattern = "_meta.rds$")
  if (length(meta_files) == 0L) {
    stop("No `_meta.rds` files found under ", assessed,
         " to collate into `qual_metadata0.rds`.", call. = FALSE)
  }
  val_msg(paste0("\n--> [2/5] Streaming ", length(meta_files),
                 " `_meta.rds` file(s) into qual_metadata0.rds ",
                 "+ timings.csv ...\n"),
          min_level = "normal")
  t_collate_meta <- Sys.time()
  # Progress crumbs every ~10% for large runs so a stalled read is
  # obvious in the log. Bounded at 10 crumbs regardless of cohort size.
  prog_every <- max(1L, floor(length(meta_files) / 10L))

  pkgs_df0_rows <- vector("list", length(meta_files))
  timings_rows  <- vector("list", length(meta_files))
  for (i in seq_along(meta_files)) {
    bundle <- readRDS(file.path(assessed, meta_files[[i]]))
    tmap <- bundle[["timings"]]
    bundle[["timings"]] <- NULL

    x <- purrr::list_flatten(bundle)
    x$depends  <- list(x$depends)
    x$suggests <- list(x$suggests)
    # Backwards-compat with legacy meta bundles (pre-#107): direct-dep
    # fields may be absent -- fall back to the recursive fields so
    # reject_iteration() still has something to intersect with.
    if (is.null(x$depends_direct))  x$depends_direct  <- x$depends[[1]]
    if (is.null(x$suggests_direct)) x$suggests_direct <- x$suggests[[1]]
    x$depends_direct  <- list(x$depends_direct)
    x$suggests_direct <- list(x$suggests_direct)
    x$rev_deps <- list(x$rev_deps)
    x$sys_info <- list(x$sys_info)
    pkgs_df0_rows[[i]] <- dplyr::as_tibble(x)

    if (!is.null(tmap) && length(tmap) > 0L) {
      pkg_name <- bundle[["pkg"]]
      ver_val  <- bundle[["ver"]]
      if (is.null(ver_val)) ver_val <- NA_character_
      timings_rows[[i]] <- purrr::imap(tmap, function(secs, phase) {
        data.frame(
          pkg     = pkg_name,
          ver     = as.character(ver_val),
          phase   = phase,
          seconds = as.numeric(secs),
          stringsAsFactors = FALSE
        )
      }) |> purrr::list_rbind()
    }
    if (i %% prog_every == 0L || i == length(meta_files)) {
      val_msg(paste0("    read ", i, " / ", length(meta_files),
                     " `_meta.rds` file(s)\n"),
              min_level = "verbose")
    }
    rm(bundle)
  }
  pkgs_df0 <- dplyr::bind_rows(pkgs_df0_rows)
  rm(pkgs_df0_rows)

  qual_metadata0_file <- file.path(val_dir, "qual_metadata0.rds")
  saveRDS(pkgs_df0, qual_metadata0_file)
  val_msg(paste0("    Saved interim pkg metadata (", nrow(pkgs_df0),
                 " row(s)) to ", qual_metadata0_file, " (",
                 format(round(difftime(Sys.time(), t_collate_meta, units = "secs"), 1)),
                 ").\n"),
          min_level = "minimal")

  #
  # ---- Update final decisions ----
  #
  # reject_iteration() walks the dep graph propagating downgrades from
  # every non-Low package to its reverse dependencies. Iterated until
  # the failed-pkg set stops growing (fixed-point). Each iteration
  # gets a crumb so a stalled propagation is obvious in the log.
  val_msg("\n--> [3/5] Propagating dep-driven decisions ...\n",
          min_level = "normal")
  t_reject <- Sys.time()

  dec_reject <- decisions[length(decisions)]
  seed_failed <- pkgs_df0$pkg[pkgs_df0$decision != decisions[1]]
  val_msg(paste0("    ", length(seed_failed), " pkg(s) start above '",
                 decisions[1], "' (seed set for propagation).\n"),
          min_level = "normal")

  failed <- seed_failed
  pkgs_df <- reject_iteration(pkgs_df0, dec_reject, deps, decisions, failed)
  iter <- 1L
  n_after <- sum(pkgs_df$final_decision != decisions[1])
  val_msg(paste0("    iter ", iter, ": ", n_after, " pkg(s) above '",
                 decisions[1], "' after propagation (+",
                 n_after - length(seed_failed), " vs. seed).\n"),
          min_level = "normal")

  while (!identical(pkgs_df$pkg[pkgs_df$final_decision != decisions[1]],
                    failed)) {
    failed <<- pkgs_df$pkg[pkgs_df$final_decision != decisions[1]]
    pkgs_df <<- reject_iteration(pkgs_df, dec_reject, deps, decisions, failed)
    iter <- iter + 1L
    n_after <- sum(pkgs_df$final_decision != decisions[1])
    val_msg(paste0("    iter ", iter, ": ", n_after, " pkg(s) above '",
                   decisions[1], "' after propagation.\n"),
            min_level = "normal")
  }
  n_final <- sum(pkgs_df$final_decision != decisions[1])
  val_msg(paste0("    Converged in ", iter, " iteration(s); final tally: ",
                 nrow(pkgs_df) - n_final, " '", decisions[1], "', ",
                 n_final, " non-'", decisions[1], "' (",
                 format(round(difftime(Sys.time(), t_reject, units = "secs"), 1)),
                 ").\n"),
          min_level = "minimal")

  saveRDS(pkgs_df, file.path(val_dir, "qual_metadata.rds"))
  val_msg(paste0("    Saved qualification evidence to ",
                 file.path(val_dir, "qual_metadata.rds"), ".\n"),
          min_level = "minimal")

  #
  # ---- Rewrite per-pkg _meta.rds for propagated decisions ----
  #
  changed_pkgs <- pkgs_df |>
    dplyr::filter(final_decision != decision)

  val_msg(paste0("\n--> [4/5] Rewriting per-pkg `_meta.rds` for ",
                 nrow(changed_pkgs), " pkg(s) whose decision changed ",
                 "under dep propagation ...\n"),
          min_level = "normal")
  t_rewrite <- Sys.time()
  n_rewritten <- 0L

  purrr::pwalk(
    list(changed_pkgs$pkg, changed_pkgs$ver,
         changed_pkgs$final_decision_reason,
         changed_pkgs$final_decision_reason_note),
    function(pkg, ver, reason, note){
      pkg_v <- paste(pkg, ver, sep = "_")
      pkg_meta_file <- file.path(assessed, glue::glue("{pkg_v}_meta.rds"))
      pkg_meta_file <- pkg_meta_file[file.exists(pkg_meta_file)]
      if (length(pkg_meta_file) > 0) {
        purrr::walk(pkg_meta_file, function(f){
          dep_meta <- readRDS(f)
          # `reason` may be "Dependency" or "Pre-Approved (dep failed)"
          # (#110), whichever reject_iteration() emitted.
          dep_meta$final_decision_reason <- reason
          dep_meta$final_decision_reason_note <- note
          dep_meta$final_decision <- decisions[length(decisions)]
          saveRDS(dep_meta, f)
          n_rewritten <<- n_rewritten + 1L
          val_msg(paste0("    ", dep_meta$pkg, " v", dep_meta$ver,
                         ": '", dep_meta$decision, "' -> '",
                         dep_meta$final_decision, "'\n"),
                  min_level = "verbose")
        })
      }
    })
  val_msg(paste0("    Rewrote ", n_rewritten, " `_meta.rds` file(s) (",
                 format(round(difftime(Sys.time(), t_rewrite, units = "secs"), 1)),
                 ").\n"),
          min_level = "minimal")

  #
  # ---- Aggregate per-package timings ----
  #
  val_msg("\n--> [5/5] Aggregating per-phase timings ...\n",
          min_level = "normal")
  timings_df <- purrr::list_rbind(purrr::compact(timings_rows))
  rm(timings_rows)
  if (nrow(timings_df) > 0L) {
    timings_file <- file.path(val_dir, "timings.csv")
    utils::write.csv(timings_df, timings_file, row.names = FALSE)
    val_msg(paste0("    Wrote ", nrow(timings_df), " row(s) covering ",
                   length(unique(timings_df$pkg)), " pkg(s) x ",
                   length(unique(timings_df$phase)), " phase(s) to ",
                   timings_file, ".\n"),
            min_level = "minimal")
  } else {
    val_msg("    No timings data on `_meta.rds` bundles; skipping ",
            "timings.csv.\n",
            min_level = "minimal")
  }

  #
  # ---- Memory watchdog summary ----
  #
  # Emits p50/p95/max per-pkg peak RSS, the top-N heaviest packages, and
  # (on Linux) a suggested `workers` for the next run keyed on
  # available RAM / p95. Silently no-ops if no mem_watchdog.tsv landed
  # (e.g. val_build(mem_watchdog = FALSE), fully cached rerun, or
  # sampler unavailable on this host). See #122.
  wd_path <- file.path(val_dir, "mem_watchdog.tsv")
  if (file.exists(wd_path)) {
    wd_sum <- summarize_mem_watchdog(wd_path)
    if (!is.null(wd_sum)) {
      val_msg(paste0(
        "\n--> Memory watchdog (", wd_sum$n, " pkg(s) sampled",
        if (nzchar(wd_sum$sampler_mix))
          paste0("; sampler ", wd_sum$sampler_mix)
        else "", "):\n",
        "    p50 peak RSS: ", format(wd_sum$p50_mb), " MB\n",
        "    p95 peak RSS: ", format(wd_sum$p95_mb), " MB\n",
        "    max peak RSS: ", format(wd_sum$max_mb), " MB\n"),
        min_level = "minimal")
      if (nrow(wd_sum$top) > 0L) {
        val_msg("    Top heaviest packages:\n", min_level = "minimal")
        for (i in seq_len(nrow(wd_sum$top))) {
          r <- wd_sum$top[i, , drop = FALSE]
          val_msg(paste0(
            "      ", format(i, width = 2), ". ", r$pkg,
            if ("version" %in% names(r)) paste0(" v", r$version) else "",
            " -- ", format(round(r$peak_rss_mb, 1)), " MB",
            if ("elapsed_sec" %in% names(r) && is.finite(r$elapsed_sec))
              paste0(" (", round(r$elapsed_sec, 1), "s)")
            else "", "\n"),
            min_level = "minimal")
        }
      }
      if (!is.null(wd_sum$suggested_workers)) {
        val_msg(paste0(
          "    Available RAM: ~", round(wd_sum$available_ram_gb, 1),
          " GB; reserve: ", wd_sum$reserve_gb, " GB; ",
          "budget / p95_gb -> suggested workers for next run: ",
          wd_sum$suggested_workers, ".\n"),
          min_level = "minimal")
      }
    }
  }

  #
  # ---- Wall-clock ----
  #
  if (!is.null(val_start)) {
    val_end <- Sys.time()
    val_end_txt <- utils::capture.output(val_end - val_start)
    val_msg("\n--> Build", val_end_txt, "\n", min_level = "minimal")
  }

  #
  # ---- Per-source PPM provisioning files ----
  #
  qm_path <- file.path(val_dir, "qual_metadata.rds")
  qa_path <- file.path(val_dir, "qual_assessments.rds")

  if (isTRUE(write_qualified_lists) && file.exists(qm_path)) {
    val_msg("\n--> Writing PPM provisioning files ",
            "(qualified-<src>.txt / blocklist-<src>.txt) ...\n",
            min_level = "normal")
    t_qual <- Sys.time()
    tryCatch({
      write_qualified_pkg_lists(
        qual_metadata = readRDS(qm_path),
        out_dir = val_dir,
        qualified_decision = decisions[1]
      )
      val_msg(paste0("    Done (",
                     format(round(difftime(Sys.time(), t_qual, units = "secs"), 1)),
                     ").\n"),
              min_level = "minimal")
    },
      error = function(e) {
        warning("write_qualified_pkg_lists() failed: ",
                conditionMessage(e), call. = FALSE)
      }
    )
  } else if (!isTRUE(write_qualified_lists)) {
    val_msg("\n--> Skipping PPM provisioning files ",
            "(write_qualified_lists = FALSE).\n",
            min_level = "normal")
  }

  #
  # ---- Summary report ----
  #
  if (isTRUE(render_report) && file.exists(qm_path)) {
    val_msg("\n--> Rendering summary report (HTML + PDF) ...\n",
            min_level = "normal")
    t_report <- Sys.time()
    pipeline_runtime <- if (!is.null(val_start)) {
      difftime(Sys.time(), val_start, units = "secs")
    } else {
      NA
    }
    tryCatch({
      val_pipeline_report(
        qual_metadata_path    = qm_path,
        qual_assessments_path = if (file.exists(qa_path)) qa_path else NA,
        out_dir               = val_dir,
        n_candidates          = n_candidates,
        pipeline_runtime      = pipeline_runtime
      )
      val_msg(paste0("    Done (",
                     format(round(difftime(Sys.time(), t_report, units = "secs"), 1)),
                     ").\n"),
              min_level = "minimal")
    },
      error = function(e) {
        warning("val_pipeline_report() failed: ", conditionMessage(e),
                call. = FALSE)
      }
    )
  } else if (!isTRUE(render_report)) {
    val_msg("\n--> Skipping summary report (render_report = FALSE).\n",
            min_level = "normal")
  }

  val_msg(paste0("\n== val_finalize() done @ ",
                 format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
                 " ==\n",
                 "    Artifacts under: ", val_dir, "\n"),
          min_level = "normal")

  invisible(list(val_dir = val_dir))
}
