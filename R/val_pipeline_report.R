#' Generate a high-level Quarto summary report for a validation run
#'
#' Renders an HTML and/or PDF summary of a `val_pipeline()` / `val_build()`
#' run, intended for GxP / QMS archival. Given a `qual_metadata.rds` produced
#' by [val_build()], it summarises the final decisions, downgrades due to
#' dependencies, per-category package lists, and runtime. When the sibling
#' `qual_assessments.rds` is also supplied, the report adds metric-level
#' distributions (coverage buckets, R CMD check outcomes, license mix, etc.).
#'
#' The function is exported so it can be re-run outside the pipeline (e.g.
#' against a historical run archived under `/data/shared/riskassessments/`),
#' and is also invoked automatically at the end of [val_pipeline()] so every
#' full run leaves a summary artefact next to its evidence files.
#'
#' # Backward compatibility with older evidence files
#'
#' The template is deliberately tolerant of older `qual_metadata.rds` /
#' `qual_assessments.rds` files so historical runs can still be summarised:
#'
#' * `decision_reason_note` / `final_decision_reason_note` (added by issue
#'   #37) are backfilled with `NA` when absent — the per-category package
#'   tables just show a blank Note column for those rows.
#' * `assessment_runtime_mins` (older files may lack it) — the runtime
#'   section is skipped with a note.
#' * Any metric column in `qual_assessments.rds` may be absent
#'   (`covr_coverage`, `license`, `r_cmd_check_errors`, ...) — the
#'   corresponding sub-section is skipped.
#' * List-typed columns in `qual_assessments.rds` (e.g. raw
#'   `r_cmd_check` output) are ignored when computing numeric summaries.
#'
#' A minimum set of columns is still required in `qual_metadata.rds`:
#' `pkg`, `ver`, `decision`, `decision_reason`, `final_decision`, and
#' `final_decision_reason`. Files missing any of these predate
#' `val.pipeline 0.0.1` and are rejected with an informative error.
#'
#' @param qual_metadata_path Character. Path to a `qual_metadata.rds` file
#'   produced by [val_build()]. Required.
#' @param qual_assessments_path Character or NULL. Optional path to the
#'   sibling `qual_assessments.rds`. When `NULL` (the default), the function
#'   looks for `qual_assessments.rds` in the same directory as
#'   `qual_metadata_path`; pass `NA` to skip this lookup and produce a
#'   metadata-only report.
#' @param out_dir Character. Directory to write the rendered report into.
#'   Defaults to the directory containing `qual_metadata_path`.
#' @param format Character vector. One or more of `"html"`, `"pdf"`. Defaults
#'   to both.
#' @param file_stem Character. Base filename (without extension) for the
#'   rendered report. Defaults to `"val_pipeline_summary"`.
#' @param title Character. Optional report title. When `NULL`, a title is
#'   derived from the validation date on the input.
#' @param subtitle Character. Optional subtitle (e.g. "R 4.5.2 / 2026-06-21
#'   snapshot"). When `NULL`, a subtitle is derived from the input.
#' @param n_candidates Integer or NULL. Number of candidate packages
#'   considered by [val_pipeline()] before the `remote_reduce` filter (i.e.
#'   `nrow(pre_filtered_pkg_metrics)`). When `NULL` (the default), the
#'   function looks for a sibling `pre_filtered_pkg_metrics.rds` in the
#'   directory containing `qual_metadata_path` and uses its row count. Pass
#'   `NA` to explicitly skip this metric.
#' @param pre_filtered_path Character or NULL. Optional path to a
#'   `pre_filtered_pkg_metrics.rds` produced by [val_pipeline()] (the
#'   pre-`val_build()` candidate-set data frame with a `package` and
#'   `final_risk` column). When `NULL` (the default), a sibling file of
#'   that name next to `qual_metadata_path` is used if present. Pass `NA`
#'   to explicitly skip and omit the Pre-Filter Summary section.
#' @param pipeline_runtime Optional total wall-clock runtime of the
#'   [val_pipeline()] call that produced these evidence files. Accepts a
#'   [difftime], a numeric number of seconds, or a pre-formatted character
#'   string. When supplied, an extra `val_pipeline() runtime` row is added
#'   to the Run Metadata table. Not persisted in the evidence RDS files
#'   because it's a pipeline-level (not per-package) fact.
#' @param quiet Logical. Suppress Quarto rendering output. Default `FALSE`.
#' @param verbose Console verbosity control. One of `"quiet"`,
#'   `"minimal"`, `"normal"` (default), or `"verbose"`. See
#'   the `val.pipeline` verbosity docs for tier definitions. The final "Wrote
#'   val.pipeline summary report(s)" confirmation is emitted only at
#'   `"normal"` or above.
#'
#' @return Invisibly, a character vector of the rendered report file paths.
#'
#' @examples
#' \dontrun{
#' val_pipeline_report(
#'   qual_metadata_path = file.path(
#'     "/data/shared/riskassessments/R_4.5.2/20260621",
#'     "qual_metadata.rds"
#'   )
#' )
#' }
#'
#' @export
val_pipeline_report <- function(
  qual_metadata_path,
  qual_assessments_path = NULL,
  out_dir = NULL,
  format = c("html", "pdf"),
  file_stem = "val_pipeline_summary",
  title = NULL,
  subtitle = NULL,
  n_candidates = NULL,
  pre_filtered_path = NULL,
  pipeline_runtime = NULL,
  quiet = FALSE,
  verbose = NULL
) {
  apply_verbose(verbose)
  stopifnot(
    is.character(qual_metadata_path),
    length(qual_metadata_path) == 1L,
    nzchar(qual_metadata_path)
  )
  if (!file.exists(qual_metadata_path)) {
    stop("qual_metadata file not found: ", qual_metadata_path, call. = FALSE)
  }
  qual_metadata_path <- normalizePath(qual_metadata_path, winslash = "/",
                                      mustWork = TRUE)

  required_cols <- c("pkg", "ver", "decision", "decision_reason",
                     "final_decision", "final_decision_reason")
  qm_peek <- readRDS(qual_metadata_path)
  missing_cols <- setdiff(required_cols, names(qm_peek))
  if (length(missing_cols) > 0L) {
    stop(
      "qual_metadata file is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ". This file was likely produced by a version of val_build() ",
      "predating val.pipeline 0.0.1 and is not supported by ",
      "val_pipeline_report().",
      call. = FALSE
    )
  }

  format <- match.arg(format, choices = c("html", "pdf"),
                      several.ok = TRUE)

  input_dir <- dirname(qual_metadata_path)

  # Resolve the optional assessments path.
  #   NULL -> auto-detect sibling qual_assessments.rds
  #   NA   -> explicitly skip
  #   path -> use as given
  qa_path <- if (is.null(qual_assessments_path)) {
    candidate <- file.path(input_dir, "qual_assessments.rds")
    if (file.exists(candidate)) {
      normalizePath(candidate, winslash = "/", mustWork = TRUE)
    } else {
      NULL
    }
  } else if (length(qual_assessments_path) == 1L &&
               is.na(qual_assessments_path)) {
    NULL
  } else {
    if (!file.exists(qual_assessments_path)) {
      stop("qual_assessments file not found: ", qual_assessments_path,
           call. = FALSE)
    }
    normalizePath(qual_assessments_path, winslash = "/", mustWork = TRUE)
  }

  # Sibling config.yml, if any
  config_candidate <- file.path(input_dir, "config.yml")
  config_path <- if (file.exists(config_candidate)) {
    normalizePath(config_candidate, winslash = "/", mustWork = TRUE)
  } else {
    NULL
  }

  # Resolve n_candidates.
  #   NULL -> try sibling pre_filtered_pkg_metrics.rds
  #   NA   -> explicitly skip
  #   numeric -> use as given
  n_candidates_val <- if (is.null(n_candidates)) {
    pf_candidate <- file.path(input_dir, "pre_filtered_pkg_metrics.rds")
    if (file.exists(pf_candidate)) {
      tryCatch(nrow(readRDS(pf_candidate)), error = function(e) NULL)
    } else {
      NULL
    }
  } else if (length(n_candidates) == 1L && is.na(n_candidates)) {
    NULL
  } else {
    stopifnot(is.numeric(n_candidates), length(n_candidates) == 1L,
              is.finite(n_candidates), n_candidates >= 0)
    as.integer(n_candidates)
  }

  # Resolve pre_filtered_path.
  #   NULL -> try sibling pre_filtered_pkg_metrics.rds
  #   NA   -> explicitly skip
  #   path -> use as given
  pf_path <- if (is.null(pre_filtered_path)) {
    pf_candidate <- file.path(input_dir, "pre_filtered_pkg_metrics.rds")
    if (file.exists(pf_candidate)) {
      normalizePath(pf_candidate, winslash = "/", mustWork = TRUE)
    } else {
      NULL
    }
  } else if (length(pre_filtered_path) == 1L &&
               is.na(pre_filtered_path)) {
    NULL
  } else {
    if (!file.exists(pre_filtered_path)) {
      stop("pre_filtered_pkg_metrics file not found: ", pre_filtered_path,
           call. = FALSE)
    }
    normalizePath(pre_filtered_path, winslash = "/", mustWork = TRUE)
  }

  if (is.null(out_dir)) out_dir <- input_dir
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)

  # Peek at inputs to synthesize title/subtitle/date
  qm <- qm_peek
  val_dates <- unique(as.character(qm$val_date))
  r_vers <- unique(as.character(qm$r_ver))
  if (is.null(title)) {
    title <- "val.pipeline Qualification Summary"
  }
  if (is.null(subtitle)) {
    subtitle <- paste0(
      "R ", paste(r_vers, collapse = ", "),
      " \u2014 ",
      paste(val_dates, collapse = ", ")
    )
  }
  val_date_str <- val_dates[1L]

  # Locate the template shipped with the package and copy to a temp working
  # directory so Quarto's intermediate outputs don't touch shared storage.
  tmpl_src <- system.file("report", "summary", "summary_template.qmd",
                          package = "val.pipeline")
  if (!nzchar(tmpl_src) || !file.exists(tmpl_src)) {
    stop("Could not locate summary_template.qmd inside val.pipeline. ",
         "Reinstall the package.", call. = FALSE)
  }

  work_dir <- file.path(tempfile(pattern = "val_pipeline_report_"))
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  qmd_path <- file.path(work_dir, "summary_template.qmd")
  file.copy(tmpl_src, qmd_path, overwrite = TRUE)

  # Precompute the metric-thresholds table from the currently-installed
  # (or loaded) config and stash it as an RDS in the working dir. The
  # template reads it back rather than calling val.pipeline::* directly,
  # which lets the template render even from a fresh R subprocess that
  # doesn't have val.pipeline on its search path (e.g. during
  # devtools::load_all() smoke tests).
  thresholds_rds <- file.path(work_dir, "metric_thresholds.rds")
  tryCatch({
    thresholds_df <- build_decisions_df(rule_type = "decide") |>
      dplyr::mutate(
        pretty = vapply(condition, pretty_rule_condition, character(1))
      ) |>
      dplyr::select(metric, metric_type, decision, pretty, auto_accept) |>
      tidyr::pivot_wider(
        id_cols = c(metric, metric_type, auto_accept),
        names_from = decision,
        values_from = pretty
      ) |>
      dplyr::mutate(
        auto_accept = vapply(auto_accept, pretty_rule_condition,
                             character(1)),
        metric_type = tools::toTitleCase(as.character(metric_type))
      ) |>
      dplyr::select(Metric = metric,
                    Type = metric_type,
                    dplyr::any_of(c("Low", "Medium", "High")),
                    `Auto-Accept` = auto_accept) |>
      dplyr::arrange(Type, Metric)
    saveRDS(thresholds_df, thresholds_rds)
  }, error = function(e) {
    warning("Could not precompute metric thresholds table: ",
            conditionMessage(e), call. = FALSE)
    if (file.exists(thresholds_rds)) unlink(thresholds_rds)
  })

  # Map friendly names to Quarto format identifiers
  format_map <- c(html = "html", pdf = "typst")
  quarto_formats <- unname(format_map[format])

  # Resolve pipeline_runtime.
  #   NULL / NA / "" -> skip
  #   difftime      -> convert to seconds, format as "Nh Nm Ns"
  #   numeric       -> treat as seconds
  #   character     -> pass through verbatim
  runtime_str <- if (is.null(pipeline_runtime) ||
                      (length(pipeline_runtime) == 1L &&
                         is.na(pipeline_runtime))) {
    NULL
  } else if (inherits(pipeline_runtime, "difftime")) {
    format_runtime_seconds(as.numeric(pipeline_runtime, units = "secs"))
  } else if (is.numeric(pipeline_runtime)) {
    stopifnot(length(pipeline_runtime) == 1L,
              is.finite(pipeline_runtime), pipeline_runtime >= 0)
    format_runtime_seconds(as.numeric(pipeline_runtime))
  } else if (is.character(pipeline_runtime)) {
    stopifnot(length(pipeline_runtime) == 1L)
    if (nzchar(pipeline_runtime)) pipeline_runtime else NULL
  } else {
    stop("pipeline_runtime must be a difftime, numeric seconds, ",
         "character, or NULL.", call. = FALSE)
  }

  execute_params <- list(
    title = title,
    subtitle = subtitle,
    qual_metadata_path = qual_metadata_path,
    qual_assessments_path = qa_path,
    val_date = val_date_str,
    config_path = config_path,
    thresholds_path = if (file.exists(thresholds_rds)) thresholds_rds else NULL,
    n_candidates = n_candidates_val,
    pre_filtered_path = pf_path,
    pipeline_runtime = runtime_str
  )

  quarto::quarto_render(
    input = qmd_path,
    output_format = quarto_formats,
    execute_params = execute_params,
    quiet = quiet
  )

  # Quarto writes outputs next to the .qmd. Collect and move them.
  ext_map <- c(html = "html", pdf = "pdf")
  produced <- character(0)
  for (fmt in format) {
    src <- file.path(work_dir, paste0("summary_template.", ext_map[[fmt]]))
    if (!file.exists(src)) {
      warning("Expected rendered ", fmt, " output was not produced: ", src,
              call. = FALSE)
      next
    }
    dest <- file.path(out_dir, paste0(file_stem, ".", ext_map[[fmt]]))
    file.copy(src, dest, overwrite = TRUE)
    produced <- c(produced, dest)
  }

  if (length(produced) > 0L && !quiet && val_verbosity_at_least("minimal")) {
    message("Wrote val.pipeline summary report(s):\n  ",
            paste(produced, collapse = "\n  "))
  }

  invisible(produced)
}
