

#' Validation: Execute an Assessment Pipeline
#'
#' A pipeline to validate R packages using specific metrics and criteria,
#' spelled out in the package's config file. This function orchestrates the
#' reduction of a large set of packages delivered through various sources
#' (either \{riskscore\} or a user-provided data set) based one primary &
#' 'exception' criteria. Then, it builds the assessment co-hort using
#' val_build(), returning a qualified list of packages and all required evidence
#' needed for provisioning to PPM.
#'
#' @param ref Character. Source of the packages. Default is "source". Options
#'   are "source" or "remote".
#' @param metric_pkg Character. The package used for metrics. Default is
#'   "riskmetric".
#' @param deps Character or NULL. Types of dependencies to consider. Default is
#'   "depends". Options include "depends", "suggests", or NULL.
#' @param deps_recursive Logical. Whether to consider dependencies recursively.
#'   Default is TRUE.
#' @param val_date Date. The date for validation. Default is the current date.
#' @param replace Logical. Whether to replace existing assessments. Default is
#'   FALSE.
#' @param out Character. Output directory for assessments. Default is
#'   Sys.getenv("RISK_OUTPATH", unset = getwd()).
#' @param opt_repos Named character vector. Repositories to use. Default is
#'   opt_repos from config.
#' @param verbose Console verbosity control. One of `"quiet"`,
#'   `"minimal"`, `"normal"` (default), or `"verbose"`. See
#'   the `val.pipeline` verbosity docs for tier definitions. Also accepts an integer 0-3
#'   or a logical (`TRUE` = `"normal"`, `FALSE` = `"quiet"`). The
#'   session option `val.pipeline.verbose` is used when this argument
#'   is left as the default and set to `NULL`.
#' @param prep Optional `val_prep` object returned by
#'   [val_prep_pipeline()]. When supplied, `val_pipeline()` skips the
#'   entire pre-filter / dependency-resolution phase and jumps straight
#'   to the `val_build()` step, reusing the packages, repositories and
#'   run directory captured by `val_prep_pipeline()`. This is the fast
#'   path callers should take when they've already emitted a
#'   `pipeline.toml` and installed the snapshot via `rv`. When `NULL`
#'   (default), `val_pipeline()` calls `val_prep_pipeline()` itself so
#'   existing one-shot workflows continue to work unchanged.
#' @param config_path Optional path to a user-supplied `config.yml`.
#'   When provided, every internal `pull_config()` call made during this
#'   run reads from that file instead of the `config.yml` bundled with
#'   `val.pipeline`. The override is scoped to this call: the prior
#'   `val.pipeline.config_path` option is restored on exit. When `NULL`
#'   (default), the pre-packaged config is used (or whatever the caller
#'   has already set via `options(val.pipeline.config_path = ...)`).
#' @return A list containing the validation directory and a data frame of
#'   package assessments.
#'
#' @importFrom dplyr as_tibble filter pull select
#' @importFrom tibble rownames_to_column
#'
#' @export
#' 
val_pipeline <- function(
  ref = c("source", "remote"),
  metric_pkg = c("riskmetric", "val.meter", "risk.assessr"),
  # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  deps = c("depends", "suggests")[1], 
  deps_recursive = TRUE,
  val_date = Sys.Date(),
  replace = FALSE, 
  out = Sys.getenv("RISK_OUTPATH", unset = getwd()),
  opt_repos = 
    c(CRAN = "https://packagemanager.posit.co/cran/latest",
      BioC = 'https://bioconductor.org/packages/3.22/bioc'),
  verbose = NULL,
  prep = NULL,
  config_path = NULL
  ){

  # Assess args
  ref <- match.arg(ref)
  metric_pkg <- match.arg(metric_pkg)
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))
  if (!is.null(prep) && !inherits(prep, "val_prep")) {
    stop("`prep` must be a `val_prep` object returned by val_prep_pipeline().",
         call. = FALSE)
  }
  apply_verbose(verbose)

  # Route pull_config() at any depth to the user-supplied config, if any.
  old_cfg <- options()["val.pipeline.config_path"]
  on.exit(options(old_cfg), add = TRUE)
  apply_config_path(config_path)

  #
  # ---- Prep phase ----
  #
  # Either run val_prep_pipeline() ourselves (one-shot mode, backwards
  # compatible), or reuse a caller-supplied prep result (split-run
  # mode, so a `rv` install can happen between the two phases).
  if (is.null(prep)) {
    prep <- val_prep_pipeline(
      ref             = ref,
      metric_pkg      = metric_pkg,
      deps            = deps,
      deps_recursive  = deps_recursive,
      val_date        = val_date,
      out             = out,
      opt_repos       = opt_repos,
      verbose         = verbose,
      config_path     = config_path
    )
  }

  val_start <- prep$val_start %||% Sys.time()
  decisions <- prep$decisions %||%
    pull_config(val = "decisions_lst", rule_type = "default")

  # Keep options aligned with the prep run when picking up from disk.
  old <- options()
  on.exit(options(old), add = TRUE)
  options(repos = prep$opt_repos, pkgType = "source", scipen = 999)

  #
  # ---- val_build() ----
  #
  outtie <- val_build(
    pkg_names       = prep$pkgs,
    ref             = ref,
    metric_pkg      = metric_pkg,
    deps            = deps,
    deps_recursive  = deps_recursive,
    val_date        = prep$val_date,
    replace         = replace,
    out             = out,
    opt_repos       = prep$opt_repos,
    prep            = prep,
    config_path     = config_path
  )
  
  
  
  #
  # ---- Inspect outputs ----
  #
  
  # old - removed for return()
  # qual <- outtie$pkg_meta
  # pkg_assess <- outtie$pkg_assess
  
  # Instead, load metadata
  # qual <- readRDS(file.path(outtie$val_dir, paste0("qual_metadata.rds")))
  # qual_asses <- readRDS(file.path(outtie$val_dir, paste0("qual_assessments.rds")))
  
  
  
  #
  # ---- Wrap up ----
  #
  # Write per-source lists of qualified packages (one pkg name per line)
  # to `qualified-<source>.txt` files alongside qual_metadata.rds. These
  # feed the Posit Package Manager source configuration for the
  # "validated" repo provisioned into the GxP environment. Failure here
  # should not fail the whole pipeline — the qualification evidence is
  # already on disk.
  qa_path <- file.path(outtie$val_dir, "qual_assessments.rds")
  qm_path <- file.path(outtie$val_dir, "qual_metadata.rds")
  qual_meta = readRDS(qm_path)

  if (file.exists(qm_path)) {
    tryCatch(
      write_qualified_pkg_lists(
        qual_metadata = qual_meta,
        out_dir = outtie$val_dir,
        qualified_decision = decisions[1]
      ),
      error = function(e) {
        warning("write_qualified_pkg_lists() failed: ",
                conditionMessage(e), call. = FALSE)
      }
    )
  }

  # Generate a high-level HTML + PDF summary report of the run, saved next to
  # qual_metadata.rds in the val_build() output directory. Suitable for GxP /
  # QMS archival. Failure to render should not fail the whole pipeline. The
  # pre-filter candidate set was already persisted eagerly above (right after
  # it was created) so an interrupted val_build() still leaves it on disk.
  if (file.exists(qm_path)) {
    tryCatch(
      val_pipeline_report(
        qual_metadata_path = qm_path,
        qual_assessments_path = if (file.exists(qa_path)) qa_path else NA,
        out_dir = outtie$val_dir,
        n_candidates = prep$n_candidates %||%
          (available.packages()[,1] |> length()),
        pipeline_runtime = difftime(Sys.time(), val_start, units = "secs")
      ),
      error = function(e) {
        warning("val_pipeline_report() failed: ", conditionMessage(e),
                call. = FALSE)
      }
    )
  }

  # Return the val_build() results (val_dir points to all evidence, incl. the
  # newly rendered summary report).
  # return(qual_meta)
}





