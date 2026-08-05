

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
#' @param workers Integer. Number of parallel workers to use during the
#'   per-package assessment loop inside [val_build()]. `1L` (default)
#'   preserves the original serial behaviour, including dep-skip
#'   short-circuiting when a package fails. Values greater than `1`
#'   fan the loop out via [future.apply::future_mapply()] under a
#'   `future::multisession` plan; the dep-skip short-circuit is
#'   disabled in that mode (final risk propagation still runs
#'   downstream via [val_decision()], so package-report accuracy is
#'   unaffected). Requires the optional `{future}` and
#'   `{future.apply}` packages when `workers > 1`.
#' @param freeze_opt_repos Logical(1). When `FALSE` (default), the
#'   config's `opt_repos` CRAN URL is rewritten to match `val_date`
#'   via [update_opt_repos()] (existing behaviour). When `TRUE`, the
#'   config's `opt_repos` is used verbatim so `val_date` can drift from
#'   the frozen PPM snapshot without silently changing which packages
#'   the pipeline pulls. Useful when the org has pinned CRAN to a
#'   specific date in `inst/config.yml` but wants each run's output
#'   folder (`R_<ver>/<YYYYMMDD>/`) to reflect the date the analysis
#'   was actually executed. `val_date` still governs the output
#'   directory name and every `val_date` field written to metadata.
#'   See #89.
#' @param propagate_libpaths Logical(1). Passed through to [val_build()];
#'   see there for the full rationale. In short: when `TRUE` (default),
#'   mirrors the current session's `.libPaths()` into `R_LIBS_SITE` so
#'   `rcmdcheck` / `covr` / any other riskmetric subprocess sees the
#'   same library search order as the parent — critical when the
#'   operator has pointed `.libPaths()` at an rv-provisioned library.
#'   See #99.
#' @param finalize Logical(1). When `TRUE` (default), automatically
#'   calls [val_finalize()] after [val_build()] returns to collate
#'   assessments, propagate dep-driven decisions, and produce the
#'   PPM provisioning files (`qualified-<src>.txt` /
#'   `blocklist-<src>.txt`) plus the HTML + PDF summary report — the
#'   pre-0.1.21 behaviour. When `FALSE`, `val_pipeline()` returns as
#'   soon as the per-package assessment loop finishes; the caller
#'   must run `val_finalize(val_dir)` themselves to produce every
#'   downstream artifact. Useful when the assessment loop is
#'   expensive enough that you want an explicit checkpoint, when
#'   iterating on decision logic against a fixed assessment corpus,
#'   or when recovering from an environment that hangs at the
#'   collation step (see #101).
#' @return Invisibly, `NULL`. The recovery / two-phase workflow is
#'   driven off the `val_prep` object returned by
#'   [val_prep_pipeline()], not this return. In particular, when
#'   `finalize = FALSE`, feed that `prep` object into
#'   `val_finalize(prep = prep)` to complete the pipeline in a fresh
#'   R session — every field it needs (`val_dir`, `val_start`,
#'   `n_candidates`, `deps`, `config_path`, `verbose`) is already
#'   there. The collated artifacts themselves (`qual_metadata.rds`,
#'   `qual_assessments.rds`, etc.) live under `val_dir` on disk.
#'
#' @examples
#' \dontrun{
#' # One-shot (pre-0.1.21 behaviour):
#' val_pipeline()
#'
#' # Two-phase, so you can recover from a fresh R session if the
#' # collation step hangs on your host (#101):
#' prep <- val_prep_pipeline()
#' val_pipeline(prep = prep, finalize = FALSE)
#' # ...later, in the same session or a fresh one:
#' val_finalize(prep = prep)
#' }
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
  config_path = NULL,
  workers = 1L,
  freeze_opt_repos = FALSE,
  propagate_libpaths = getOption("val.pipeline.propagate_libpaths", TRUE),
  finalize = TRUE
  ){

  # Assess args
  ref <- match.arg(ref)
  metric_pkg <- match.arg(metric_pkg)
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))
  stopifnot(is.logical(freeze_opt_repos), length(freeze_opt_repos) == 1L,
            !is.na(freeze_opt_repos))
  stopifnot(is.logical(finalize), length(finalize) == 1L, !is.na(finalize))
  if (!is.null(prep) && !inherits(prep, "val_prep")) {
    stop("`prep` must be a `val_prep` object returned by val_prep_pipeline().",
         call. = FALSE)
  }
  apply_verbose(verbose)
  configure_bioc_repositories_if_requested(quiet = TRUE)
  configure_riskmetric_offline_if_requested(quiet = TRUE)

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
      config_path     = config_path,
      freeze_opt_repos = freeze_opt_repos
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
  # We always pass `finalize = FALSE` here because val_pipeline() owns
  # the full finalization scope (collation + PPM provisioning files +
  # summary report), whereas val_build(finalize = TRUE) only owns the
  # collation half. Delegating everything to val_finalize() below
  # keeps the two-phase (build → finalize) semantics identical whether
  # a caller uses val_pipeline() or drives val_build() themselves.
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
    config_path     = config_path,
    workers         = workers,
    propagate_libpaths = propagate_libpaths,
    finalize        = FALSE
  )

  #
  # ---- Finalize ----
  #
  # val_finalize() runs the collation tail (assessment/meta bundling,
  # reject_iteration() dep-driven decision propagation, timings.csv)
  # AND the PPM provisioning files + summary report. When the caller
  # opts out with `finalize = FALSE`, val_pipeline() returns as soon
  # as the per-package assessment loop finishes so the operator can
  # run val_finalize(val_dir) themselves later — useful for two-phase
  # runs on hosts where the collation step has been observed to hang
  # (see #101), or for iterating on decision logic against a fixed
  # assessment corpus.
  if (isTRUE(finalize)) {
    val_finalize(
      val_dir      = outtie$val_dir,
      deps         = deps,
      val_start    = val_start,
      n_candidates = prep$n_candidates,
      verbose      = verbose,
      config_path  = config_path
    )
  } else {
    val_msg(paste0("\n--> Skipped finalization (finalize = FALSE). ",
                   "In this or a fresh R session, run\n",
                   "      val_finalize(prep = prep)\n",
                   "    to complete the pipeline (val_dir already on disk at\n",
                   "    ", outtie$val_dir, ").\n"),
            min_level = "normal")
  }

  # Return nothing. The recovery / two-phase workflow is driven off
  # the `val_prep` object returned by val_prep_pipeline(), which is
  # the sole source of truth for val_finalize()'s inputs.
  invisible(NULL)
}





