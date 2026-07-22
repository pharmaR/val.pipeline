
#' Validation: Prep Stage of the Assessment Pipeline
#'
#' Runs everything `val_pipeline()` normally does *before* the per-package
#' assessment loop kicks off — pulling config, evaluating viable metrics,
#' running the pre-filter (`val_categorize()`), applying the primary
#' filter + `pass_primary` inclusion, and then resolving the full
#' (optionally recursive) dependency tree via [tools::package_dependencies()]
#' so the final ordered list of packages that `val_build()` would iterate
#' over is materialised eagerly. Finally, it writes that list out as a
#' `pipeline.toml` file suitable for feeding to `rv` so a Posit Package
#' Manager snapshot can be installed *before* the (expensive) build step
#' begins.
#'
#' The returned object can be fed straight into `val_pipeline(prep = ...)`
#' (or `val_build(prep = ...)`) to skip the prep phase on the second leg
#' of the run.
#'
#' @inheritParams val_pipeline
#' @param toml_path Character or `NULL`. Where to write the
#'   `pipeline.toml` file. Defaults to `file.path(val_dir, "pipeline.toml")`
#'   where `val_dir` is the run's date-stamped output directory.
#' @param toml_project_name Character(1). Value written to the toml's
#'   `[project].name` field. Defaults to `"val.pipeline run"`.
#'
#' @return A list of class `"val_prep"` containing:
#' \describe{
#'   \item{pkgs}{Character vector of package names, sorted in the
#'     dependency-frequency order `val_build()` uses.}
#'   \item{vers}{Character vector of package versions, aligned to `pkgs`.}
#'   \item{avail_pkgs}{The dep-frequency-sorted `available.packages()`
#'     data frame `val_build()` needs to look up repository sources.}
#'   \item{val_dir}{The run's output directory
#'     (`<out>/R_<r_ver>/<YYYYMMDD>`).}
#'   \item{val_date, val_start, r_ver}{Timing / version metadata.}
#'   \item{opt_repos}{Named character vector of the val-date-adjusted
#'     repositories.}
#'   \item{decisions, viable_metrics}{Config snapshots picked up from
#'     `pull_config()`.}
#'   \item{pre_filtered_pkg_metrics}{The full `val_categorize()` output.}
#'   \item{n_candidates}{Number of rows in `pre_filtered_pkg_metrics`
#'     (used later by the summary report).}
#'   \item{toml_path}{Path to the written `pipeline.toml`.}
#' }
#'
#' @seealso [val_pipeline()] for the one-shot wrapper that composes the
#'   prep + build phases, and [write_pipeline_toml()] for the underlying
#'   toml writer.
#'
#' @importFrom dplyr as_tibble filter pull select
#' @importFrom tibble rownames_to_column
#' @importFrom glue glue
#'
#' @export
val_prep_pipeline <- function(
  ref = c("source", "remote"),
  metric_pkg = c("riskmetric", "val.meter", "risk.assessr"),
  deps = c("depends", "suggests")[1],
  deps_recursive = TRUE,
  val_date = Sys.Date(),
  out = Sys.getenv("RISK_OUTPATH", unset = getwd()),
  opt_repos =
    c(CRAN = "https://packagemanager.posit.co/cran/latest",
      BioC = 'https://bioconductor.org/packages/3.22/bioc'),
  verbose = NULL,
  toml_path = NULL,
  toml_project_name = "val.pipeline run",
  config_path = NULL
){

  # Assess args
  ref <- match.arg(ref)
  metric_pkg <- match.arg(metric_pkg)
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))
  apply_verbose(verbose)

  # Route pull_config() at any depth to the user-supplied config, if any.
  old_cfg <- options()["val.pipeline.config_path"]
  on.exit(options(old_cfg), add = TRUE)
  apply_config_path(config_path)

  #
  # ---- Set time variables ----
  #
  r_ver <- getRversion()
  val_start <- Sys.time()
  val_start_txt <- format(val_start, '%Y-%m-%d %H:%M:%S',
                          tz = 'US/Eastern', usetz = TRUE)
  val_date <- as.Date(val_date)
  val_date_txt <- gsub("-", "", val_date)
  val_msg(paste0("\n\n\nValidation prep pipeline initiated: R v", r_ver,
                 " @ ", val_start_txt, "\n\n"),
          min_level = "minimal")

  #
  # ---- Pull config variables ----
  #
  opt_repos <- pull_config(val = "opt_repos", rule_type = "default") |> unlist()
  decisions <- pull_config(val = "decisions_lst", rule_type = "default")

  #
  # ---- Set / capture 'old' options ----
  #
  old <- options()
  on.exit(options(old), add = TRUE)
  options(repos = opt_repos)

  #
  # ---- val_categorize() ----
  #
  viable_metrics <- c("dplyr", "Biobase") |>
    riskmetric::pkg_ref(source = "pkg_cran_remote") |>
    dplyr::as_tibble() |>
    dplyr::filter(!is.na(version)) |>
    riskmetric::pkg_assess() |>
    riskmetric::pkg_score() |>
    dplyr::select(-c(package, version, pkg_ref, pkg_score)) |>
    t() |>
    as.data.frame() |>
    dplyr::filter(!is.na(V1)) |>
    tibble::rownames_to_column(var = "metric") |>
    dplyr::pull(metric)

  if("r_cmd_check" %in% viable_metrics){
    vm <- viable_metrics[which(viable_metrics != "r_cmd_check")]
    viable_metrics <- c(vm, "r_cmd_check_warnings", "r_cmd_check_errors")
  }

  pre_filtered_pkg_metrics <-
    val_categorize(
      source = "riskscore",
      decisions = decisions,
      else_cat = decisions[length(decisions)],
      decisions_df = build_decisions_df(
        rule_type = "remote_reduce",
        viable_metrics = viable_metrics
      )
    )

  #
  # ---- Persist pre-filter set eagerly, alongside all other artifacts ----
  #
  r_dir  <- file.path(out, glue::glue("R_{r_ver}"))
  val_dir <- file.path(r_dir, val_date_txt)
  if (!dir.exists(val_dir)) {
    dir.create(val_dir, recursive = TRUE, showWarnings = FALSE)
  }
  tryCatch(
    saveRDS(pre_filtered_pkg_metrics,
            file.path(val_dir, "pre_filtered_pkg_metrics.rds")),
    error = function(e) {
      warning("Could not persist pre_filtered_pkg_metrics.rds: ",
              conditionMessage(e), call. = FALSE)
    }
  )

  #
  # ---- Update repos option to val_date snapshot ----
  #
  opt_repos <- update_opt_repos(val_date = val_date, opt_repos = opt_repos)
  options(repos = opt_repos, pkgType = "source", scipen = 999)

  #
  # ---- Filter / Reduce pkgs ----
  #
  pass_remote_pkgs <- pull_config(val = "pass_primary", rule_type = "default")

  passed_pkgs <-
    pre_filtered_pkg_metrics |>
    dplyr::filter(
      package %in% pass_remote_pkgs |
      final_risk %in% decisions[1]
    )

  build_pkgs <- passed_pkgs |> dplyr::pull(package)

  val_msg("\n--> Final Decision Category Counts for 'pre' assessment risk: \n----> Returned",
          prettyNum(length(build_pkgs), big.mark = ","),
          "pkgs for build.\n",
          min_level = "minimal")

  #
  # ---- Full dependency-tree resolution ----
  #
  # Delegates to the shared resolve_pkg_tree() helper so the same
  # dep-frequency-sorted `pkgs` / `vers` / `avail_pkgs` triple lands
  # here as in val_build()'s no-prep path.
  tree       <- resolve_pkg_tree(
    pkg_names      = build_pkgs,
    deps           = deps,
    deps_recursive = deps_recursive
  )
  pkgs       <- tree$pkgs
  vers       <- tree$vers
  avail_pkgs <- tree$avail_pkgs

  val_msg("\n-->", length(pkgs), "package(s) resolved for build.\n\n",
          min_level = "minimal")

  #
  # ---- Write pipeline.toml ----
  #
  if (is.null(toml_path)) toml_path <- file.path(val_dir, "pipeline.toml")
  write_pipeline_toml(
    pkgs      = pkgs,
    opt_repos = opt_repos,
    r_version = paste(R.Version()$major, R.Version()$minor, sep = "."),
    name      = toml_project_name,
    path      = toml_path
  )
  val_msg(paste0("\n--> Wrote pipeline toml with ",
                 prettyNum(length(pkgs), big.mark = ","),
                 " pkg(s) to ", toml_path, "\n"),
          min_level = "minimal")

  #
  # ---- Return ----
  #
  structure(
    list(
      pkgs                     = pkgs,
      vers                     = vers,
      avail_pkgs               = avail_pkgs,
      val_dir                  = val_dir,
      val_date                 = val_date,
      val_start                = val_start,
      r_ver                    = r_ver,
      opt_repos                = opt_repos,
      decisions                = decisions,
      viable_metrics           = viable_metrics,
      pre_filtered_pkg_metrics = pre_filtered_pkg_metrics,
      n_candidates             = nrow(pre_filtered_pkg_metrics),
      toml_path                = toml_path
    ),
    class = c("val_prep", "list")
  )
}
