

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
      BioC = 'https://bioconductor.org/packages/3.21/bioc')
  ){

  # Assess args
  ref <- match.arg(ref)
  metric_pkg <- match.arg(metric_pkg)
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))

  # Since running this script is such a computationally intensive process, the
  # start of this script would actually begin by filtering packages
  # based on primarily pkg downloads, and then we'd feed that list to pkg_names.
  
  
  
  # 
  # ---- Set time variables ----
  #
  # store R Version
  r_ver = getRversion()
  
  # Grab val date, output messaging
  val_start <- Sys.time()
  val_start_txt <- format(val_start, '%Y-%m-%d %H:%M:%S', tz = 'US/Eastern', usetz = TRUE)
  
  # val_date <-as.Date("2025-10-07") # hardcode for testing
  # val_date <- as.Date(val_start)
  val_date_txt <- gsub("-", "", val_date)
  cat(paste0("\n\n\nValidation pipeline initiated: R v", r_ver, " @ ", val_start_txt,"\n\n"))
  
  
  # 
  # ---- Pull config variables ----
  #
  # For now, let's just filter using cranlogs to determine downloaded pkgs
  # opt_repos = c(val_build_repo = "https://cran.r-project.org") # put in config
  opt_repos <- pull_config(val = "opt_repos", rule_type = "default") |> unlist()
  decisions <- pull_config(val = "decisions_lst", rule_type = "default")
  
  
  
  #
  # ---- Set capture 'old' options ----
  #
  old <- options()
  on.exit(function() options(old))
  
  # set the options
  options(repos = opt_repos)
  # options('repos')
  
  #
  # ---- val_categorize() ----
  #.
  
  # Assess the 'dplyr' pkg to identify which metrics are available for
  # 'pkg_cran_remote' Need one pkg from CRAN & one from BioConductor in case our
  # config only specifies one.
  viable_metrics <- c("dplyr", "Biobase") |>
    riskmetric::pkg_ref(source = "pkg_cran_remote") |>
    dplyr::as_tibble() |>
    dplyr::filter(!is.na(version)) |> # remove either pkg if not found
    riskmetric::pkg_assess() |>
    riskmetric::pkg_score() |>
    dplyr::select(-c(package, version, pkg_ref, pkg_score)) |>
    t() |>
    as.data.frame() |>
    dplyr::filter(!is.na(V1)) |>
    # make rownames a column
    tibble::rownames_to_column(var = "metric") |>
    dplyr::pull(metric)
  
  if("r_cmd_check" %in% viable_metrics){
    vm <- viable_metrics[which(viable_metrics != "r_cmd_check")]
    viable_metrics <- c(vm, "r_cmd_check_warnings", "r_cmd_check_errors")
  }
  
  # "filter" packages 
  # > 22k here (on CRAN alone). Eventually, want to use PACKAGES file here For
  # now, will use pkg_cran_remote via {riskscore} to gain a  High level summary
  # of pkgs
  
  pre_filtered_pkg_metrics <- 
    val_categorize(
      source = "riskscore",
      # avail_pkgs = available.packages() |> as.data.frame(),
      decisions = decisions,
      else_cat = decisions[length(decisions)],
      decisions_df = build_decisions_df(
        rule_type = "remote_reduce",
        viable_metrics = viable_metrics
        )
      )
  # see <- pre_filtered_pkg_metrics |> dplyr::filter(dwnlds > 1000000) 
  
  
  
  #
  # ---- Set repos option to val_date date if needed ----
  #
  
  opt_repos <- update_opt_repos(val_date = val_date, opt_repos = opt_repos)
  options(repos = opt_repos, pkgType = "source", scipen = 999)
    # options("repos") # verify
  
  
  #
  # ---- Filter / Reduce pkgs ----
  #
  
  # Note: has to be decisions[1] ("Low") only because of the way we allowed
  # 'High' risk pkgs to get promoted to "Medium" in `pre_filtered_pkg_metrics`.
  # Specifically, a 'High' Risk pkg could have a severly low annual downloads #.
  
  # Currently, not being used
  failed_pkgs <-
    pre_filtered_pkg_metrics |>
    dplyr::filter(!final_risk %in% decisions[1])
  # dplyr::filter(rev_deps > 100) 
  # dplyr::filter(dwnlds > 120000) 
  
  passed_pkgs <-
    pre_filtered_pkg_metrics |>
    dplyr::filter(final_risk %in% decisions[1])
  
    
  build_pkgs <- passed_pkgs |>
    dplyr::pull(package)
  
  
  cat("\n--> Final Decision Category Counts for'pre' assessment risk: \n----> Returned", prettyNum(length(build_pkgs), big.mark = ","), "pkgs for build.\n")
  
  
  
  
  #
  # ---- val_build() ----
  #
  
  # Validation build
  outtie <- val_build(
    pkg_names = build_pkgs, # Not sorted
    
    # everything else
    ref = ref, 
    metric_pkg = metric_pkg,
    deps = deps,
    deps_recursive = deps_recursive,
    val_date = val_date, 
    replace = replace,
    out = out,
    opt_repos = opt_repos
  )
  
  
  
  #
  # ---- Inspect outputs ----
  #
  
  qual <- outtie$pkgs_df
  
  # nrow(qual)
  saveRDS(qual, file.path(outtie$val_dir, paste0("qual_evidence_", val_date_txt, ".rds")))
  
  # # Inspect the assessment dir
  # # valdate <- gsub("-", "", Sys.Date())
  # valdate <- "20250731"
  # val_dir <- file.path(
  #   "dev/riskassessments",
  #   glue::glue('R_{getRversion()}'),
  #   valdate
  #   )
  # assessed <- file.path(val_dir, "assessed")
  # meta_files <- list.files(assessed, pattern = "_meta.rds$")
  # ass_files <- list.files(assessed, pattern = "_assessments.rds$")
  # 
  # # choose a pkg
  # pkg_name <- "zoo"
  # meta_pkg <- meta_files[stringr::str_detect(meta_files, pkg_name)]
  # meta <- readRDS(file.path(assessed, meta_pkg)) 
  # ass_pkg <- ass_files[stringr::str_detect(ass_files, pkg_name)]
  # ass <- readRDS(file.path(assessed, ass_pkg)) 
  # 
  # # explore outputs
  # meta
  # names(ass)
  # ass$covr_coverage$totalcoverage
  # ass$downloads_1yr |> prettyNum(big.mark = ",")
  
  # 
  # # val_build(pkg_names = c('aamatch'), deps = NULL) # No coverage
  
  
  
  
  
  #
  # ---- Wrap up ----
  #
  # determine qualified pkgs to provision for PPM
  # qualified <- qual |>
  #   dplyr::filter(final_decision == decisions[1])
  
  # Store as pins board?
  # How to Provision PPM metadata for all pkgs

 return(qual)
}





