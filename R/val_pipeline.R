

#' Validation: Execute an Assessment Pipeline
#'
#' A pipeline to validate R packages using specific metrics and criteria,
#' spelled out in the package's config file. This function orchestrates the
#' reduction of a large set of packages delivered through various sources
#' (either {riskscore} or a user-provided data set) based one primary &
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
#'   'dev/riskassessments'.
#' @param opt_repos Named character vector. Repositories to use. Default is
#'   opt_repos from config.
#' @return A list containing the validation directory and a data frame of
#'   package assessments.
#'
#' @export
#' 
val_pipeline <- function(
  ref = c("source", "remote")[1],
  metric_pkg = "riskmetric", 
  # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  deps = c("depends", "suggests")[1], 
  deps_recursive = TRUE,
  val_date = Sys.Date(),
  replace = FALSE, 
  out = 'dev/riskassessments',
  opt_repos = c(CRAN = paste0("https://packagemanager.posit.co/cran/", Sys.Date()))
  ){

  # Assess args
  if(!metric_pkg %in% c('risk.assessr', 'riskmetric')) stop("'metric_pkg' arg must be either 'riskmetric' or 'risk.assessr' but '", metric_pkg, "' was given.")
  if(!ref %in% c('source', 'remote')) stop("'ref' arg must be either 'source' or 'remote' but '", ref, "' was given.")
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))

  # Since running this script is such a computationally intensive process, the
  # start of this script would actually begin by filtering packages
  # based on pkg downloads, and then we'd feed that list to pkg_names...
  # Eventually, this 'dev' script will become a new function called val_pipeline()
  
  
  
  # filter packages > 20k here (on CRAN alone)
  # Eventually using PACKAGES file
  # For now, will use pkg_cran_remote to gain a
  # High level summary of pkgs
  # --> riskscore PR/ workbench job in progress
  
  # 
  # ---- Set System variables ----
  #
  # store R Version
  r_ver = getRversion()
  
  # Grab val date, output messaging
  val_start <- Sys.time()
  val_start_txt <- format(val_start, '%Y-%m-%d %H:%M:%S', tz = 'US/Eastern', usetz = TRUE)
  
  val_date <-as.Date("2025-10-07") # hardcode for testing
  # val_date <- as.Date(val_start)
  val_date_txt <- gsub("-", "", val_date)
  cat(paste0("\n\n\nValidation pipeline initiated: R v", r_ver, " @ ", val_start_txt,"\n\n"))
  
  
  # 
  # ---- Pull config variables ----
  #
  # For now, let's just filter using cranlogs to determine downloaded pkgs
  # opt_repos = c(val_build_repo = "https://cran.r-project.org") # put in config
  opt_repos <- pull_config(val = "opt_repos", rule_type = "default") |> unlist()
  opt_repos_rr <- pull_config(val = "opt_repos_remote_reduce", rule_type = "default") |> unlist()
  decisions <- pull_config(val = "decisions_lst", rule_type = "default")
  
  
  #
  # ---- Set repos option to risk scores date ----
  #
  old <- options()
  on.exit(function() options(old))
  options(repos = opt_repos_rr, pkgType = "source", scipen = 999) # , rlang_interactive = FALSE
  # options("repos") # verify
  
  
  #
  # ---- val_categorize() ----
  #
  pre_filtered_pkg_metrics <- 
    val_categorize(
      source = "riskscore",
      avail_pkgs = available.packages() |> as.data.frame(),
      decisions = decisions,
      else_cat = decisions[length(decisions)],
      decisions_df = build_decisions_df(rule_type = "remote_reduce")
      )
  # see <-
  #   pre_filtered_pkg_metrics |>
  #     dplyr::filter(dwnlds > 1000000) 
  
  
  
  #
  # ---- Set repos option to today's date ----
  #
  options(repos = opt_repos, pkgType = "source", scipen = 999) # , rlang_interactive = FALSE
  # options("repos") # verify
  
  
  #
  # ---- Reduce pkgs ----
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
  
  # See the full dependency tree before running val_build()
  #
  # these_pkgs <- "withr"  # messes with the entire process
  # these_pkgs <- "matrix" # takes 5 mins to install
  # these_pkgs <- "askpass"
  # these_pkgs <- "codetools"
  # these_pkgs <- build_pkgs
  # tree <- tools::package_dependencies(
  #   packages = these_pkgs,
  #   db = available.packages(),
  #   # which = c("Suggests"),
  #   which = "strong", #c("Depends", "Imports", "LinkingTo"),
  #   # which = c("Depends", "Imports", "LinkingTo", "Suggests"), # prod
  #   recursive = TRUE
  #   # recursive = FALSE
  # ) |>
  #   unlist(use.names = FALSE) |>
  #   unique()
  # # How many? # 621 pkgs -->  When recursive: 2,570. Only 744 when you don't include Suggests
  # full_tree <- c(these_pkgs, tree) |> unique()
  # full_tree |> length()
  
  
  
  # Validation build
  outtie <- val_build(
    # pkg_names = 'rlang',
    # pkg_names = 'askpass', # 2.5 - 3 mins when deps, 2 pkgs, no prompts
    # pkg_names = 'withr',
    # pkg_names = 'codetools',
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
  outtie$val_dir
  qual <- outtie$pkgs_df
  
  # nrow(qual)
  # saveRDS(qual, file.path(outtie$val_dir, paste0("qual_evidence_", val_date_txt, ".rds")))
  
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
  qualified <- assessed |>
    dplyr::filter(final_decision == decisions[1])
  
  # Store as pins board?
  # How to Provision PPM metadata for all pkgs

 return(qualified)
}





