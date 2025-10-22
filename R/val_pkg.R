
#' Validation: Assess a Package
#'
#' Validation process at the package level. Includes steps to download the
#' package source (preferred), install the package, assess the package using the
#' user-specified metric package (only `riskmetric` is supported currently),
#' apply risk decisions, and build a report. Note: to save time, every
#' package will be assessed using a "pkg_cran_remote" reference initially to see
#' if any primary metrics met the "auto-accept" threshold(s), if applicable. If
#' they did, the 'covr_coverage' computation will be skipped.
#'
#' @param pkg Character(1). Name of package to validate.
#' @param ver Character(1). Version of package to validate.
#' @param avail_pkgs Data frame. Output of `available.packages()`.
#' @param ref Character(1). Either "source" or "remote". Indicates whether to
#'   download the package source from CRAN (or other repo) or to install the
#'   package directly from the repo.
#' @param metric_pkg Character(1). Either "riskmetric", "val.meter", or
#'   "risk.assessr" indicating which package to use for the assessment.
#' @param out_dir Character(1). Directory to store outputs.
#' @param val_date Date. Date of validation. Default is current date.
#'
#' @importFrom glue glue
#' @importFrom utils download.file untar capture.output
#' @importFrom riskmetric pkg_ref pkg_assess pkg_score all_assessments
#' @importFrom riskreports package_report
#' @importFrom dplyr filter pull select arrange as_tibble
#' @importFrom tools package_dependencies
#'
#' @return A list containing package metadata, assessment results, and
#'   decisions.
#' @export
#' 
val_pkg <- function(
    pkg,
    ver,
    avail_pkgs,
    ref = c("source", "remote"),
    metric_pkg = c("riskmetric", "val.meter", "risk.assessr"),
    out_dir,
    val_date = Sys.Date()
) {
  # i <- 1 # for debugging
  # pkg <- pkgs[i] # for debugging
  # ver <- vers[i] # for debugging
  
  # assess args
  ref <- match.arg(ref)
  metric_pkg <- match.arg(metric_pkg)
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))
  
  pkg_v <- paste(pkg, ver, sep = "_")
  start <- Sys.time()
  start_txt <- format(start, '%H:%M:%S', tz = 'US/Eastern', usetz = TRUE)
  cat(paste0("\n\n\nNew Package: ", pkg, " v", ver," @ ", start_txt,"\n"))
  
  #
  # ---- Setup ----
  #
  
  # Dirs
  if(ref == "source"){
    tarballs <- file.path(out_dir, 'tarballs')
    sourced <- file.path(out_dir, 'sourced')
    if(!dir.exists(tarballs)) dir.create(tarballs)
    if(!dir.exists(sourced)) dir.create(sourced)
  }
  
  installed <- file.path(out_dir, 'installed')
  assessed <- file.path(out_dir, 'assessed')
  reports <- file.path(out_dir, 'reports')
  if(!dir.exists(installed)) dir.create(installed)
  if(!dir.exists(assessed)) dir.create(assessed)
  if(!dir.exists(reports)) dir.create(reports)
  
  # Where did package come from?
  repo_src_contrib <- avail_pkgs |>
    dplyr::filter(Package %in% pkg) |> 
    dplyr::pull(Repository)  # keep '/src/contrib/` ending
  repo_src <- repo_src_contrib |> dirname() |> dirname() # trim off '/src/contrib'
  repo_name <- get_repo_origin(repo_src = repo_src, pkg_name = pkg)
  
  # Decisions
  decisions <- pull_config(val = "decisions_lst", rule_type = "default")
  
  if(ref == "source") {
    
    #
    # ---- Download Tarball ----
    #
    tarball_url <- file.path(repo_src_contrib, paste0(pkg_v,".tar.gz"))
    dwn_ld <- try(utils::download.file(tarball_url,
                                       file.path(tarballs, basename(tarball_url)), 
                                       quiet = TRUE, mode = "wb"),
                  silent = TRUE)
    if (inherits(dwn_ld, "try-error") | dwn_ld != 0) {
      wrn_msg <- glue::glue("Unable to download the source files for {pkg} from '{tarball_url}'.")
      warning(wrn_msg)
    } else {
      cat("\n-->", pkg_v,"downloaded.\n")
    }
    
    #
    # ---- Untar ---- 
    #
    tar_file <- file.path(tarballs, glue::glue("{pkg_v}.tar.gz"))
    utils::untar(tar_file, exdir = sourced)
    cat("\n-->", pkg_v,"untarred.\n")
  }
  
  
  #
  # ---- Grab Dependencies ----
  #
  
  # Grab deps: Depends, Imports, LinkingTo, Suggests
  # pkg_base <- avail_pkgs |> dplyr::filter(Package %in% pkg)
  
  # grab depends
  depends <- 
    tools::package_dependencies(
      packages = pkg,
      db = avail_pkgs |> as.matrix(),
      which = c("Depends", "Imports", "LinkingTo"),
      recursive = TRUE
    ) |>
    unlist(use.names = FALSE) 
  
  # grab suggests
  suggests <- 
    tools::package_dependencies(
      packages = pkg,
      db = avail_pkgs |> as.matrix(),
      which = "Suggests",
      recursive = TRUE # this really blows up for almost any pkg
    ) |>
    unlist(use.names = FALSE) 
  
  
  #
  # ---- Assess ---- 
  #
  
  #
  #### riskmetric ####
  #
  if(metric_pkg == "riskmetric"){
    
    #
    # Update: can't remove these - will ruin some metrics like
    # code coverage & R CMD Check
    #
    # Remove original docs, if they exist, because they force a
    # user prompt when run interactively
    # inst_doc <- file.path(sourced, pkg, "inst", "doc")
    # if(dir.exists(inst_doc) & interactive()) {
    #   unlink(inst_doc, recursive = TRUE, force = TRUE)
    #   cat("\n-->", pkg_v,"removed original inst/doc.\n")
    # }
    
    #
    ### Initial Assessment ###
    #
    
    # We will ALWAYS perform a "pkg_cran_remote" assessment first, because it is
    # WAY faster (even faster than a "pkg_source" while excluding
    # "covr_coverage") so that if any primary metrics have an auto_accept
    # condition, we can then run again with a "pkg_source" ref while excluding
    # "covr_coverage" for final output. However, 
    init_pkg_ref <- riskmetric::pkg_ref(pkg, source = 
        if(stringr::str_detect(tolower(repo_name), "bioc")) "pkg_bioc_remote" else "pkg_cran_remote")
    cat("\n-->", pkg_v, "initial reference complete.\n")
    
    # Pull available {riskmetric} assessments
    init_metrics <- riskmetric::all_assessments()
    
    # if it's a 'remote_only' pkg, and we only want to assess primary metrics,
    # then we could do that here (below). For now, we'll leave it all since
    # we'll want a report that is the most populated as possible
    # remote_pkgs <- pull_config(val = "remote_only", rule_type = "default")
    # if(pkg %in% remote_pkgs) {
    #   # pull primary metrics only
    #   prime_metrics <- build_decisions_df(rule_type = "decide") |>
    #     dplyr::filter(tolower(metric_type) == "primary") |>
    #     dplyr::pull(metric) |>
    #     unique() %>%
    #     paste("assess", ., sep = "_")
    #   init_metrics <- init_metrics[names(init_metrics) %in% prime_metrics] 
    # }
    
    # rm(pkg_assessment0)
    init_pkg_assessment0 <-
      init_pkg_ref |>
      # dplyr::as_tibble() |> # no tibbles allowed for stip or riskreports
      riskmetric::pkg_assess(assessments = init_metrics)
    
    # strip assessment of '.recording' attribute:
    init_pkg_assessment <-
      init_pkg_assessment0 |> 
      strip_recording()
    
    init_pkg_scores <- riskmetric::pkg_score(init_pkg_assessment)
    
    init_assessed_end <- Sys.time()
    init_ass_mins <- difftime(init_assessed_end, start, units = "mins")
    init_ass_mins_txt <- utils::capture.output(init_assessed_end - start)
    cat("\n-->", pkg_v, "initial assessment complete.\n")
    cat("----> (", init_ass_mins_txt, ")\n")
    
    
    # 
    #### Initial Decision
    #
    init_viable_metrics <- init_pkg_scores |>
      dplyr::as_tibble() |>
      t() |>
      as.data.frame() |>
      dplyr::filter(!is.na(V1)) |>
      # make rownames a column
      tibble::rownames_to_column(var = "metric") |>
      dplyr::pull(metric)
    
    if("r_cmd_check" %in% init_viable_metrics){
      init_vm <- init_viable_metrics[which(init_viable_metrics != "r_cmd_check")]
      init_viable_metrics <- c(init_vm, "r_cmd_check_warnings", "r_cmd_check_errors")
    }
    
    # pkg_assessment$downloads_1yr |> prettyNum(big.mark = ",")
    init_decision <- 
      val_decision( 
        pkg = pkg,
        source = list(assessment = init_pkg_assessment, scores = init_pkg_scores), # include both
        excl_metrics = NULL, # "covr_coverage", # Subset not really necessary
        decisions = decisions,
        else_cat = decisions[length(decisions)],
        decisions_df = build_decisions_df(
          rule_type = "decide",
          viable_metrics = init_viable_metrics
          )
      )
    
    auto_accepted <-
      init_decision |> 
      # dplyr::select(package, final_risk, dplyr::ends_with("cataa"))
      dplyr::select(dplyr::ends_with("cataa")) |>
      as.vector() |> unlist() |> any()

    # Should I also consider an auto_fail threshold?
    
    
    #
    #### Final Assessment ###
    #
    src_ref <- if(ref == "source") 'pkg_source' else 'pkg_cran_remote'
    if(src_ref == "pkg_cran_remote") {
      pkg_assessment <- init_pkg_assessment
      pkg_scores <- init_pkg_scores
      cat("\n-->", pkg_v, "used initial 'pkg_cran_remote' assessment.\n")
      exclude_met <- NULL
    } else {
      
      # Setup 'pkg_source' reference
      pkg_ref <- riskmetric::pkg_ref(file.path(sourced, pkg), source = "pkg_source")
      cat("\n-->", pkg_v,"referrenced w/ 'pkg_source'.\n")
      
      # Pull available {riskmetric} assessments
      assess_metrics <- riskmetric::all_assessments()
      
      if (auto_accepted) {
        # Run assessment WITHOUT "covr_coverage"!
        cat("\n-->", pkg_v, "auto-accepted. Will compile final 'pkg_source' assessment w/o 'covr_coverage' metric to save compute time.\n")
        assess_metrics$assess_covr_coverage <- NULL
        exclude_met <- "covr_coverage"
        
      } else {
        # Run assessment WITH "covr_coverage"
        cat("\n-->", pkg_v, "was NOT auto-accepted. Will compile final 'pkg_source' assessment, including 'covr_coverage' metric.\n")
        exclude_met <- NULL
      }
      
      pkg_assessment0 <- pkg_ref |>
        # dplyr::as_tibble() |> # no tibbles allowed for stip or riskreports
        riskmetric::pkg_assess(assessments = assess_metrics)
      
      # strip assessment of '.recording' attribute:
      pkg_assessment <-  pkg_assessment0 |> 
        strip_recording()
      
      pkg_scores <- riskmetric::pkg_score(pkg_assessment)
      
      # Clean up any new folders created in the working directory that end in '-tests'
      # this is an unfortunate by-produce of riskmetric's processes
      wd_dirs <- list.dirs(getwd(),recursive = FALSE)
      # Find which dirs end in "-test"
      pkg_test_dir <- wd_dirs[grepl("-tests$", wd_dirs)]
      unlink(pkg_test_dir, recursive = TRUE, force = TRUE)
    }
    
  
  #
  #### risk.assessr ####
  #
  } else if(metric_pkg == "risk.assessr") {
    requireNamespace("risk.assessr", quietly = TRUE)
    if(ref == "source") {
      pkg_assessment <- risk.assessr::risk_assess_pkg(tar_file)
      # names(pkg_assessment)
      # pkg_assessment$results
      # pkg_assessment$results$dep # doesn't mention lattice...
      # (pkg_assessment$results$download$last_month_download * 12) |> prettyNum(big.mark = ",")
      # pkg_assessment$covr_list # failed
      # pkg_assessment$tm        # failed
      # pkg_assessment$check_list # appears to depend on pkgs installed locally
    } else {
      pkg_assessment <- risk.assessr::assess_pkg_r_package(pkg, ver)
      # names(pkg_assessment0)
      # pkg_assessment0$results
      # pkg_assessment0$results$dep 
      # (pkg_assessment0$results$download$last_month_download * 12) |> prettyNum(big.mark = ",")
      # pkg_assessment0$covr_list 
      # pkg_assessment0$tm        
      # pkg_assessment0$check_list 
    }
    
  #
  #### val.meter ####
  #
  } else if(metric_pkg == "val.meter") {
    stop("Not yet implemented: val_pkg() using 'val.meter' tooling")
  } # no else since we assert metric_pkg values at top of val_pkg().
  
  
  assessed_end <- Sys.time()
  ass_mins <- difftime(assessed_end, start, units = "mins")
  ass_mins_txt <- utils::capture.output(assessed_end - start)
  cat("\n-->", pkg_v,"assessed.\n")
  cat("----> (", ass_mins_txt, ")\n")
  
  
  
  
  #
  # ---- Save Assessment---- 
  #
  
  assessment_file <- file.path(assessed, glue::glue("{pkg_v}_assessments.rds"))
  scores_file <- file.path(assessed, glue::glue("{pkg_v}_scores.rds"))
  # pkg_assessment <- readRDS(assessment_file) # for debugging
  # pkg_scores <- readRDS(scores_file) # for debugging
  saveRDS(pkg_assessment, assessment_file)
  saveRDS(pkg_scores, scores_file)
  cat("\n-->", pkg_v,"assessments & scores saved.\n")
  
  
  
  #
  # ---- Apply Decisions ----
  #
  
  cat("\n--> Making a risk decision for", pkg_v,"...\n\n")
  
  # Use org-level criterion to set thresholds and Update final decision (if not
  # already 'high risk') AND then filter packages to a final 'qualified' list
  #
  # Note: this needs to happen again because (1) we don't have metrics like
  # 'covr_coverage' represented in our pre-filter, plus with have other
  # non-riskmetric assessments, like 'installed_cleanly' we need to consider.
  # (2) Secondly, because val_filter() (our pre-filtering engine) wasn't run on
  # the intended system (aka, {riskscore} OR the PACKAGES) file, so we have to
  # run val_build() & re-filter.
  
  # if need to read in an assessment:
  # pkg_assessment <- readRDS(assessment_file)
  # pkg_scores <- readRDS(scores_file)
  
  # riskmetric doesn't pick up certain metrics for pkg_ref(source = "pkg_cran_remote")
  # What metrics do we need to remove for the decisioning process?
  viable_metrics <- pkg_scores |>
    dplyr::as_tibble() |>
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
  
  decision <- 
    val_decision( 
      pkg = pkg,
      source = list(assessment = pkg_assessment, scores = pkg_scores), 
      excl_metrics = exclude_met, # Subset if desired
      decisions = decisions,
      else_cat = decisions[length(decisions)],
      avail_pkgs = avail_pkgs,
      decisions_df = build_decisions_df(
        rule_type = "decide",
        viable_metrics = viable_metrics
        )
    )
  decision_aa <- decision |>
    dplyr::select(dplyr::ends_with("cataa")) |>
    as.vector() |> unlist() |> any()
  
  if(decision_aa) {
    approved_pkgs <- pull_config(val = "approved_pkgs", rule_type = "default")
    aa_metrics <- decision |>
      dplyr::select(dplyr::ends_with("cataa")) |>
      names() %>%
      gsub("_cataa", "", .)

    decision_reason <- dplyr::case_when(
      pkg %in% approved_pkgs ~ "Pre-Approved package",
      length(aa_metrics) > 0 ~ paste("Met auto-accepted metric threshold(s) for:", paste(aa_metrics, collapse = ", ")),
      TRUE ~ "Risk Assessment"
    ) 
  } else {
    decision_reason <- "Risk Assessment"
  }
  
  cat("\n-->", pkg_v,"decision reason:\n---->", decision_reason, "\n")
  
  #
  # ---- Build Report ----
  #
  
  # file.edit(system.file("report/package/pkg_template.qmd", package = "riskreports"))
  options(riskreports_output_dir = reports)
  pr <- riskreports::package_report(
    package_name = pkg,
    package_version = ver,
    # template_path = file.path(getwd(), "riskassessment"),
    output_format = "typst", #"html", #"md", "pdf", "all",
    # params list: https://github.com/pharmaR/riskreports/blob/main/inst/report/package/pkg_template.qmd
    params = list(
      assessment_path = assessment_file,
      hide_reverse_deps = 'false',
      source = src_ref # defined above
    ),
    quiet = TRUE # To silence quarto output for readability
  )
  # pr
  
  cat("\n-->", pkg_v,"Report built.\n")
  
  
  #
  # ---- Save Pkg Meta Bundle ---- 
  #
  
  # Save a list of items beyond the assessment values
  meta_list <- list(
    pkg = pkg,
    ver = ver,
    r_ver = getRversion(),
    sys_info = R.Version(),
    repos = repo_name, # A named character
    val_date = val_date,
    ref = ref,
    metric_pkg = metric_pkg,
    # metrics = pkg_assessment, # saved separately for {riskreports}
    decision = decision$final_risk,
    decision_reason = decision_reason,
    final_decision = NA_character_, # Will be set later
    final_decision_reason = NA_character_, # Will be set later
    depends = if(identical(depends, character(0))) NA_character_ else depends,
    suggests = if(identical(suggests, character(0))) NA_character_ else suggests,
    rev_deps = if(is.null(pkg_assessment$reverse_dependencies)) NA_character_ else pkg_assessment$reverse_dependencies |> as.vector(),
    assessment_runtime = list(txt = ass_mins_txt, mins = ass_mins)
  )
  # meta_list <- readRDS(file.path(assessed, glue::glue("{pkg_v}_meta.rds")))
  # meta_list$rev_deps
  saveRDS(meta_list, file.path(assessed, glue::glue("{pkg_v}_meta.rds")))
  cat("\n-->", pkg_v,"meta bundle saved.\n")
  
  return(meta_list)
}


