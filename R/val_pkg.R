#' Package Validation
#'
#' Validation process at the package level. Includes steps...
#' 
#' @importFrom glue glue
#' @importFrom utils download.file untar
#' @importFrom riskmetric pkg_ref pkg_assess
#' @importFrom riskreports package_report
#' 
val_pkg <- function(
    pkg,
    ver,
    avail_pkgs,
    ref = "source",
    metric_pkg = "riskmetric",
    out_dir,
    val_date = Sys.Date()
) {
  # i <- 1 # for debugging
  # pkg <- pkgs[i] # for debugging
  # ver <- vers[i] # for debugging
  
  # assess args
  if(!metric_pkg %in% c('risk.assessr', 'riskmetric', 'val.meter')) stop("'metric_pkg' arg must be either 'riskmetric', 'val.meter', or 'risk.assessr' but '", metric_pkg, "' was given.")
  if(!ref %in% c('source', 'remote')) stop("'ref' arg must be either 'source' or 'remote' but '", ref, "' was given.")
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))
  
  pkg_v <- paste(pkg, ver, sep = "_")
  start <- Sys.time()
  start_txt <- format(start, '%H:%M:%S', tz = 'US/Eastern', usetz = TRUE)
  cat(paste0("\n\n\nNew Package: ", pkg, " v", ver," @ ", start_txt,"\n"))
  
  #
  # ---- Setup Dirs ----
  #
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
  
  if(ref == "source") {
    
    #
    # ---- Download Tarball ----
    #
    tarball_url <- paste0("https://cran.r-project.org/src/contrib/", pkg_v,".tar.gz")
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
    
    #
    # ---- Grab Dependencies (using archive)
    #
    # Not being used...
    
    # if(file.exists(tar_file)) {
    #   arch <- archive::archive(file.path(tarballs, glue::glue("{pkg_v}.tar.gz"))) |>
    #     dplyr::arrange(tolower(path))
    # }
    
    # if (file.exists(tar_file)) {
    #   desc_file <- glue::glue("{pkg}/DESCRIPTION")
    #   
    #   tar_con <- archive::archive_read(tar_file, desc_file, format = "tar")
    #   on.exit(close(tar_con))
    #   
    #   desc_con <- desc::description$new(text = readLines(tar_con))
    #   if ('Suggests' %in% desc_con$fields()) {
    #     sug_vctr <- desc_con$get_list(key = 'Suggests') %>% sort()
    #   } else {
    #     msg <- paste("Suggests not found for package", pkg)
    #     rlang::warn(msg)
    #     sug_vctr <- character(0)
    #   }
    # } else {
    #   sug_vctr <- unlist(
    #     tools::package_dependencies(
    #       packages = pkg,
    #       db = meta,
    #       which=c("Suggests"),
    #       recursive=FALSE)) |>
    #     unname() |> sort()
    # }
  }
  
  #
  # ---- Grab Dependencies ----
  #
  # Grab deps (Depends, Imports, LinkingTo, Suggests)
  # pkg_base <- avail_pkgs |> dplyr::filter(Package %in% pkg)
  
  # grab depends
  # depends_base <- pkg_base |>
  #   unite(pkg_deps, c(Depends, Imports, LinkingTo), sep = ", ", na.rm = TRUE) |>
  #   dplyr::pull(pkg_deps)
  # depends0 <- strsplit(depends_base, split = ", ")[[1]]
  # depends <- avail_pkgs |>
  #   dplyr::filter(Package %in% stringr::word(depends0, 1)) |>
  #   dplyr::pull(Package)
  depends <- 
    tools::package_dependencies(
      packages = pkg,
      db = available.packages(),
      which = c("Depends", "Imports", "LinkingTo"),
      recursive = TRUE
    ) |>
    unlist(use.names = FALSE) 
  
  # grab suggests
  # suggests_base <- pkg_base |> dplyr::pull(Suggests)
  # suggests0 <- strsplit(suggests_base, split = ", ")[[1]]
  # suggests <- avail_pkgs |>
  #   dplyr::filter(Package %in% stringr::word(suggests0, 1)) |>
  #   dplyr::pull(Package)
  suggests <- 
    tools::package_dependencies(
      packages = pkg,
      db = available.packages(),
      which = "Suggests",
      recursive = TRUE
    ) |>
    unlist(use.names = FALSE) 
  
  
  # ---- Can Install? ----
  # Make sure we can install cleanly first, else don't with assessment, return meta
  inst_out <- tryCatch({
    if(ref == "source"){
      utils::install.packages(
        file.path(sourced, pkg),
        lib = installed,
        repos = NULL
      )
    } else { # ref == 'remote'
      utils::install.packages(
        pkg,
        lib = installed
      )
    }
  },
  warning = function(w) return(paste("Warning:", w$message)),
  error = function(e) return(paste("Error:", e$message))
  )
  # inst_out
  
  
  clean_install <- if(is.null(inst_out) &
    pkg %in% list.files(installed)) TRUE else FALSE
  
  
  if(!clean_install) {
    # save metadata
    meta_list <- list(
      pkg = pkg,
      ver = ver,
      r_ver = getRversion(),
      sys_info = R.Version(), 
      repos = options("repos"),
      val_date = val_date,
      clean_install = clean_install,
      ref = ref,
      metric_pkg = metric_pkg,
      # metrics = pkg_assessment, # saved separately for {riskreports}
      decision = "High Risk",
      decision_reason = "Failed 'clean_install' step",
      final_decision = "High Risk",
      depends = if(identical(depends, character(0))) NA_character_ else depends,
      suggests = if(identical(suggests, character(0))) NA_character_ else suggests,
      assessment_runtime = list(txt = NA_character_, mins = NA)
    )
    saveRDS(meta_list, file.path(meta_list, glue::glue("{pkg_v}_meta.rds")))
    cat("\n-->", pkg_v,"didn't install cleanly.")
    cat("\n--> Install Output:\n", paste(inst_out, collapse = "\n"),"\n")
    cat("\n-->", pkg_v,"meta bundle saved.\n")
    return(meta_list)
  } else {
    cat("\n-->", pkg_v,"installed cleanly.\n")
  }
  
  
  #
  # ---- Assess ---- 
  #
  #### riskmetric ####
  #
  if(metric_pkg == "riskmetric"){
    
    # Update: can't remove these - will ruin some metrics like
    # code coverage & R CMD Check
    # Remove original docs, if they exist, because they force a
    # user prompt when run interactively
    # inst_doc <- file.path(sourced, pkg, "inst", "doc")
    # if(dir.exists(inst_doc) & interactive()) {
    #   unlink(inst_doc, recursive = TRUE, force = TRUE)
    #   cat("\n-->", pkg_v,"removed original inst/doc.\n")
    # }
    
    #
    ### Create pkg ref ###
    #
    src_ref <- if(ref == "source") 'pkg_source' else 'pkg_cran_remote'
    if(ref == "source") {
      pkg_ref <- riskmetric::pkg_ref(file.path(sourced, pkg), source = src_ref)
    } else { # ref == "remote"
      # remote has no 'prompt' issue when building assessment, but no code covr either
      pkg_ref <- riskmetric::pkg_ref(pkg, source = src_ref)
    } 
    cat("\n-->", pkg_v,"referrenced.\n")
    
    #
    ### Assessment ###
    #
    pkg_assessment0 <- pkg_ref |>
      # dplyr::as_tibble() |> # no tibbles allowed for stip ^ or riskreports
      riskmetric::pkg_assess()
    
    # strip assessment of '.recording' attribute:
    pkg_assessment <-  pkg_assessment0 |> 
      strip_recording()
    
    pkg_score <- riskmetric::pkg_score(pkg_assessment) # What should I do with this?
    
    
    # check some stuff
    # object.size(pkg_assessment0) # 'askpass' 136,744 bytes
    # object.size(pkg_assessment) # 'askpass' 20,896 bytes
    # inherits(pkg_assessment, "list_of_pkg_metric") # works
    # class(pkg_assessment)
    # names(pkg_assessment)
    # attr(pkg_assessment$covr_coverage, "label")
    # pkg_assessment$covr_coverage |> names()
    # pkg_assessment$covr_coverage$totalcoverage
  
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
  ass_mins_txt <- capture.output(assessed_end - start)
  cat("\n-->", pkg_v,"assessed.\n")
  cat("--> (", ass_mins_txt, ")\n")
  
  
  
  
  #
  # ---- Save Assessment---- 
  #
  assessment_file <- file.path(assessed, glue::glue("{pkg_v}_assessments.rds"))
  scores_file <- file.path(assessed, glue::glue("{pkg_v}_scores.rds"))
  saveRDS(pkg_assessment, assessment_file)
  saveRDS(pkg_score, scores_file)
  cat("\n-->", pkg_v,"assessments & scores saved.\n")
  
  
  #
  # ---- Decision (val.criterion) ----
  #
  #
  
  # Use org-level criterion to set thresholds and Update final decision (if not
  # already 'high risk') AND then filter packages to a final 'qualified' list
  #
  # Note: this needs to happen again because (1) we don't have metrics like
  # 'covr_coverage' represented in our pre-filter, plus with have other
  # non-riskmetric assessments, like 'installed_cleanly' we need to consider.
  # (2) Secondly, because val_filter() (our pre-filtering engine) wasn't run on
  # the intended system (aka, {riskscore} OR the PACKAGES) file, so we have to
  # run val_build() & re-filter.
  
  # maybe I should call it pre_filter() & post_filter()
  post_filtered_pkg_metrics <- 
    val_filter(
      pre = FALSE, 
      source = pkg_assessment, # pass in pkg_assessment
      avail_pkgs = available.packages() |> as.data.frame(),
      decisions = c("Low", "Medium", "High"), # need to pass decisions into val_pkg()
      else_cat = "High",
      decisions_df =
        build_decisions_df(
          decision_lst = c("Low", "Medium", "High"),
          
          rules_lst = list(
            downloads_1yr = list(
              cond = list(
                High = ~ is.na(.x),
                High = ~ .x < 120000,
                Medium = ~ dplyr::between(.x, 120000, 240000),
                Low = ~ .x > 240000
              ),
              type = "primary",
              accept_cats = c("Low"),
              min_value = 40000
            ),
            reverse_dependencies = list(
              cond = list(
                High = ~ is.na(.x),
                High = ~ .x < 2,
                Medium = ~ dplyr::between(.x, 2, 7),
                Low = ~ .x > 7
              ),
              type = "exception",
              accept_cats = c("Low", "Medium")
            ),
            dependencies =  list(
              cond = list(
                High = ~ is.na(.x),
                High = ~ .x > 8,
                Medium = ~ dplyr::between(.x, 4, 8),
                Low = ~ .x < 4
              ),
              type = "exception",
              accept_cats = c("Low", "Medium")
            ),
            news_current =  list(
              cond = list(
                High = ~ is.na(.x),
                High = ~ .x != 1,
                Low = ~ .x == 1
              ),
              type = "exception",
              accept_cats = c("Low")
            ),
            # bugs_status = list(
            #   cond = list(
            #       High = ~ is.na(.x) | .x != 1,
            #       Low = ~ .x == 1
            #     ),
            #   type = "exception",
            #   accept_cats = c("Low")
            #   ),
            has_vignettes =  list(
              cond = list(
                High = ~ is.na(.x),
                High = ~ .x < 1,
                Medium = ~ .x == 1,
                Low = ~ .x > 1
              ),
              type = "exception",
              accept_cats = c("Low")
            ),
            has_source_control =  list(
              cond = list(
                High = ~ is.na(.x),
                High = ~ .x == 0,
                Low = ~ .x > 0
              ),
              type = "exception",
              accept_cats = c("Low")
            ),
            has_website =  list(
              cond = list(
                High = ~ is.na(.x),
                High = ~ .x == 0,
                Low = ~ .x > 0
              ),
              type = "exception",
              accept_cats = c("Low")
            )
          )
        )
    )
  # clean_install
  decision <- sample(c(rep("Low Risk",6), rep("Medium Risk", 3), "High Risk"), 1)
  decision_reason = "Decision randomly selected"
  
  #
  # ---- Build Report ----
  #
  # file.edit(system.file("report/package/pkg_template.qmd", package = "riskreports"))
  options(riskreports_output_dir = reports)
  pr <- riskreports::package_report(
    package_name = pkg,
    package_version = ver,
    # template_path = file.path(getwd(), "riskassessment"),
    output_format = "html", #"md", "pdf", "all",
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
    repos = options("repos"),
    val_date = val_date,
    clean_install = clean_install,
    ref = ref,
    metric_pkg = metric_pkg,
    # metrics = pkg_assessment, # saved separately for {riskreports}
    decision = decision,
    decision_reason = decision_reason,
    final_decision = NA_character_, # Will be set later
    depends = if(identical(depends, character(0))) NA_character_ else depends,
    suggests = if(identical(suggests, character(0))) NA_character_ else suggests,
    assessment_runtime = list(txt = ass_mins_txt, mins = ass_mins)
  )
  saveRDS(meta_list, file.path(assessed, glue::glue("{pkg_v}_meta.rds")))
  cat("\n-->", pkg_v,"meta bundle saved.\n")
  
  return(meta_list)
}