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
  if(!ref %in% c('risk.assessr', 'riskmetric')) stop("'metric_pkg' arg must be either 'riskmetric' or 'risk.assessr' but '", metric_pkg, "' was given.")
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
  pkg_base <- avail_pkgs |> dplyr::filter(Package %in% pkg)
  
  # grab depends
  depends_base <- pkg_base |>
    unite(pkg_deps, c(Depends, Imports, LinkingTo), sep = ", ", na.rm = TRUE) |>
    dplyr::pull(pkg_deps)
  depends0 <- strsplit(depends_base, split = ", ")[[1]]
  depends <- avail_pkgs |>
    dplyr::filter(Package %in% stringr::word(depends0, 1)) |>
    dplyr::pull(Package)
  
  # grab suggests
  suggests_base <- pkg_base |> dplyr::pull(Suggests)
  suggests0 <- strsplit(suggests_base, split = ", ")[[1]]
  suggests <- avail_pkgs |>
    dplyr::filter(Package %in% stringr::word(suggests0, 1)) |>
    dplyr::pull(Package)
  
  
  # ---- Can Install? ----
  # Make sure we can install cleanly first, else don't with assessment, return meta
   FALSE

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
      sys_info = R.version,
      repos = options("repos"),
      val_date = val_date,
      clean_install = clean_install,
      ref = ref,
      metric_pkg = metric_pkg,
      # metrics = pkg_assessment, # saved separately for {riskreports}
      decision = "High Risk",
      final_decision = "High Risk",
      depends = depends, # populate this from avail
      suggests = suggests,
      assessment_runtime = list(txt = ass_mins_txt, mins = ass_mins)
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
  if(metric_pkg == "riskmetric"){
    if(ref == "source") {
      pkg_ref <- riskmetric::pkg_ref(file.path(sourced, pkg), source = "pkg_source")
    } else { # ref == "remote"
      # remote has no 'prompt' issue when building assessment, but no code covr either
      pkg_ref <- riskmetric::pkg_ref(pkg, source = "pkg_cran_remote")
    } 
    cat("\n-->", pkg_v,"referrenced.\n")
    
    # Do I need to pull a 'pkg_cran_remote' assessment here as well?
    pkg_assessment <- riskmetric::pkg_assess(pkg_ref)
    # pkg_assessment$covr_coverage$totalcoverage
    # covr <- riskmetric::assess_covr_coverage(pkg_ref)
    
  } else if(metric_pkg == "risk.assessr") {
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
  }
  
  assessed_end <- Sys.time()
  ass_mins <- difftime(assessed_end, start, units = "mins")
  ass_mins_txt <- capture.output(assessed_end - start)
  cat("\n-->", pkg_v,"assessed.\n")
  cat("--> (", ass_mins_txt, ")\n")
  
  #
  # ---- Save Assessment---- 
  #
  assessment_file <- file.path(assessed, glue::glue("{pkg_v}_assessments.rds"))
  saveRDS(pkg_assessment, assessment_file)
  cat("\n-->", pkg_v,"assessments saved.\n")
  
  #
  # ---- Decision ----
  #
  # Make a risk decision for the package threshold criteria we set
  decision <- sample(c(rep("Low Risk",6), rep("Medium Risk", 3), "High Risk"), 1)
  
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
    # params: https://github.com/pharmaR/riskreports/blob/main/inst/report/package/pkg_template.qmd
    params = list(
      assessment_path = assessment_file,
      hide_reverse_deps = 'false'
    ),
    quiet = TRUE # To silence quarto output for readability
  )
  pr
  
  cat("\n-->", pkg_v,"Report built.\n")
  
  #
  # ---- Save Pkg Meta Bundle ---- 
  #
  # Save a list of items beyond the assessment values
  meta_list <- list(
    pkg = pkg,
    ver = ver,
    r_ver = getRversion(),
    sys_info = R.version,
    repos = options("repos"),
    val_date = val_date,
    clean_install = clean_install,
    ref = ref,
    metric_pkg = metric_pkg,
    # metrics = pkg_assessment, # saved separately for {riskreports}
    decision = decision,
    final_decision = NULL, # Will be set later
    depends = depends, 
    suggests = suggests,
    assessment_runtime = list(txt = ass_mins_txt, mins = ass_mins)
  )
  saveRDS(meta_list, file.path(assessed, glue::glue("{pkg_v}_meta.rds")))
  cat("\n-->", pkg_v,"meta bundle saved.\n")
  
  return(meta_list)
}