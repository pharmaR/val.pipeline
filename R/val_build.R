
#' Validation: Build an Assessment Co-hort
#'
#' Build a risk assessment validation for a set of R packages from various
#' sources (CRAN / Bioconductor / GitHub), with the ability to include
#' (optionally recursive) dependencies and suggests, and save the results in a
#' structured directory. The cherry on top is that this build will use logic
#' from val_decision() to not only apply risk decisions too all packages
#' assessed, but goes back around and will re-categorize decisions based on
#' whether any dependencies were categorized as "High Risk" / "Rejected". It
#' also is intelligent enough to sort the list of packages to run those with the
#' most dependencies first, so that if a package fails, it doesn't waste any
#' time running it's reverse dependence. After the pipeline applies a decision
#' onto each package using criteria provided in a config file, it even generates
#' a report detailing specifics of the assessment as supporting evidence. The
#' end result is a directory containing the assessment results and reports for
#' each package evaluated.
#'
#' @param pkg_names Character vector of package names to assess. If NULL
#'   (default), all packages available from the specified repository will be
#'   assessed.
#' @param ref Character string indicating the source of the packages to assess.
#'   Either "source" (default) for source packages, or "remote" for packages
#'   from remote repositories like CRAN/Bioconductor.
#' @param metric_pkg Character string specifying the risk assessment package to
#'   use. Either "riskmetric" (default) & or "val.meter" (not implemented yet).
#' @param deps Character vector specifying which types of dependencies to
#'   include in the assessment. Options are "depends", "suggests", or both
#'   (default). If NULL, only the specified packages from 'pkg_names' will be
#'   assessed without their dependencies.
#' @param deps_recursive Logical indicating whether to include dependencies
#'   recursively. Default is TRUE.
#' @param val_date Date object or character string representing the date of the
#'   validation build. Default is the current date (Sys.Date()).
#' @param out Character string specifying the output directory for the
#'   validation build. Default is "riskassessment" in the current working
#'   directory.
#' @param replace Logical indicating whether to replace existing assessments for
#'   packages that have already been assessed. Default is FALSE.
#' @param opt_repos Named character vector specifying the repository options for
#'   package installation. Default is CRAN.
#'
#' @importFrom glue glue
#' @importFrom tidyr unite
#' @importFrom dplyr filter pull mutate case_when as_tibble bind_rows
#' @importFrom purrr map2 set_names reduce map map_lgl list_flatten
#' @importFrom stringr word
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages capture.output
#'
#' @return A list containing:
#' - val_dir: The directory where the validation build results are stored.
#' - pkg_meta: A data frame summarizing the risk assessment results for all 
#'   packages assessed, including their dependencies and final risk decisions.
#' - pkg_assess: A data frame containing detailed (`riskmetric`) assessment
#'   records for each package.
#'
#' @export
#' 
val_build <- function(
    pkg_names = NULL, #
    ref = c("source", "remote"),
    metric_pkg = c("riskmetric", "val.meter", "risk.assessr"),
    deps = c("depends", "suggests")[1], # deps = c("depends"), deps = NULL
    deps_recursive = TRUE,
    val_date = Sys.Date(),
    out = 'riskassessment',
    replace = FALSE,
    opt_repos = 
    c(CRAN = "https://packagemanager.posit.co/cran/latest",
      BioC = 'https://bioconductor.org/packages/3.21/bioc')
    ){
  
  #
  # Quick Param Run
  #
  
  # ref = "source" # default
  # # ref = "remote",
  # metric_pkg = "riskmetric" # default
  # # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  # # deps = c("depends", "suggests")
  # deps = "depends"  # default
  # # deps = NULL
  # # deps_recursive = FALSE
  # deps_recursive = TRUE # default
  # val_date = Sys.Date() # Sys.Date() # is  default
  # replace = FALSE # default
  # # replace = TRUE
  # out = Sys.getenv("RISK_OUTPATH", unset = getwd())
  # opt_repos = opt_repos
  
  # Assess args
  ref <- match.arg(ref)
  metric_pkg <- match.arg(metric_pkg)
  stopifnot(inherits(as.Date(val_date), c("Date", "POSIXt")))
  
  # store R Version
  r_ver = getRversion()
  
  # Grab val date, output messaging
  val_start <- Sys.time()
  val_start_txt <- format(val_start, '%Y-%m-%d %H:%M:%S', tz = 'US/Eastern', usetz = TRUE)
  val_date <- as.Date(val_date)
  val_date_txt <- gsub("-", "", val_date)
  cat(paste0("\n\n\nNew Validation build: R v", r_ver, " @ ", val_start_txt,"\n\n"))
  
  
  #
  # ---- Setup ----
  #
  
  # Pull in some config variables
  decisions <- pull_config(val = "decisions_lst", rule_type = "default")
  remote_pkgs <- pull_config(val = "remote_only", rule_type = "default")
  # opt_repos <- pull_config(val = "opt_repos", rule_type = "default") |> unlist()
  
  old <- options()
  on.exit(function() options(old))
  if(ref == 'source') {
    options(repos = opt_repos, pkgType = "source") # , rlang_interactive = FALSE
  } else {
    options(repos = opt_repos) # , rlang_interactive = FALSE
  }
  # options("repos")
  
  #
  # ---- Which pkgs, ordered ----
  #
  
  # Make available.packages into a data.frame
  avail_pkgs <- available.packages() |> as.data.frame()
  
  # grab the packages we need
  if(is.null(pkg_names)) {
    # if no pkgs_names give, assume we are supposed to analyze all pkgs available
    pkgs <- avail_pkgs$Package
    
  } else {
    # if pkg_names is provided, then we need to 
    if(is.null(deps)) {
      # if no deps given, then just use the pkgs provided
      full_dep_tree <- pkg_names
      
    } else {
      # If deps are provided, then we need to get the dependencies
      # and add them to the list of pkgs to assess
      
      deps_low <- tolower(deps)
      which_deps <- dplyr::case_when(
        all(c("depends", "suggests") %in% deps_low) ~ "most",
        deps_low == "depends" ~ "strong",
        deps_low == "suggests" ~ "Suggests",
        .default = NULL)[1]
      
      # check that the which_deps is valid
      if(!which_deps %in% c("most", "strong", "Suggests")) stop("problem with 'which_deps'")
      
      dep_tree <- tools::package_dependencies(
        packages = pkg_names,
        # db = available.packages(),
        which = which_deps,
        # recursive = TRUE) # For debugging
        recursive = deps_recursive)
       
      # sort avail_pkgs based on how often a pkg appears in the dep_tree
      # if a pkgs is observed as a dependency often, we'd want to run an
      # assessment on those pkgs first! Why? Because if it fails, then we
      # can avoid running assessments on pkgs that depend on it.
      pkg_freqs <- dep_tree |> unlist(use.names = FALSE) |> table()
      
      avail_pkgs <- avail_pkgs |>
        dplyr::mutate(dep_freq = pkg_freqs[Package]) |>
        dplyr::mutate(dep_freq = ifelse(is.na(dep_freq), 0, dep_freq)) |>
        dplyr::arrange(dplyr::desc(dep_freq), Package) #|>
        # dplyr::select(-dep_freq) # keep it
      
      full_dep_tree <- dep_tree |>
        unlist(use.names = FALSE) |> 
        c(names(dep_tree)) |>
        unique() |>
        sort()
    }
    
    # select pkgs FROM avail_pkgs so that the pkg names and versions are aligned
    # i.e. in the correct order
    pkgs <-
      avail_pkgs |>
      dplyr::filter(Package %in% full_dep_tree) |>
      dplyr::pull(Package)
  }
  vers <- avail_pkgs |>
    dplyr::filter(Package %in% pkgs) |>
    dplyr::pull(Version)
  pkgs_length <- length(pkgs)
  cat("\n-->", pkgs_length, "package(s) to process.\n\n")
  
  
  # Prompt the user to confirm they want to continue when assessing a lot of pkgs
  if(interactive() & pkgs_length >= 10) {
    message("Wow, looks like there is more than 10 pkgs to assess. That could take a while. Do you want to continue?")
    continue <- readline(prompt = "Continue: Y/N?")
    if(tolower(continue) == 'n') stop("User chose to stop the validation build.")
  }
  
  
  #
  # ---- Define dirs ----
  #
  
  r_dir <- file.path(out, glue::glue('R_{r_ver}'))
  val_dir <- file.path(r_dir, val_date_txt)
  assessed <- file.path(val_dir, 'assessed') # needed
  
  # create dirs if they don't exist
  if(!dir.exists(out)) dir.create(out)
  if(!dir.exists(r_dir)) dir.create(r_dir)
  if(!dir.exists(val_dir)) dir.create(val_dir)
  if(!dir.exists(assessed)) dir.create(assessed) # needed
  
  
  
  #
  # ---- Build pkg bundles ----
  #
  
  # Initiate a list to store pkgs that include the reverse dependencies of pkgs
  # that have failed
  dont_run <- character(0)
  
  
  # Start bundling
  pkg_bundles <- purrr::map2(pkgs, vers, function(pkg, ver){
    
    # i <- 1 # for debugging
    # pkg <- pkgs[i] # for debugging
    # ver <- vers[i] # for debugging
    
    pkg_v <- paste(pkg, ver, sep = "_")
    pkg_meta_file <- file.path(assessed, glue::glue("{pkg_v}_meta.rds"))
    
    # prevent running pkgs that depend on pkgs that have already failed
    if(!(pkg %in% dont_run)) {
    
      # check to make sure the pkg bundle doesn't already exist. If so, we can
      # skip building the bundle, but we still need to assess it's dependencies
      if(!file.exists(pkg_meta_file) | replace) {
        pkg_meta <- val_pkg(
          pkg = pkg,
          ver = ver,
          avail_pkgs = avail_pkgs,
          ref = if(pkg %in% remote_pkgs) 'remote' else ref,
          metric_pkg = metric_pkg,
          out_dir = val_dir,
          val_date = val_date)
      } else {
        cat(paste0("\n\n\nAttempted New Package: ", pkg, " v", ver,", but already assessed.\n\n"))
        pkg_meta <- readRDS(pkg_meta_file)
        
        cat("\n-->", pkg_v,"Using assessment previously stored.\n")
      }
      
      # if a pkg fails, make sure it's reverse dependencies don't run an assessment
      # if(pkg == "sys") pkg_meta$decision = "Medium" # for debugging
      if(pkg_meta$decision != decisions[1]) {
        cat(paste0("\n\n--> ", pkg, " v", ver," was assessed with a '", pkg_meta$decision,"' risk. All packages that depend on it will also be marked as '", decisions[length(decisions)],"' risk.\n\n"))
        dont_run <<- c(dont_run, pkg_meta$rev_deps) |> unique()
      }
      
    } else {
      # ---- Pkg is in 'dont_run'! ----
      cat(paste0("\n\n\nAttempted New Package: ", pkg, " v", ver,", but one of it's dependencies already failed so skipping assessment and marking risk as '", decisions[length(decisions)], "'.\n\n"))
      
      # grab depends
      depends <- 
        tools::package_dependencies(
          packages = pkg,
          db = available.packages(),
          which = c("Depends", "Imports", "LinkingTo"),
          recursive = TRUE
        ) |>
        unlist(use.names = FALSE) 
      
      # grab suggests
      suggests <- 
        tools::package_dependencies(
          packages = pkg,
          db = available.packages(),
          which = "Suggests",
          recursive = TRUE # this really blows up for almost any pkg
        ) |>
        unlist(use.names = FALSE) 
      
      # Where did package come from?
      repo_src <- avail_pkgs |>
        dplyr::filter(Package %in% pkg) |> 
        dplyr::pull(Repository) |> 
        dirname() |> dirname() # trim '/src/contrib/` ending
      repo_name <- get_repo_origin(repo_src = repo_src, pkg_name = pkg)
      
      
      
      pkg_meta <- list(
        pkg = pkg,
        ver = ver,
        r_ver = getRversion(),
        sys_info = list(R.Version()),
        repos = repo_name,
        val_date = val_date,
        ref = NA_character_,
        metric_pkg = NA_character_,
        decision = decisions[length(decisions)],
        decision_reason = "Dependency",
        final_decision = decisions[length(decisions)],
        final_decision_reason = "Dependency",
        depends = if(identical(depends, character(0))) NA_character_ else depends,
        suggests = if(identical(suggests, character(0))) NA_character_ else suggests,
        rev_deps = NA_character_,
        assessment_runtime = list(txt = NA_character_, mins = NA)
      )
      # save the pkg_meta
      saveRDS(pkg_meta, pkg_meta_file)
      cat("\n-->", pkg_v,"meta bundle saved.\n")
    }
    
    # return!
    pkg_meta
    
  }) |>
    purrr::set_names(nm = pkgs)
  
  # Message
  # dont_run |> length()
  skipped_pkgs <- pkgs[pkgs %in% dont_run]
  cat("\n--> All", pkgs_length, "packages processed;", skipped_pkgs |> length(),"of which were avoided due to a dependency failing it's risk assessment.\n")

  
  
  
  #
  # ---- Collate Pkg Meta into DF ----
  #
  
  # Reduce package bundles down into a data.frame containing specific info
  # names(pkg_bundles)
  pkgs_df0 <- purrr::map( pkg_bundles, ~ {
      # .x <- pkg_bundles$askpass
      x <- purrr::list_flatten(.x)
      # x$depends  <- if(all(is.na(x$depends)))  NA_character_ else paste(x$depends, collapse = ", ")
      # x$suggests <- if(all(is.na(x$suggests))) NA_character_ else paste(x$suggests, collapse = ", ")
      
      x$depends <- list(x$depends)
      x$suggests <- list(x$suggests)
      x$rev_deps <- list(x$rev_deps)
      x$sys_info <- list(x$sys_info)
      # x$repos <- list(x$repos)
      dplyr::as_tibble(x)
    }) |> 
    purrr::reduce(dplyr::bind_rows)
  
  
  saveRDS(pkgs_df0, file.path(val_dir, "qual_supplement0.rds"))
  # cat(paste0("\n--> Saved qualification evidence to ", file.path(val_dir, "qual_evidence.rds"), "\n"))
  
  
  
  #
  # ---- Collate Assessment files into DF ----
  #
  
  # # Start bundling rds files
  assessment_bundle <- purrr::map2(pkgs, vers, function(pkg, ver){
    # i <- 1 # for debugging
    # pkg <- pkgs[i] # for debugging
    # ver <- vers[i] # for debugging
    pkg_v <- paste(pkg, ver, sep = "_")
    pkg_assess_record_file <- file.path(assessed, glue::glue("{pkg_v}_assess_record.rds"))
    if(file.exists(pkg_assess_record_file)) {
      assess_record <- readRDS(pkg_assess_record_file)
    }
  }) |>
    purrr::reduce(dplyr::bind_rows)
  saveRDS(assessment_bundle, file.path(val_dir, "qual_assessments.rds"))
  
  
  cat("\n--> Collated pkg metadata.\n")
  
  #
  # ---- Update final decisions ----
  #
  
  # We need to be able to change 'final' decisions (recursively) if a package's
  # dependency doesn't pass. That means, All the packages where decision is NOT
  # marked "Low" need to have their decision matriculate up through their
  # reverse dependencies (rev_deps).
  
  # Steps:
  # 1. identify all packages that are NOT "Low Risk"
  # 2. identify all packages that depend on those packages
  # 3. change their decision the decision of their dependency
  
  # pkgs_df0$decision[1] <- "High" # for debugging
  # NOTE: deps is from the args to val_build()... will need to be added
  reject_iteration <- function(pkg_dat, dec_reject = "High", failed_pkgs = NULL){
    
    # 1. identify all packages that are NOT "Low Risk"
    if(is.null(failed_pkgs)) {
      failed_pkgs <- pkg_dat$pkg[pkg_dat$final_decision != decisions[1]]
    }
    
    # Process the data.frame
    pkg_dat <- pkg_dat |>
      
      # 2. identify all packages that depend on those packages
      dplyr::mutate(dep_failed = purrr::map_lgl(depends, ~ any(.x %in% failed_pkgs)),
                    sug_failed = purrr::map_lgl(suggests, ~ any(.x %in% failed_pkgs))) |>
      
      # 3. change their decision the decision of their dependency
      dplyr::mutate(
        final_decision = dplyr::case_when(
          dep_failed ~ dec_reject, # if any of the dependencies failed, then mark as 'High'
          sug_failed & ("Suggests" %in% deps) ~ dec_reject, # if any of the suggests failed, then mark as 'High'
          .default = decision
        ),
        final_decision_reason = dplyr::case_when(
          dep_failed ~ "Dependency",
          sug_failed & ("Suggests" %in% deps) ~ "Dependency",
          .default = decision_reason
        )
      ) |>
      dplyr::select(-dep_failed, -sug_failed)
    
    return(pkg_dat)
  }
  
  # First iteration:
  # Based off of 'decision', not 'final_decision'
  dec_reject <- decisions[length(decisions)]
  failed <- pkgs_df0$pkg[pkgs_df0$decision != decisions[1]] # start w/ 'decision'
  pkgs_df <- reject_iteration(pkgs_df0, dec_reject, failed)
  
  # All remaining iterations!
  while(!identical(pkgs_df$pkg[pkgs_df$final_decision != decisions[1]], failed)) {
    # if the list of failed pkgs has changed, then we need to iterate again
    failed <<- pkgs_df$pkg[pkgs_df$final_decision != decisions[1]]
    pkgs_df <<- reject_iteration(pkgs_df, dec_reject, failed)
  }
  
  cat("\n--> Assigned 'final' decisions.\n")
  
  
  
  
  
  #
  # ---- Update pkg_meta RDS file ----
  #
  # Which packges had a decision change?
  changed_pkgs <-
    pkgs_df |>
    dplyr::filter(final_decision != decision)

  purrr::walk2(changed_pkgs$pkg, changed_pkgs$ver, function(pkg, ver){
    # i <- 1 # for debugging
    # pkg <- changed_pkgs$pkg[i] # for debugging
    # ver <- changed_pkgs$ver[i] # for debugging
    pkg_v <- paste(pkg, ver, sep = "_")
    pkg_meta_file <- file.path(assessed, glue::glue("{pkg_v}_meta.rds"))
    pkg_meta_file <- pkg_meta_file[file.exists(pkg_meta_file)]
    if(length(pkg_meta_file) > 0) {
      # update the decision of each reverse dependency pkg
      purrr::walk(pkg_meta_file, function(f){
        dep_meta <- readRDS(f)
        dep_meta$final_decision_reason <- "Dependency"
        dep_meta$final_decision <- decisions[length(decisions)]
        saveRDS(dep_meta, f)
        cat(paste0("\n\n--> Updated ", dep_meta$pkg, " v", dep_meta$ver," from '", dep_meta$decision,"' to '", dep_meta$final_decision,"' in meta bundle .rds.\n"))
      })
    }
  })
  
  cat("\n--> Updated", nrow(changed_pkgs),"pkg metadata files.\n")
  
  val_end <- Sys.time()
  val_end_txt <- utils::capture.output(val_end - val_start)
  cat("\n--> Build", val_end_txt,"\n")
  
  saveRDS(pkgs_df, file.path(val_dir, "qual_supplement.rds"))
  cat(paste0("\n--> Saved qualification evidence to ", file.path(val_dir, "qual_evidence.rds"), "\n"))
  
  # Return object 
  return(list(
    val_dir = val_dir,
    pkg_meta = pkgs_df,
    pkg_assess = assessment_bundle
  ))
}



