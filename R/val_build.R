
#' Build Validation
#' 
#' 
#' @importFrom glue glue
#' @importFrom tidyr unite
#' @importFrom dplyr filter pull
#' @importFrom purrr map2 set_names
#' @importFrom stringr word
#' 
val_build <- function(
    pkg_names = NULL, #
    ref = "source", 
    metric_pkg = "riskmetric",
    deps = c("depends", "suggests"), # deps = c("depends"), deps = NULL
    deps_recursive = TRUE,
    val_date = Sys.Date(),
    out = 'riskassessment',
    replace = FALSE,
    opt_repos = c(val_build_repo = "https://cran.r-project.org")
    ){
  
  # Assess args
  if(!metric_pkg %in% c('risk.assessr', 'riskmetric')) stop("'metric_pkg' arg must be either 'riskmetric' or 'risk.assessr' but '", metric_pkg, "' was given.")
  if(!ref %in% c('source', 'remote')) stop("'ref' arg must be either 'source' or 'remote' but '", ref, "' was given.")
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
  # ---- Set repos option ----
  #
  old <- options()
  on.exit(function() options(old))
  if(ref == 'source') {
    options(repos = opt_repos, pkgType = "source") # , rlang_interactive = FALSE
  } else {
    options(repos = opt_repos) # , rlang_interactive = FALSE
  }
  
  
  #
  # ---- Determine the list of pkgs to evaluate ----
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
      
      # params for debugging:
      # deps = NULL
      # deps = "depends"
      # deps = "suggests"
      # deps = c("depends", "suggests")
      # deps_recursive = TRUE
      
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
        db = available.packages(),
        which = which_deps,
        recursive = deps_recursive)
       
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
  # ---- Define dirs for processing ----
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
  # ---- Run through the list of packages ----
  #
  pkg_bundles <- purrr::map2(pkgs, vers, function(pkg, ver){
    # i <- 1 # for debugging
    # pkg <- pkgs[i] # for debugging
    # ver <- vers[i] # for debugging
    
    # check to make sure the pkg bundle doesn't already exist. If so, we can
    # skip building the bundle, but we still need to assess it's dependencies
    pkg_v <- paste(pkg, ver, sep = "_")
    pkg_meta_file <- file.path(assessed, glue::glue("{pkg_v}_meta.rds"))
    if(!file.exists(pkg_meta_file) | replace) {
      pkg_meta <- val_pkg(
        pkg = pkg,
        ver = ver,
        avail_pkgs = avail_pkgs,
        ref = ref,
        metric_pkg = metric_pkg,
        out_dir = val_dir,
        val_date = val_date)
    } else {
      cat(paste0("\n\n\nAttempted New Package: ", pkg, " v", ver,", but already assessed.\n\n"))
      pkg_meta <- readRDS(pkg_meta_file)
      
      cat("\n-->", pkg_v,"Using assessment previously stored.\n")
      
    }
    
    # ---- Rinse & Repeat on deps & suggests 
    # old... trying to do some recursive stuff, but it was a bad idea.
    # maybe_new <- c(pkg_meta$dependencies, pkg_meta$suggests)
    # dep_files <- file.path(assessed, glue::glue("{maybe_new}_meta.rds"))
    # new_pkgs_files <- dep_files[!file.exists(dep_files)]
    # # split into pkg & ver here
    # new_pkgs <- basename(new_pkgs_files) |> tools::file_path_sans_ext()
    # new_vers <- basename(new_pkgs_files) |> tools::file_path_sans_ext()
    # purrr::map2(new_pkgs, new_vers, function(pkg, ver){
    #   pkg_bundle <- val_pkg(pkg = pkg, ver = ver)
    # })
    
    # return
    pkg_meta
  }) |> purrr::set_names(nm = pkgs)
  
  cat("\n--> All packages processed.\n")
  
  #
  # ---- Convert to DF ----
  # names(pkg_bundles)
  # pkg_bundles$zoo |> dplyr::as_tibble()
  # pkg_bundles$zoo$suggests
  # pkg_bundles$lattice$suggests
  # rm(.x)
  # dput(pkg_bundles)
  pkgs_df <- purrr::map( pkg_bundles,
    ~ {
      # .x <- pkg_bundles$zoo
      x <- purrr::list_flatten(.x)
      # x$depends  <- if(all(is.na(x$depends)))  NA_character_ else paste(x$depends, collapse = ", ")
      # x$suggests <- if(all(is.na(x$suggests))) NA_character_ else paste(x$suggests, collapse = ", ")
      x$depends <- list(x$depends)
      x$suggests <- list(x$suggests)
      dplyr::as_tibble(x)
    }) |> 
    purrr::reduce(dplyr::bind_rows)
  # pkgs_df$suggests
  
  cat("\n--> Collated pkg metadata.\n")
  
  
  #
  # ---- Update final decisions ----
  #
  # So, after working through all those packages, we need to be able to
  # change 'final' decisions if a package's dependency doesn't pass
  # Reduce package bundles down into a data.frame containing specific info
  
  
  
  
  cat("\n--> Assigned 'final' decisions.\n")
  
  end <- Sys.time()
  end_txt <- capture.output(end - start)
  cat(end_txt)
  
  # Return object 
  return(list(
    val_dir = val_dir,
    pkgs_df = pkgs_df
  ))
}

# val_build(pkg_names = c('aamatch'), deps = NULL)
# val_build(pkg_names = c('zoo'), deps = NULL)
# val_build('zoo')