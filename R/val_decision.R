
#' Assign a decision
#'
#' First whack at attempting to filter packages based on org-level criterion
#' used to set thresholds (and later update final decision (if not already 'high
#' risk')) AND filter packages before running val_build(). Note: If PACKAGES
#' file had assessments, we'd be using that (paired with \{val.filter\}), but
#' instead, we're going to use riskscore::cran_assessed_20250812 for the time
#' being
#'
#' @param pkg character, the package name to assess
#' @param source character, either "riskscore" (default), "PACKAGES", or a
#'   data.frame.
#' @param excl_metrics character vector, allow users to subset metrics as needed
#'   without changing the config
#' @param decisions character vector, the risk categories to use.
#' @param else_cat character, the default risk category if no conditions are
#'   met.
#' @param avail_pkgs data.frame, the output of available.packages()
#' @param decisions_df data.frame, the output of build_decisions_df()
#'
#' @importFrom dplyr filter pull mutate case_when between rename left_join
#'   across if_else everything select
#' @importFrom purrr map map_int
#' @importFrom tools package_dependencies
#' 
val_decision <- function(
    pkg = NULL,
    source = NULL,
    excl_metrics = NULL,
    decisions = c("Low", "Medium", "High"),
    else_cat = "High",
    avail_pkgs = available.packages() |> as.data.frame(),
    decisions_df = build_decisions_df(rule_type = "decide")
) {
  # Verify pkg is not null
  if(is.null(pkg)) {
    stop("Must provide a package name to 'pkg'")
  }
  
  # verify decisions_df is compliant
  if(!all(c("metric", "decision", "condition", "metric_type", "accept_condition") %in% colnames(decisions_df))) {
    stop("'decisions_df' is not compliant. Must contain columns: 'metric', 'decision', 'condition', 'metric_type', 'accept_condition'")
  }
  
  # Use package metrics based on specified source
  if(!inherits(source, "list")) {
    stop("\nInvalid source specified. Must be a list")
  } 
  
  # Verify we have both the 'assessment' and 'scores' data.frames
  if(!all(c("assessment", "scores") %in% names(source))) {
    stop("\nInvalid source specified. Must be a list with elements 'assessment' and 'scores'")
  }
  
  
  
  #
  # ---- Prepare workable fields to categorize  ----
  #
  
  # First, allow user to subset the metrics
  # names(source$assessment)
  decisions_df <- decisions_df |>
    dplyr::filter(!(metric %in% excl_metrics))

  assessment <- source$assessment[!(names(source$assessment) %in% excl_metrics)]
  scores <- source$scores[!(names(source$scores) %in% excl_metrics)]
  
  
  # rm(pkgs_df)
  pkgs_df <- data.frame(package = pkg)
  
  # names(assessment)
  
  # extract list values into a numeric vector
  
  # downloads_1yr
  if("downloads_1yr" %in% decisions_df$metric &
     "downloads_1yr" %in% names(assessment)) {
    pkgs_df$downloads_1yr <- if(is.null(assessment$downloads_1yr)) NA_real_ else as.numeric(assessment$downloads_1yr)
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "downloads_1yr"))
  }
  # covr_coverage
  if("covr_coverage" %in% decisions_df$metric &
     "covr_coverage" %in% names(assessment)) {
    if(!any(is.na(assessment$covr_coverage))) {
      pkgs_df$covr_coverage <- if(is.null(assessment$covr_coverage$totalcoverage)) NA_real_ else as.numeric(assessment$covr_coverage$totalcoverage)
    } else {
      decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "covr_coverage"))
    }
    # filecoverage <- assessment$covr_coverage$filecoverage
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "covr_coverage"))
  }
  # r_cmd_check
  if("r_cmd_check_errors" %in% decisions_df$metric &
     "r_cmd_check" %in% names(assessment)) {
    r_cmd_check <- assessment$r_cmd_check
    # r_cmd_check[[2]]
    if("pkg_metric_error" %in% class(assessment$r_cmd_check)) {
      pkgs_df$r_cmd_check_errors <- NA_real_
    } else {
      # logic when not an error
      pkgs_df$r_cmd_check_errors <- if(length(r_cmd_check) > 1){
        if(is.null(r_cmd_check[[2]])) NA_real_ else r_cmd_check[[2]]
      } else {
        if(is.na(r_cmd_check)) NA_real_ else r_cmd_check
      }
      
    }
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "r_cmd_check_errors"))
  }
  # r_cmd_check
  if("r_cmd_check_warnings" %in% decisions_df$metric &
     "r_cmd_check" %in% names(assessment)) {
    r_cmd_check <- assessment$r_cmd_check
    if("pkg_metric_error" %in% class(assessment$r_cmd_check)) {
      pkgs_df$r_cmd_check_warnings <- NA_real_
    } else {
      # logic when not an error
      pkgs_df$r_cmd_check_warnings <-  if(length(r_cmd_check) > 1){
        if(is.null(r_cmd_check[[3]])) NA_real_ else r_cmd_check[[3]]
      } else {
        if(is.na(r_cmd_check)) NA_real_ else r_cmd_check
      }
    }
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "r_cmd_check_warnings"))
  }
  # reverse_dependencies
  if("reverse_dependencies" %in% decisions_df$metric &
     "reverse_dependencies" %in% names(assessment)) {
    pkgs_df$reverse_dependencies <- if(is.null(assessment$reverse_dependencies)) NA_real_ else assessment$reverse_dependencies |> length()
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "reverse_dependencies"))
  }
  # dependencies
  if("dependencies" %in% decisions_df$metric &
     "dependencies" %in% names(assessment)) {
    
    # eh, this doesn't include suggests
    # assessment$dependencies |> nrow()
    
    deppies <- tools::package_dependencies(
      packages = pkg,
      db = avail_pkgs |> as.matrix(),
      which = c("Depends", "Imports", "LinkingTo"),
      recursive = FALSE
    ) |>
      unlist(use.names = FALSE)
    
    # We don't really HAVE to store these deps here.
    # paste & collapse them into a single string
    # pkgs_df$deps <- if(length(deppies) == 0) NULL else paste(deppies, collapse = ", ")
    pkgs_df$dependencies <- deppies |> length()
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "dependencies"))
  }
  # has_vignettes
  if("has_vignettes" %in% decisions_df$metric &
     "has_vignettes" %in% names(assessment)) {
    pkgs_df$has_vignettes <- if(is.null(assessment$has_vignettes)) NA_real_ else as.numeric(assessment$has_vignettes)
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "has_vignettes"))
  }
  # has_source_control
  if("has_source_control" %in% decisions_df$metric &
     "has_source_control" %in% names(assessment)) {
    pkgs_df$has_source_control <- if(is.null(assessment$has_source_control)) NA_real_ else assessment$has_source_control |> length()
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "has_source_control"))
  }
  # has_website
  if("has_website" %in% decisions_df$metric &
     "has_website" %in% names(assessment)) {
    pkgs_df$has_website <- if(is.null(assessment$has_website)) NA_real_ else assessment$has_website |> length()
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "has_website"))
  }
  # has_news
  if("has_news" %in% decisions_df$metric &
     "has_news" %in% names(assessment)) {
    pkgs_df$has_news <- if(is.null(assessment$has_news)) NA_real_ else as.numeric(assessment$has_news)
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "has_news"))
  }
  # news_current
  if("news_current" %in% decisions_df$metric &
     "news_current" %in% names(scores)) {
    pkgs_df$news_current <- if(is.null(scores$news_current)) NA_real_ else as.numeric(scores$news_current)
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "news_current"))
  }
  # bugs_status
  if("bugs_status" %in% decisions_df$metric &
     "bugs_status" %in% names(scores)) {
    pkgs_df$bugs_status <- if(is.null(scores$bugs_status)) NA_real_ else as.numeric(scores$bugs_status)
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "bugs_status"))
  }
  # remote_checks
  if("remote_checks" %in% decisions_df$metric &
     "remote_checks" %in% names(assessment)) {
    pkgs_df$remote_checks <- if(is.null(assessment$remote_checks)) NA_real_ else as.numeric(assessment$remote_checks)
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "remote_checks"))
  }
  
  # exported_namespace
  if("exported_namespace" %in% decisions_df$metric &
     "exported_namespace" %in% names(assessment)) {
    pkgs_df$exported_namespace <- if(is.null(assessment$exported_namespace)) NA_real_ else assessment$exported_namespace |> length()
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "exported_namespace"))
  }
  # export_help
  if("export_help" %in% decisions_df$metric &
     "export_help" %in% names(scores)) {
    pkgs_df$export_help <- if(is.null(scores$export_help)) NA_real_ else as.numeric(scores$export_help) * 100
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "export_help"))
  }
  # has_maintainer
  if("has_maintainer" %in% decisions_df$metric &
     "has_maintainer" %in% names(assessment)) {
    pkgs_df$has_maintainer <- if(is.null(assessment$has_maintainer)) NA_real_ else assessment$has_maintainer |> length()
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "has_maintainer"))
  }
  # size_codebase
  if("size_codebase" %in% decisions_df$metric &
     "size_codebase" %in% names(assessment)) {
    pkgs_df$size_codebase <- if(is.null(assessment$size_codebase)) NA_real_ else as.numeric(assessment$size_codebase)
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "size_codebase"))
  }
  # has_bug_reports_url
  if("has_bug_reports_url" %in% decisions_df$metric &
     "has_bug_reports_url" %in% names(assessment)) {
    pkgs_df$has_bug_reports_url <- if(is.null(assessment$has_bug_reports_url)) NA_real_ else assessment$has_bug_reports_url
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "has_bug_reports_url"))
  }
  # has_examples
  if("has_examples" %in% decisions_df$metric &
     "has_examples" %in% names(scores)) {
    pkgs_df$has_examples <- if(is.null(scores$has_examples)) NA_real_ else as.numeric(scores$has_examples) * 100
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "has_examples"))
  }
  # license
  if("license" %in% decisions_df$metric &
     "license" %in% names(assessment)) {
    pkgs_df$license <- if(is.null(assessment$license)) NA_real_ else assessment$license
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "license"))
  }

  
  
  #
  # ---- Apply Decisions ----
  #
  
  # Some setup:
  # Where did package come from?
  repo_src <- avail_pkgs |>
    dplyr::filter(Package %in% pkg) |> 
    dplyr::pull(Repository) |>
    dirname() |> dirname() # remove '/src/contrib/` ending
  curr_repos <- options("repos")
  
  # look-up name of curr_repos based on repo_src. Yes, I know that is very
  # dependent on the naming convention chosen for our repos
  repo_name <- curr_repos$repos[curr_repos$repos %in% repo_src] |> names()
  if(length(repo_name) == 0) repo_name <- "unknown"
  if(length(repo_name) > 1) {
    repo_name <- repo_name[1]
    cat(glue::glue("\n\n!!! WARNING: Package '{pkg}' appears to come from multiple repos. Using '{repo_name[1]}' for decisioning.\n"))
  }

  
  #
  # --- Primary Metrics ----
  #
  primed_pkgs <- rip_cats_by_pkg(
    label = "Primary",
    repo_name = repo_name,
    dec_df = decisions_df,
    pkgs_df = pkgs_df,
    else_cat = else_cat
  )
  

  #
  # --- Secondary Metrics ----
  #
  
  # If not from CRAN, then make decision using secondary metric conditions
  if(tolower(repo_name) != "cran" & primed_pkgs$final_risk_cat == decisions[1]) {
  
    cat(glue::glue("\n\n\n--> Package '{pkg}' appears to come from repo: '{repo_name}' ({repo_src}) which requires secondary metric assessments.\n\n"))
    #
    # ---- Perform 'Secondary' Checks ----
    #
    pkgs_sec_cats <- rip_cats_by_pkg(
      label = "Secondary",
      repo_name = repo_name,
      dec_df = decisions_df,
      pkgs_df = primed_pkgs |>
        dplyr::rename(primary_risk_category = final_risk_cat) |>
        dplyr::select(-dplyr::ends_with("_cat")), # remove to keep things tidy, or could leave them in?
      else_cat = else_cat
    )
      

    #
    # ---- Generalte Final Decision ----
    #
    
    # Combine Primary & Secondary into final decision
    pkgs_final <- pkgs_sec_cats |>
      dplyr::rename(secondary_risk_category = final_risk_cat) |>

      # create final_risk variable that finds the highest risk between 
      # primary_risk_category & secondary_risk_category 
      dplyr::mutate(
        final_risk_id = ifelse(
            is.na(primary_risk_category), as.integer(secondary_risk_category),
            max(as.integer(primary_risk_category), as.integer(secondary_risk_category))
          )
      ) |>
      dplyr::left_join(
        dec_id_df |> dplyr::rename(final_risk = decision),
        by = c("final_risk_id" = "decision_id")
      ) |>
      dplyr::mutate(
        final_risk = factor(final_risk, levels = decisions),
      ) |>
      dplyr::select(
        package,
        primary_risk_category, secondary_risk_category, final_risk,
        # final_risk_manual,
        dplyr::everything(),
        -final_risk_id # keep id?
      )
    
    
  } else {
    if(primed_pkgs$final_risk_cat != decisions[1]) {
      cat(glue::glue("\n\n--> Package '{pkg}' has been categorized as '{primed_pkgs$final_risk_cat}' risk based on Primary metrics alone. No Secondary metrics needed.\n"))
    }
    # Secondary metrics not needed because it's a CRAN pkg or pkg 'failed'
    pkgs_final <- primed_pkgs |>
      dplyr::select(
        package, final_risk = final_risk_cat,
        dplyr::everything()
      )
  }
  
  #
  # ---- Return data for filtering (presumably)
  # 
  cat("\n\n--> Final Risk Summary for package '", pkg, "':\n", sep = "")
  print(
    pkgs_final$final_risk |>
      # factor(levels = c("Low", "Medium", "High")) |> # not needed
      table() |>
      as.data.frame() |>
      dplyr::rename("Final Risk" = Var1, Cnt = Freq)
  )
  
  return(pkgs_final)
}




#' Categorize Decisions for A Corpus of Packages
#'
#' First whack at attempting to filter packages based on org-level criterion
#' used to set thresholds (and later update final decision (if not already 'high
#' risk')) AND filter packages before running val_build(). Note: If PACKAGES
#' file had assessments, we'd be using that (paired with \{val.filter\}), but
#' instead, we're going to use riskscore::cran_assessed_20250812 for the time
#' being
#'
#' @param source character, either "riskscore" (default), "PACKAGES", or a
#'   data.frame.
#' @param decisions character vector, the risk categories to use.
#' @param else_cat character, the default risk category if no conditions are met.
#' @param decisions_df data.frame, the output of build_decisions_df()
#'
#' @importFrom dplyr filter pull mutate case_when between rename left_join
#'   across if_else everything select
#' @importFrom purrr map map_int 
#' @importFrom tools package_dependencies
#' @importFrom utils head packageVersion
#' 
#' 
val_categorize <- function(
    source = "riskscore",
    decisions = c("Low", "Medium", "High"),
    else_cat = "High",
    decisions_df = build_decisions_df(rule_type = "remote_reduce")
) {
  # @importFrom riskscore cran_assessed_20250812 cran_scored_20250812
  
  # verify decisions_df is compliant
  if(!all(c("metric", "decision", "condition", "metric_type", "accept_condition") %in% colnames(decisions_df))) {
    stop("'decisions_df' is not compliant. Must contain columns: 'metric', 'decision', 'condition', 'metric_type', 'accept_condition'")
  }
  
  # Use package metrics based on specified source
  if(length(source) > 1) {
    stop("\nInvalid source specified. Must be one of 'riskscore', list, data.frame, or 'PACKAGES' file path.")
  } else if(source == "riskscore") {
    
    requireNamespace("riskscore", quietly = TRUE)
    # Use "pkg_cran_remote" data from riskscore::cran_assessed_latest
    # remotes::install_github("pharmar/riskscore", force = TRUE,
    #                         ref = "main")
    pv <- utils::packageVersion("riskscore") # verify ‘v0.0.3'
    riskscore_run_date <- riskscore::assessed_latest$riskmetric_run_date |> unique()
    cat(paste0("\n--> Using {riskscore} Version: 'v", pv, "', last compiled on '",
               riskscore_run_date,"'.\n"))
    if(Sys.Date() - as.Date(riskscore_run_date) > 60) {
      cat(glue::glue("\n\n!!! WARNING: the latest riskscore assessment date is more than 60 days old, compared to today's validation date. Consider updating {{riskscore}} w/ a fresh run.\n"))
    }
    
    opt_repos <- pull_config(val = "opt_repos", rule_type = "default") |> unlist()
    opt_repos <- update_opt_repos(val_date = riskscore_run_date, opt_repos = opt_repos)
    options(repos = opt_repos, pkgType = "source", scipen = 999)
    # options("repos") # verify
    
    # Some setup:
    curr_repos <- options("repos")
    avail_pkgs <- available.packages() |>as.data.frame()
      
    # Where did package come from?
    # categorize Repository field to match names in options("repos")
    avail_pkgs$repo_name <-
      purrr::map_chr(avail_pkgs$Repository, ~ {
        if(is.null(.x)) NA_character_ else {
          repo_src <- .x |> dirname() |> dirname() # remove '/src/contrib/` ending
          repo_name <- curr_repos$repos[curr_repos$repos %in% repo_src] |> names()
          if(length(repo_name) == 0) repo_name <- "unknown"
          if(length(repo_name) > 1) {
            repo_name <- repo_name[1]
          }
          repo_name
        }
      })
    
    cat("\n\nCategorizing available packages. Starting w/", nrow(avail_pkgs), "pkgs.\n")

    # Sometimes, the package field is a list() in the 2025-10-01 riskscore
    # build. So, we need to unlist it first
    if(inherits(riskscore::assessed_latest$package, "list")){
      package_col <- riskscore::assessed_latest$package |> unlist()
      ver_col <- riskscore::assessed_latest$version |> unlist()
    } else {
      package_col <- riskscore::assessed_latest$package
      ver_col <- riskscore::assessed_latest$version
    }
    
    # How many pkgs are in the riskscore data vs available.packages()?
    cat(glue::glue("\n--> There are {prettyNum(nrow(riskscore::assessed_latest), big.mark = ',')} packages with assessments in the latest riskscore data & {prettyNum(nrow(avail_pkgs), big.mark = ',')} packages available.\n"))

    # Pkgs not in available.packages()
    missing_ap <- package_col[!(package_col %in% avail_pkgs$Package)]
    if(length(missing_ap) > 0) {
      cat(glue::glue("\nNote: There are {length(missing_ap)} packages in the riskscore data that are NOT in available.packages(). These will be ignored, because they have likely been removed from their CRAN-like repo since the last riskscore build. OR, the current option('repos') urls are excluding them.\n\n"))
      print(utils::head(missing_ap, 20))

      if(length(missing_ap) > 20) cat("...\n")
    }
    
    # Pkgs not in riskscore data
    # These are a little more problematic because we don't have assessments for them
    # So by default, do we need to mark them as "low" so that they are evaluated by
    # val_build() using pkg_sources? Yes!
    missing_rs <- avail_pkgs$Package[!(avail_pkgs$Package %in% package_col)]
    if(length(missing_rs) > 0) {
      cat(glue::glue("\n\n!!! WARNING: There are {length(missing_rs)} packages in available.packages() that are NOT in the riskscore data.\n"))
      print(utils::head(missing_rs, 20))
      if(length(missing_rs) > 20) cat("...\n")
    }
    # options("repos")
    # dput(missing_rs)
    # avail_pkgs$Repository |> table()
    
    pkgs <- avail_pkgs |>
      dplyr::select(package = Package, version = Version, repo_src = Repository, repo_name) |>
      dplyr::left_join(
        riskscore::assessed_latest |>
          # if the package doesn't exist in riskscore for initial filtering, we
          # have to keep it in the pool to be evaluated using pkg sources later
          dplyr::mutate(auto_pass = ifelse(package %in% missing_rs, TRUE, FALSE),
                        package = package_col,
                        version = ver_col) |>
          dplyr::select(package, #version, 
                        auto_pass,
                        downloads_1yr, reverse_dependencies,
                        has_vignettes, has_source_control, has_website,
                        news_current, bugs_status
          ),
        # do not want to join on "version"  because it may not match what's
        # available in the riskscore data, since it can be up to 2 months old
        by = c("package") # No 'version'
      ) |>
      dplyr::mutate(auto_pass = ifelse(is.na(auto_pass), TRUE, auto_pass))
      
    # table(pkgs$auto_pass) # should be all false. If not, those pkgs will be passed
    # so that they can be evaluated using pkg sources
    
    pkgs_scored <- 
      avail_pkgs |>
      dplyr::select(package = Package, version = Version) |>
      dplyr::left_join(
        riskscore::scored_latest |>
          dplyr::select(package, version, news_current, bugs_status),
        by = c("package", "version")
      )
    # object.size(pkgs) / 1000000000
    
    #
    # ---- Prepare workable fields to categorize  ----
    #
    # extract list values into a numeric vector
    if("downloads_1yr" %in% decisions_df$metric) {
      pkgs$dwnlds <- purrr::map_dbl(pkgs$downloads_1yr, ~ {
          if(is.null(.x[[1]])) NA_real_ else as.numeric(.x[[1]])
        })
    }
    if("reverse_dependencies" %in% decisions_df$metric) {
      pkgs$rev_deps <- pkgs$reverse_dependencies |>
        purrr::map(~length(.x[[1]])) |>
        unlist() |>
        as.numeric()
    }
    if("dependencies" %in% decisions_df$metric) {
      pkgs <- pkgs |>
        dplyr::mutate(
          deps = 
            tools::package_dependencies(
              packages = package,
              db = available.packages(),
              which = c("Depends", "Imports", "LinkingTo"),
              recursive = FALSE
            ),
          n_deps = purrr::map_int(deps, ~length(.x))
        )
    }
    if("news_current" %in% decisions_df$metric) {
      pkgs <- pkgs |>
        # dplyr::rename(version = version.x) |>
        dplyr::left_join(
          pkgs_scored |>
            dplyr::mutate(news_curr = as.numeric(news_current)) |>
            dplyr::select(package, version, news_curr),
          by = c("package", "version")
        )
    }
    if("has_vignettes" %in% decisions_df$metric) {
      # is.integer(pkgs$has_vignettes[[11]][[1]])
      # pkgs$n_vig <- NULL
      # .x <- pkgs$has_vignettes[[104]]
      pkgs$n_vig <- purrr::map_dbl(pkgs$has_vignettes, ~ {
        if(is.null(.x[[1]])) NA_real_ else {
          if("pkg_metric_error" %in% class(.x[[1]])) NA_real_ else as.numeric(.x[[1]])
        }
      })
    }
    if("has_source_control" %in% decisions_df$metric) {
      # is.integer(pkgs$has_source_control[[11]][[1]] |> length())
      pkgs$src_cntrl <- pkgs$has_source_control |>
        purrr::map(~.x[[1]] |> length()) |>
        unlist()
    }
    if("has_website" %in% decisions_df$metric) {
      # is.integer(pkgs$has_website[[11]][[1]] |> length())
      pkgs$n_sites <- pkgs$has_website |>
        purrr::map(~.x[[1]] |> length()) |>
        unlist()
    }
    
    # Add dervied col names to 'decisions_df' so we can map metrics with their
    # 'useful' column
    decisions_df <- decisions_df |>
      dplyr::mutate(derived_col = dplyr::case_when(
        metric == "downloads_1yr" ~ "dwnlds",
        metric == "reverse_dependencies" ~ "rev_deps",
        metric == "dependencies" ~ "n_deps",
        metric == "news_current" ~ "news_curr",
        metric == "has_vignettes" ~ "n_vig",
        metric == "has_source_control" ~ "src_cntrl",
        metric == "has_website" ~ "n_sites",
        
      ))
    
  } else if (inherits(source, "list")) {
    stop("Not yet implemented: val_filter() using data.frame 'source'")
    # The assessments object's default structure is a list

    
  } else if (is.data.frame(source)) {
    stop("Not yet implemented: val_filter() using data.frame 'source'")
    
    # Not sure what this looks like yet, but we could assume it looks similar
    # to the riskscore output
    
  } else if(source == "PACKAGES") {
    stop("Not yet implemented: val_filter() using 'PACKAGES' file")
  } else {
    stop("Invalid source specified. Must be one of 'riskscore', 'PACKAGES', or a data.frame")
  }
  
  
  
  # Note! This area is to automatically exclude pkgs
  # that are considered high risk so we won't have to waste compute
  # assessing these pkgs. That said, we'll be filtering based
  # on our definition of 'high risk' and 'high risk' only, based on
  # the cran_remote ref source. 
  #
  # That means this area will not be used for SETTING decisions, because
  # those decisions can only be made on the 'source' ref assessments. If
  # a pkg is 'high risk', we should automatically create the metadata list
  # necessary.
  #
  # In the future, we'd like to use {val.filter} here to get our subset.
  # We'd also like to be able to extract the filtering logic as needed
  

  
  
  #
  # ---- Apply Decisions ----
  #
  
  # Some setup
  # Which metrics are available?
  all_mets <- decisions_df$metric |> unique()
  
  
  #
  # --- Primary Metrics ----
  #
  primary_metrics <- decisions_df |>
    dplyr::filter(tolower(metric_type) == "primary")
    # for debugging
    # dplyr::filter(tolower(metric) %in% c("downloads_1yr", "reverse_dependencies"))
    # dplyr::filter(tolower(metric) %in% c("has_website")) 
  
  
  if(nrow(primary_metrics) > 0) {
    
    primed_pkgs <- split_join_cats(
      pkgs_data = pkgs,
      dec_df = primary_metrics,
      decisions = decisions,
      else_cat = else_cat,
      label = "Primary"
    )
    
  } else {
    cat("\n\n-->No 'Primary' metrics found in 'decisions_df'.")
    # Set as "Medium" risk for now, which allows it to get promoted to "low"
    # risk as needed.
    primed_pkgs <- pkgs |>
      dplyr::mutate(final_risk_cat = factor(NA, levels = decisions))
  }
  
  

  # ---- 'Exceptions' ----
  # Exceptions to this? Perhaps some 'high' risk pkgs
  # could move to 'medium' if they have other outstanding metrics? Similarly,
  # "Medium" could move to "Low". Also, there may be some exception metrics
  # that will auto-accept a pkg to "Low" risk.
  exception_metrics <- decisions_df |>
    dplyr::filter(!(tolower(metric_type) %in% "primary"))
  
  # exc_met_len <- exception_metrics$metric |> unique() |> length()
  
  if(nrow(exception_metrics > 0)){
    
    pkgs_exc_cats <- split_join_cats(
      pkgs_data = 
        primed_pkgs |>
          dplyr::rename(primary_risk_category = final_risk_cat) |>
          dplyr::select(-c(dplyr::ends_with("_cat"), dplyr::ends_with("_cataa"))),
      dec_df = exception_metrics,
      decisions = decisions,
      else_cat = else_cat,
      label = "Exception"
    ) |>
      dplyr::rename(exception_risk_category = final_risk_cat)
    
    # cat(glue::glue("\n\n> Applying Decisions Categories to {exc_met_len} 'Exception' risk metric(s).\n\n"))
    # cat("\n---->", paste(exception_metrics$metric |> unique(), collapse = '\n----> '), "\n")
    # 
    # # Create metric-based risk categories decision columns
    # # rm(pkgs_final)
    # pkgs_exc_cats <- rip_cats(
    #   met_dec_df = exception_metrics,
    #   pkgs_df =  
    #     primed_pkgs |>
    #       dplyr::rename(primary_risk_category = final_risk_cat) |>
    #       dplyr::select(-c(dplyr::ends_with("_cat"), dplyr::ends_with("_cataa"))),
    #   else_cat = else_cat
    #   ) |>
    #   dplyr::rename(exception_risk_category = final_risk_cat)

    #
    # ---- Promote 'Exceptions' ----
    #
    
    dec_id_df <- unique(decisions_df[,c("decision", "decision_id")])
    promos <-
      pkgs_exc_cats |>
      
      # create final_risk variable that reduces the decision category
      # from primary_risk_category if exception_risk_category is the lowest risk
      dplyr::mutate(
        final_risk_id = 
          dplyr::case_when(
            is.na(primary_risk_category) ~ as.integer(exception_risk_category),
            as.integer(exception_risk_category) == 1 &
              as.integer(primary_risk_category) > 1 ~ as.integer(primary_risk_category) - 1,
            .default = as.integer(primary_risk_category)
          )
      ) |>
      dplyr::left_join(
        dec_id_df |> dplyr::rename(final_risk = decision),
        by = c("final_risk_id" = "decision_id")
      ) |>
      dplyr::mutate(
        final_risk = factor(final_risk, levels = decisions),
      ) |>
      # if auto_pass is TRUE, then set final_risk to "Low"
      dplyr::mutate(
        final_risk = ifelse(auto_pass, decisions[1], as.character(final_risk)) |> factor(levels = decisions)
      ) |>
      dplyr::select(
        package, version, 
        primary_risk_category, exception_risk_category, final_risk, 
        # final_risk_manual, 
        dplyr::everything(),
        -final_risk_id, # keep id?
        ) 
    
    # Make note of Pkgs that shifted thanks to exceptions
    if(!all(is.na(promos$primary_risk_category))) {
      cat("\n--> Exceptions to Primary metric decisions based on meeting ALL of the following metric criterion:\n\n")
      diff_table <- {
        promos$final_risk |>
          factor(levels = levels(decisions_df$decision)) |>
          table()
      } - {
        promos$primary_risk_category |>
          factor(levels = levels(decisions_df$decision)) |>
          table()
      }
      
      # print note on promotions to console
      cat("\n")
      print(
        diff_table |>
          as.data.frame() |>
          dplyr::rename(`Risk Shifted` = Var1, Added = Freq)
      )
    }
    
    # remove primary & exception risk category columns
    pkgs_final <- promos |>
      dplyr::select(-primary_risk_category, -exception_risk_category)
    
  } else {
    pkgs_final <- primed_pkgs |>
      # if auto_pass is TRUE, then set final_risk to "Low"
      dplyr::mutate(
        final_risk = ifelse(auto_pass, decisions[1],
          ifelse(is.na(final_risk_cat), else_cat,
            final_risk_cat |> as.character()) |> factor(levels = decisions)
      )) |>
      dplyr::select(
        package, version, final_risk,
        dplyr::everything()
      )
  }
    
  #
  # ---- Return data for filtering (presumably) ----
  # 
  cat("\n")
  print(
    pkgs_final$final_risk |>
      # factor(levels = c("Low", "Medium", "High")) |> # not needed
      table() |>
      as.data.frame() |>
      dplyr::left_join(
        {round(prop.table(table(pkgs_final$final_risk)), 3) * 100} |>
          as.data.frame(),
        by = "Var1"
      ) |>
      dplyr::rename("Final Risk" = Var1, Cnt = Freq.x, Pct = Freq.y)
  )
  
  return(pkgs_final)
}










