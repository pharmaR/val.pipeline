
#' Assign a decision
#'
#' First whack at attempting to filter packages based on org-level criterion
#' used to set thresholds (and later update final decision (if not already 'high
#' risk')) AND filter packages before running val_build(). Note: If PACKAGES
#' file had assessments, we'd be using that (paired with {val.filter}), but
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
  # covr_coverage
  if("covr_coverage" %in% decisions_df$metric &
     "covr_coverage" %in% names(assessment)) {
    if(!is.na(assessment$covr_coverage)) {
      pkgs_df$covr_coverage <- if(is.null(assessment$covr_coverage$totalcoverage)) NA_real_ else as.numeric(assessment$covr_coverage$totalcoverage)
    } else {
      decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "covr_coverage"))
    }
    # filecoverage <- assessment$covr_coverage$filecoverage
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "covr_coverage"))
  }
  # downloads_1yr
  if("downloads_1yr" %in% decisions_df$metric &
     "downloads_1yr" %in% names(assessment)) {
    pkgs_df$downloads_1yr <- if(is.null(assessment$downloads_1yr)) NA_real_ else as.numeric(assessment$downloads_1yr)
  } else {
    decisions_df <- decisions_df |> dplyr::filter(!(metric %in% "downloads_1yr"))
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
      db = available.packages(),
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
    pkgs_df$export_help <- if(is.null(scores$export_help)) NA_real_ else scores$export_help * 100
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
    pkgs_df$has_examples <- if(is.null(scores$has_examples)) NA_real_ else scores$has_examples * 100
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
  
  cat("\n\n--> Using the 'rule sets' for the following metrics:\n")
  cat("\n---->", paste(decisions_df$metric |> unique(), collapse = '\n----> '), "\n")
  
  
  #
  # ---- Apply Decisions ----
  #
  
  # ---- 'Primary' Metrics ----
  primary_metrics <- decisions_df |>
    dplyr::filter(tolower(metric_type) == "primary") |>
    # dplyr::filter(tolower(metric) %in% c("downloads_1yr", "reverse_dependencies")) |>
    # dplyr::filter(tolower(metric) %in% c("covr_coverage")) |>
    dplyr::mutate(derived_col = metric)
  
  # Share a note
  prime_met_len <- primary_metrics$metric |> unique() |> length()
  cat(glue::glue("\n\n> Applying Decisions Categories for {prime_met_len} 'Primary' risk metric(s).\n\n"))
  
  # else_cat <- "High" # for debugging
  
  # Generate case_when()'s
  cond_exprs <- get_case_whens(
    met_dec_df = primary_metrics,
    met_names = primary_metrics$metric |> unique(),
    else_cat = else_cat
    )
  cond_exprs_ids <- get_case_whens(
    met_dec_df = primary_metrics, met_names = primary_metrics$metric |> unique(),
    else_cat = else_cat,
    ids = TRUE
  )
  cond_exprs_aa <- get_case_whens(
    met_dec_df = primary_metrics,
    met_names = primary_metrics |> dplyr::filter(!is.na(auto_accept)) |> distinct(derived_col) |> pull(derived_col),
    else_cat = else_cat,
    auto_accept = TRUE
  )
  
  dec_id_df <- unique(primary_metrics[,c("decision", "decision_id")])
  
  # pkgs_df$dwnlds_cat <- NULL
  pkgs_primed <-
    pkgs_df |>
    dplyr::rowwise() |> 
    dplyr::mutate(!!! cond_exprs) |>
    dplyr::mutate(!!! cond_exprs_ids) %>%
    {if(length(cond_exprs_aa) > 0) dplyr::mutate(., !!! cond_exprs_aa) else .} |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # convert cat vars into factors so we can use pmax() on them
      across(ends_with("_cat"), ~ factor(.x, levels = decisions)), 
      
      # if any column ending in "_cataa" is TRUE, then set final_risk_catid to 1 (Low)
      final_risk_cataa = ifelse(rowSums(across(ends_with("_cataa"), ~ .x), na.rm = TRUE) > 0, 1, NA_integer_),
      
      # higher risk trumps lower risk amongst all _cat vars (e.g., High > Medium > Low)
      max_catid = pmax(!!!rlang::syms(paste0(unique(primary_metrics$derived_col), "_catid")), na.rm = TRUE) |> as.integer(),
      final_risk_catid = dplyr::case_when(
        !is.na(final_risk_cataa) ~ final_risk_cataa,
        is.finite(max_catid) ~ max_catid,
        .default = as.integer(NA) # if this happens, need to investigate!
      ),
      final_risk_cat = decision_to_id_v(dec_id_df, rev = TRUE, final_risk_catid)
    ) |>
    dplyr::select(-c(ends_with("_catid"), "final_risk_cataa"))
  

  # everything below hasn't really been worked out yet.
  pkg_source = "cran"
  if(tolower(pkg_source) == "github") {
    
  
    # ---- 'Secondary' Metrics ----
    # Used for GitHub hosted pkgs, etc.
    secondary_metrics <- decisions_df |>
      dplyr::filter(tolower(metric_type) != "primary") 
    
    sec_met_len <- secondary_metrics$metric |> unique() |> length()
    
    if(nrow(secondary_metrics > 0)){
      
      cat(glue::glue("\n\n> Applying Decisions Categories to {sec_met_len} 'Exception' risk metric(s).\n\n"))
      
      
      # Create metric-based risk categories decision columns
      # rm(pkgs_final)
      pkgs_sec_cats <- rip_cats(
        met_dec_df = secondary_metrics,
        pkgs_df =  
          pkgs_primed |>
          dplyr::rename(primary_risk_category = final_risk_cat) |>
          dplyr::select(-dplyr::ends_with("_cat")),
        else_cat = else_cat
      ) |>
        dplyr::rename(exception_risk_category = final_risk_cat)
      
      #
      # ---- Promote 'Exceptions' ----
      #
      promos <-
        pkgs_sec_cats |>
        
        # create final_risk variable that reduces the decision category
        # from primary_risk_category if exception_risk_category is the lowest risk
        dplyr::mutate(
          final_risk_id = 
            ifelse(
              as.integer(exception_risk_category) == 1 & as.integer(primary_risk_category) > 1,
              as.integer(primary_risk_category) - 1,
              as.integer(primary_risk_category)
            ),
          final_risk = final_risk_id |> factor(labels = decisions),
        ) |>
        dplyr::select(
          package, version, 
          primary_risk_category, exception_risk_category, final_risk, 
          # final_risk_manual, 
          dplyr::everything(),
          -final_risk_id, # keep id?
        ) 
      
      # Make note of Pkgs that shifted thanks to exceptions
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
      
      pkgs_final <- promos |>
        dplyr::select(-primary_risk_category, -exception_risk_category)
    }
    
  } else {
    
    pkgs_final <- pkgs_primed |>
      dplyr::select(
        package, final_risk = final_risk_cat,
        dplyr::everything()
      )
  }
  
  #
  # ---- Return data for filtering (presumably)
  # 
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
#' file had assessments, we'd be using that (paired with {val.filter}), but
#' instead, we're going to use riskscore::cran_assessed_20250812 for the time
#' being
#'
#' @param source character, either "riskscore" (default), "PACKAGES", or a
#'   data.frame.
#' @param avail_pkgs data.frame, the output of available.packages() as a data.frame
#' @param decisions character vector, the risk categories to use.
#' @param else_cat character, the default risk category if no conditions are met.
#' @param decisions_df data.frame, the output of build_decisions_df()
#'
#' @importFrom dplyr filter pull mutate case_when between rename left_join
#'   across if_else everything select
#' @importFrom purrr map map_int 
#' @importFrom tools package_dependencies
#' 
#' 
val_categorize <- function(
    source = "riskscore",
    avail_pkgs = available.packages() |> as.data.frame(),
    decisions = c("Low", "Medium", "High"),
    else_cat = "High",
    decisions_df = build_decisions_df(rule_type = "remote_reduce")
) {
  # @importFrom riskscore cran_assessed_20250812 cran_scored_20250812
  
  cat("\n\nCategorizing available packages. Starting w/", nrow(avail_pkgs), "pkgs.\n")
  
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
    #                         ref = "dev")
    pv <- packageVersion("riskscore") # verify ‘v0.0.2'
    riskscore_run_date <- riskscore::cran_assessed_latest$riskmetric_run_date |> unique()
    cat(paste0("\n--> Using {riskscore} Version: 'v", pv, "', last compiled on '",
               riskscore_run_date,"'.\n"))
    if(Sys.Date() - as.Date(riskscore_run_date) > 60) {
      cat(glue::glue("\n!!! WARNING: the latest riskscore assessment date is more than 60 days old, compared to today's validation date. Consider updating {{riskscore}} w/ a fresh run.\n"))
    }
    

    # Sometimes, the package field is a list() in the 2025-10-01 riskscore
    # build. So, we need to unlist it first
    if(inherits(riskscore::cran_assessed_latest$package, "list")){
      package_col <- riskscore::cran_assessed_latest$package |> unlist()
      ver_col <- riskscore::cran_assessed_latest$version |> unlist()
    } else {
      # package_col <- riskscore::cran_assessed_20250812$package
      # ver_col <- riskscore::cran_assessed_20250812$version
      package_col <- riskscore::cran_assessed_latest$package
      ver_col <- riskscore::cran_assessed_latest$version
    }
    
    # How many pkgs are in the riskscore data vs available.packages()?
    cat(glue::glue("\n--> There are {prettyNum(nrow(riskscore::cran_assessed_latest), big.mark = ',')} packages with assessments in the latest riskscore data & {prettyNum(nrow(avail_pkgs), big.mark = ',')} packages available on CRAN.\n"))

    # Pkgs not in available.packages()
    missing_ap <- package_col[!(package_col %in% avail_pkgs$Package)]
    if(length(missing_ap) > 0) {
      cat(glue::glue("\nNote: There are {length(missing_ap)} packages in the riskscore data that are NOT in available.packages(). These will be marked ignored, because they have likely been removed from CRAN since the last riskscore build.\n\n"))
      print(head(missing_ap, 20))
      if(length(missing_ap) > 20) cat("...\n")
    }
    
    # Pkgs not in riskscore data
    # These are a little more problematic because we don't have assessments for them
    # So by default, do we need to mark them as "low" so that they are evaluated by
    # val_build() using pkg_sources? Yes!
    missing_rs <- avail_pkgs$Package[!(avail_pkgs$Package %in% package_col)]
    if(length(missing_rs) > 0) {
      cat(glue::glue("\n!!! WARNING: There are {length(missing_rs)} packages in available.packages() that are NOT in the riskscore data.\n"))
      print(head(missing_rs, 20))
      if(length(missing_rs) > 20) cat("...\n")
    }
    # options("repos")
    # dput(missing_rs)
    # avail_pkgs$Repository |> table()
    
    pkgs <- avail_pkgs |>
      dplyr::select(package = Package, version = Version) |>
      dplyr::left_join(
        riskscore::cran_assessed_latest |>
          # if the package field is a list(), the convert it to character
          dplyr::mutate(auto_pass = FALSE, package = package_col, version = ver_col) |>
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
        riskscore::cran_scored_latest |>
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
      pkgs$n_vig <- purrr::map_dbl(pkgs$has_vignettes, ~ {
        if(is.null(.x[[1]])) NA_real_ else as.numeric(.x[[1]])
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
  
  
  cat("\n\n--> Building decision data.frame using the 'rule sets' for the following metrics:\n")
  cat("\n---->", paste(decisions_df$metric |> unique(), collapse = '\n----> '), "\n")
  
  
  #
  # ---- Apply Decisions ----
  #
  

  # ---- 'Primary' Metrics ----
  primary_metrics <- decisions_df |>
    dplyr::filter(tolower(metric_type) == "primary")
    # dplyr::filter(tolower(metric) %in% c("downloads_1yr", "reverse_dependencies"))
    # dplyr::filter(tolower(metric) %in% c("has_website"))
  
  # Share a note
  prime_met_len <- primary_metrics$metric |> unique() |> length()
  cat(glue::glue("\n\n> Applying Decisions Categories for {prime_met_len} 'Primary' risk metric(s).\n\n"))
  
  # Create metric-based risk categories decision columns
  pkgs_primed <- rip_cats(
    met_dec_df = primary_metrics,
    pkgs_df = pkgs,
    else_cat = else_cat
  )
  
  

  # ---- 'Exceptions' ----
  # Exceptions to this? Perhaps some 'high' risk pkgs
  # could move to 'medium' if they have other outstanding metrics? Similarly,
  # "Medium" could move to "Low". Also, there may be some exception metrics
  # that will auto-accept a pkg to "Low" risk.
  exception_metrics <- decisions_df |>
    dplyr::filter(tolower(metric_type) != "primary") 
  
  exc_met_len <- exception_metrics$metric |> unique() |> length()
  
  if(nrow(exception_metrics > 0)){
    
    cat(glue::glue("\n\n> Applying Decisions Categories to {exc_met_len} 'Exception' risk metric(s).\n\n"))
    
    
    # Create metric-based risk categories decision columns
    # rm(pkgs_final)
    pkgs_exc_cats <- rip_cats(
      met_dec_df = exception_metrics,
      pkgs_df =  
        pkgs_primed |>
          dplyr::rename(primary_risk_category = final_risk_cat) |>
          dplyr::select(-c(dplyr::ends_with("_cat"), dplyr::ends_with("_cataa"))),
      else_cat = else_cat
      ) |>
      dplyr::rename(exception_risk_category = final_risk_cat)

    #
    # ---- Promote 'Exceptions' ----
    #
    promos <-
      pkgs_exc_cats |>
      
      # create final_risk variable that reduces the decision category
      # from primary_risk_category if exception_risk_category is the lowest risk
      dplyr::mutate(
        final_risk_id = 
          ifelse(
            as.integer(exception_risk_category) == 1 & as.integer(primary_risk_category) > 1,
            as.integer(primary_risk_category) - 1,
            as.integer(primary_risk_category)
          ),
        final_risk = final_risk_id |> factor(labels = decisions),
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
    
    pkgs_final <- promos |>
      dplyr::select(-primary_risk_category, -exception_risk_category)
    
  } else {
    pkgs_final <- pkgs_primed |>
      # if auto_pass is TRUE, then set final_risk to "Low"
      dplyr::mutate(
        final_risk = ifelse(auto_pass, decisions[1], as.character(final_risk_cat)) |> factor(levels = decisions)
      ) |>
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












#' Filter Packages based on Hardcoded values
#'
#' First whack at attempting to filter packages based on org-level criterion
#' used to set thresholds (and later update final decision (if not already 'high
#' risk')) AND filter packages before running val_build(). Note: If PACKAGES
#' file had assessments, we'd be using that (paired with {val.filter}), but
#' instead, we're going to use riskscore::cran_assessed_20250812 for the time
#' being
#'
#' @param pre logical, whether or not we are using pre or post filtering logic.
#'   May remove this later.
#' @param source character, either "riskscore" (default), "PACKAGES", or a
#'   data.frame.
#' @param avail_pkgs data.frame, the output of available.packages() as a data.frame
#'
#' @importFrom dplyr filter pull mutate case_when between rename left_join
#'   across if_else everything
#' @importFrom purrr map map_int
#' @importFrom tools package_dependencies
#' 
old_manual_val_categorize <- function(
    pre = TRUE,
    source = "riskscore",
    avail_pkgs = available.packages() |> as.data.frame(),
    decision_cats = c("Low", "Medium", "High"),
    metrics = c("downloads_1yr", "reverse_dependencies",
                "dependencies", "news_current", "bugs_status",
                "has_vignettes", "has_source_control", "has_website"
    )#,
    # exceptions_df = NULL,
    ) {
  # @importFrom riskscore cran_assessed_20250812 cran_scored_20250812
  
  cat("\n\nFiltering available packages. Starting w/",nrow(avail_pkgs),"pkgs.\n")
  
  
  # Use package metrics based on specified source
  if(length(source) > 1) {
    stop("Invalid source specified. Must be one of 'riskscore', 'PACKAGES', or a data.frame")
  } else if(source == "riskscore") {
    
    requireNamespace("riskscore", quietly = TRUE)
    # Use "pkg_cran_remote" data from riskscore::cran_assessed_20250812
    # remotes::install_github("pharmar/riskscore", force = TRUE,
    #                         ref = "main")
    pv <- packageVersion("riskscore") # verify ‘v0.0.1'
    cat(paste0("\n--> Using {riskscore} Version: 'v", pv, "'\n"))
    
    pkgs <- avail_pkgs |>
      dplyr::select(package = Package, version = Version) |>
      dplyr::left_join(
        riskscore::cran_assessed_latest |>
          dplyr::select(package, version, 
                        downloads_1yr, reverse_dependencies,
                        has_vignettes, has_source_control, has_website,
                        news_current, bugs_status
          ),
        by = c("package", "version")
      )
    pkgs_scored <- 
      avail_pkgs |>
      dplyr::select(package = Package, version = Version) |>
      dplyr::left_join(
        riskscore::cran_scored_latest |>
          dplyr::select(package, version, news_current, bugs_status),
        by = c("package", "version")
      )
    # object.size(pkgs) / 1000000000
  } else if (is.data.frame(source)) {
    pkgs <- avail_pkgs |>
      dplyr::select(package = Package, version = Version) |>
      dplyr::left_join(
        source |>
          dplyr::select(package, version, 
                        downloads_1yr, reverse_dependencies,
                        has_vignettes, has_source_control, has_website,
                        news_current, bugs_status
          ),
        by = c("package", "version")
      )
    pkgs_scored <- 
      avail_pkgs |>
      dplyr::select(package = Package, version = Version) |>
      dplyr::left_join(
        source |>
          dplyr::select(package, version, news_current, bugs_status),
        by = c("package", "version")
      )
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
  # That means this area will not be used for setting decisions, because
  # those decisions can only be made on the 'source' ref assessments. If
  # a pkg is 'high risk', we should automatically create the metadata list
  # necessary.
  #
  # In the future, we'd like to use {val.filter} here to get our subset.
  # We'd also like to be able to extract the filtering logic as needed
  
  
  #
  # ---- Annual Downloads ----
  #
  # This will be the #1 decider for filtering packages
  # extract list values into a numeric vector
  # pkgs$downloads_1yr[[7]] |> unlist()
  # [[1]]
  if("downloads_1yr" %in% metrics) {
    cat("\n--> Using 'downloads_1yr' metric to filter packages.\n")
    
    pkgs <- pkgs |>
      dplyr::mutate(
        dwnlds = ifelse(is.na(downloads_1yr), NA,
                        downloads_1yr |>
                          purrr::map(~.x[[1]]) |>
                          unlist() |>
                          as.numeric()
        )
      )
    
    # Overall summary
    # hist(pkgs$dwnlds, breaks = 5)
    # summary(is.na(pkgs$dwnlds)) # none
    # summary(pkgs$dwnlds)
    # summary(as.factor(pkgs$dwnlds))
    # round(prop.table(summary(as.factor(pkgs$dwnlds))), 3) * 100
    
    
    # Workshop a threshold
    low_dwnlds <- 80000
    med_dwnlds <- 40000
    pkgs$dwnlds_cat <- dplyr::case_when(
      is.na(pkgs$dwnlds) ~ "High", # no data, so assume high risk
      pkgs$dwnlds > low_dwnlds ~ "Low",
      dplyr::between(pkgs$dwnlds, med_dwnlds, low_dwnlds) ~ "Medium",
      .default = "High"
    )
    # table(pkgs$dwnlds_cat)
    # round(prop.table(table(pkgs$dwnlds_cat)), 3) * 100
    
    
    # Downloads alone
    cat("\n--> Download Risk decisions based off 'downloads_1yr' metric alone:\n")
    metric_lab <- "" # "dwnlds/yr"
    print(
      pkgs$dwnlds_cat |>
        factor(levels = c("Low", "Medium", "High")) |>
        table() |>
        as.data.frame() |>
        dplyr::left_join(
          {round(prop.table(table(pkgs$dwnlds_cat)), 3) * 100} |>
            as.data.frame(),
          by = "Var1"
        ) |>
        dplyr::mutate(Condition = dplyr::case_when(
          is.na(Var1) ~ "Decision NA", # no data, so assume high risk
          Var1 == "Low" ~ paste(">", prettyNum(low_dwnlds, big.mark = ","), metric_lab),
          Var1 == "Medium" ~ paste0(
            "[", prettyNum(med_dwnlds, big.mark = ","),
            ", ", 
            prettyNum(low_dwnlds, big.mark = ","), "] ", metric_lab
          ),
          .default = paste("<", prettyNum(med_dwnlds, big.mark = ","), metric_lab)
        )) |>
        dplyr::select(Risk = Var1, Condition, Cnt = Freq.x, Pct = Freq.y)
    )
  } 
  
  
  
  
  #
  # ---- Exceptions ----
  #
  # Exceptions to this? Perhaps some 'high' risk pkgs
  # could move to 'medium' if they have other outstanding metrics?
  
  # create an empty data.frame cataloging exceptions
  # This is more or less a placeholder until we can get the riskassessment
  # 'automated decisions' ui functionality into {val.filter}
  exdf <- data.frame(
    metric_name = character(),
    metric_lab = character(),
    low_cutoff = numeric(),
    medium_cutoff = numeric(),
    conds = character(),
    exception_cats = character(),
    stringsAsFactors = FALSE
  )
  
  
  #
  # ---- Reverse Dependencies ----
  #
  # extract list values into a numeric vector
  # cran_assessed$reverse_dependencies[[7]]
  
  if("reverse_dependencies" %in% metrics) {
    pkgs$rev_deps <- pkgs$reverse_dependencies |>
      purrr::map(~length(.x[[1]])) |>
      unlist() |>
      as.numeric()
    
    # Overall summary
    # hist(pkgs$rev_deps, breaks = 5)
    # summary(pkgs$rev_deps)
    # summary(as.factor(pkgs$rev_deps))
    # round(prop.table(summary(as.factor(pkgs$rev_deps))), 3) * 100
    
    
    # Workshop a threshold
    exrd <- 
      data.frame(
        metric_name = "reverse_dependencies",
        metric_lab = "", # "rev deps",
        low_cutoff = 7,
        medium_cutoff = 2,
        exception_cats = 'c("Low", "Medium")'
      ) |>
      dplyr::mutate(
        conds = paste(
          "Low: >", low_cutoff, metric_lab, "|",
          "Medium: [", medium_cutoff, ", ", low_cutoff, "]", metric_lab, "|",
          "High: <", medium_cutoff,  metric_lab
        )
      )
    exdf <- exdf |> dplyr::bind_rows(exrd) # add to exceptions df
    pkgs$rev_deps_cat <- dplyr::case_when(
      is.na(pkgs$rev_deps) ~ "High", # no data, so assume high risk
      pkgs$rev_deps > exrd$low_cutoff ~ "Low",
      dplyr::between(pkgs$rev_deps, exrd$medium_cutoff, exrd$low_cutoff) ~ "Medium",
      .default = "High"
    )
  }
  
  # table(pkgs$rev_deps_cat)
  # round(prop.table(table(pkgs$rev_deps_cat)), 3) * 100
  # str(pkgs$rev_deps_cat )
  
  #
  # ---- Dependencies ----
  #
  # Too many dependencies is problematic ... ?
  # pkgs$deps <- pkgs$dependencies[[1]] |>
  #   purrr::map(~.x[[1]]) |>
  #   unlist() |>
  #   as.numeric()
  
  # For some reason, the dependencies metric didn't come thru in `riskscore`,
  # but we don't need it since that info is available in `available.packages()`
  
  # It can't depend on a version of R that is more higher than the R version we
  # are evaluating right now
  # depends_base <- avail_pkgs |>
  #   dplyr::select(Package, Version, Depends) |>
  #   dplyr::filter(stringr::str_detect(Depends, r_ver)) |>
  
  
  if("dependencies" %in% metrics) {
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
    # dep_1 <- pkgs$package[pkgs$n_deps == 1]
    
    # Overall summary
    # hist(pkgs$n_deps, breaks = 5)
    # summary(pkgs$n_deps)
    # summary(as.factor(pkgs$n_deps))
    # round(prop.table(summary(as.factor(pkgs$n_deps))), 3) * 100
    
    # Workshop a threshold
    exdeps <- 
      data.frame(
        metric_name = "dependencies",
        metric_lab = "", # "deps",
        low_cutoff = 4,
        medium_cutoff = 8,
        exception_cats = 'c("Low", "Medium")'
      ) |>
      dplyr::mutate(
        conds = paste(
          "Low: <", low_cutoff, metric_lab, "|",
          "Medium: [", medium_cutoff, ", ", low_cutoff, "]", metric_lab, "|",
          "High: >", medium_cutoff,  metric_lab
        )
      )
    exdf <- exdf |> dplyr::bind_rows(exdeps) # add to exceptions df
    pkgs$n_deps_cat <- dplyr::case_when(
      is.na(pkgs$n_deps) ~ "High", # no data, so assume high risk
      pkgs$n_deps < exdeps$low_cutoff ~ "Low",
      dplyr::between(pkgs$n_deps, exdeps$low_cutoff, exdeps$medium_cutoff) ~ "Medium",
      .default = "High"
    )
    # table(pkgs$n_deps_cat)
    # round(prop.table(table(pkgs$n_deps_cat)), 3) * 100
  }
  
  #
  # ---- Bug status ----
  #
  # # {riskscore} deemed not reliable here
  # 
  # # extract list values into a numeric vector
  # # cran_assessed$reverse_dependencies[[7]]
  # pkgs$bug_stat_d <- pkgs$bugs_status |>
  #   purrr::map(~length(.x[[1]])) |>
  #   unlist()
  # 
  # 
  # pkgs <- pkgs |>
  #   # dplyr::rename(version = version.x) |>
  #   dplyr::left_join(
  #     pkgs_scored |>
  #       dplyr::select(package, version, bug_stat = bugs_status),
  #     by = c("package", "version")
  #   )
  # 
  # # Overall summary
  # # hist(as.numeric(pkgs$bug_stat), breaks = 5)
  # # summary(as.numeric(pkgs$bug_stat))
  # # summary(as.factor(as.numeric(pkgs$bug_stat)))
  # # round(prop.table(summary(as.factor(as.numeric(pkgs$bug_stat)))), 3) * 100
  # 
  # # Workshop a threshold
  # pkgs$bug_stat_cat <- dplyr::case_when(
  #   is.na(pkgs$bug_stat) ~ "High", # no data, so assume high risk
  #   as.numeric(pkgs$bug_stat) > .49999 ~ "Low",
  #   dplyr::between(as.numeric(pkgs$bug_stat), .10, .49999) ~ "Medium",
  #   .default = "High"
  # )
  # table(is.na(pkgs$bug_stat)) # this is not reliable
  # table(pkgs$bug_stat_cat)
  # round(prop.table(table(pkgs$bug_stat_cat)), 3) * 100
  
  
  
  #
  # ---- news_current ----
  #
  # extract list values into a numeric vector
  # library(dplyr)
  # is.logical(pkgs$news_current[[11]][[1]])
  # pkgs <- pkgs |>
  #   dplyr::mutate(dplyr::across(c(news_current), ~ if("pkg_metric_error" %in% class(.x[[1]])) "pkg_metric_error" else .x)) #|>
  #   dplyr::mutate(
  #     news_curr =
  #       if("pkg_metric_error" %in% class(news_current[[1]])) "pkg_metric_error" else news_current[[1]])
  #       unlist() %>%
  #       dplyr::if_else(!is.logical(.), logical(0), .)
  #   )
  # str(pkgs$news_curr)
  # failing above
  
  # Try again with scored df
  # pkgs$news_curr <- NULL
  
  if("news_current" %in% metrics) {
    pkgs <- pkgs |>
      # dplyr::rename(version = version.x) |>
      dplyr::left_join(
        pkgs_scored |>
          dplyr::mutate(news_curr = as.numeric(news_current)) |>
          dplyr::select(package, version, news_curr),
        by = c("package", "version")
      )
    
    # Overall summary
    # table(is.na(pkgs$news_curr))
    # table(pkgs$news_curr) # this is not reliable
    # round(prop.table(table(pkgs$news_curr)), 3) * 100
    
    # Workshop a threshold
    exnc <- 
      data.frame(
        metric_name = "news_current",
        metric_lab = "",
        low_cutoff = 1,
        medium_cutoff = NA,
        exception_cats = '"Low"'
      ) |>
      dplyr::mutate(
        conds = paste(
          "Low: == ", low_cutoff, metric_lab, "|",
          "High: !=", low_cutoff,  metric_lab
        )
      )
    exdf <- exdf |> dplyr::bind_rows(exnc) # add to exceptions df
    pkgs$news_curr_cat <- dplyr::case_when(
      is.na(pkgs$news_curr) ~ "High", # no data, so assume high risk
      pkgs$news_curr == exnc$low_cutoff ~ "Low",
      .default = "High"
    )
    # str(pkgs$news_curr_cat)
    # table(pkgs$news_curr_cat)
    # round(prop.table(table(pkgs$news_curr_cat)), 3) * 100
  }
  
  
  #
  # ---- has_vignettes ----
  #
  
  if("has_vignettes" %in% metrics) {
    # is.integer(pkgs$has_vignettes[[11]][[1]])
    pkgs$n_vig <- 
      ifelse(is.na(pkgs$has_vignettes), NA,
        pkgs$has_vignettes |>
        purrr::map(~.x[[1]]) |>
        unlist()
      )
    # str(pkgs$n_vig)
    
    # Overall summary
    # hist(pkgs$n_vig, breaks = 5)
    # summary(pkgs$n_vig)
    # summary(as.factor(pkgs$n_vig))
    # round(prop.table(summary(as.factor(pkgs$n_vig))), 3) * 100
    
    # Workshop a threshold
    exvig <- 
      data.frame(
        metric_name = "has_vignettes",
        metric_lab = "", # "article",
        low_cutoff = 1,
        medium_cutoff = NA,
        exception_cats = '"Low"'
      ) |>
      dplyr::mutate(
        conds = paste(
          "Low: >", low_cutoff, metric_lab, "|",
          "Medium: ==", low_cutoff, metric_lab, "|",
          "High: <", low_cutoff,  metric_lab
        )
      )
    exdf <- exdf |> dplyr::bind_rows(exvig) # add to exceptions df
    pkgs$n_vig_cat <- dplyr::case_when(
      is.na(pkgs$n_vig) ~ "High", # no data, so assume high risk
      pkgs$n_vig > exvig$low_cutoff ~ "Low",
      pkgs$n_vig == exvig$low_cutoff ~ "Medium",
      .default = "High"
    )
    # table(pkgs$n_vig_cat)
    # round(prop.table(table(pkgs$n_vig_cat)), 3) * 100
  }
  
  #
  # ---- Has Source Control ----
  #
  
  if("has_source_control" %in% metrics) {
    # is.integer(pkgs$has_source_control[[11]][[1]] |> length())
    pkgs$src_cntrl <- pkgs$has_source_control |>
      purrr::map(~.x[[1]] |> length()) |>
      unlist()
    # str(pkgs$n_vig)
    
    # Overall summary
    # hist(pkgs$src_cntrl, breaks = 5)
    # summary(pkgs$src_cntrl)
    # summary(as.factor(pkgs$src_cntrl))
    # round(prop.table(summary(as.factor(pkgs$src_cntrl))), 3) * 100
    
    # Workshop a threshold
    exsrc <- 
      data.frame(
        metric_name = "has_source_control",
        metric_lab = "", # "src cntrl",
        low_cutoff = 0,
        medium_cutoff = NA,
        exception_cats = '"Low"'
      ) |>
      dplyr::mutate(
        conds = paste(
          "Low: >", low_cutoff, metric_lab, "|",
          "High: ==", low_cutoff,  metric_lab
        )
      )
    exdf <- exdf |> dplyr::bind_rows(exsrc) # add to exceptions df
    pkgs$src_cntrl_cat <- dplyr::case_when(
      is.na(pkgs$src_cntrl) ~ "High", # no data, so assume high risk
      pkgs$src_cntrl > exsrc$low_cutoff ~ "Low",
      .default = "High"
    )
    # table(pkgs$src_cntrl_cat)
    # round(prop.table(table(pkgs$src_cntrl_cat)), 3) * 100
  }
  
  #
  # ---- has_website ----
  #
  if("has_website" %in% metrics) {
    # is.integer(pkgs$has_website[[11]][[1]] |> length())
    pkgs$n_sites <- pkgs$has_website |>
      purrr::map(~.x[[1]] |> length()) |>
      unlist()
    # str(pkgs$n_vig)
    
    # Overall summary
    # hist(pkgs$n_sites, breaks = 5)
    # summary(pkgs$n_sites)
    # summary(as.factor(pkgs$n_sites))
    # round(prop.table(summary(as.factor(pkgs$n_sites))), 3) * 100
    
    # Workshop a threshold
    exsite <- 
      data.frame(
        metric_name = "has_website",
        metric_lab = "", # "site",
        low_cutoff = 0,
        medium_cutoff = NA,
        exception_cats = '"Low"'
      ) |>
      dplyr::mutate(
        conds = paste(
          "Low: >", low_cutoff, metric_lab, "|",
          "High: ==", low_cutoff,  metric_lab
        )
      )
    exdf <- exdf |> dplyr::bind_rows(exsite) # add to exceptions df
    pkgs$website_cat <- dplyr::case_when(
      is.na(pkgs$n_sites) ~ "High", # no data, so assume high risk
      pkgs$n_sites > exsite$low_cutoff ~ "Low",
      .default = "High"
    )
    # table(pkgs$website_cat)
    # round(prop.table(table(pkgs$website_cat)), 3) * 100
  }
  
  #
  # ---- Filter ----
  # 
  
  # Exceptions
  # If reverse dependencies are "low" risk, AND
  # dependencies are "low" risk, AND
  # code coverage is "low" risk,
  # then, bump lower risk level
  # exceptions <- function(data) {
  #   pkgs$rev_deps_cat %in% c("Low", "Medium") &
  #     pkgs$n_deps_cat %in% c("Low", "Medium") &
  #     # pkgs$bug_stat_cat == "Low" & # {riskscore} data not usable
  #     pkgs$news_curr_cat &
  #     pkgs$n_vig_cat == "Low" &
  #     pkgs$src_cntrl_cat == "Low" &
  #     pkgs$website_cat == "Low" 
  # }
  
  pull_ex <- function(metric) {
    eval(parse(text=
                 dplyr::filter(exdf, metric_name == metric) |>
                 dplyr::pull(exception_cats)
    )) 
  }
  rev_deps_excat <- pull_ex("reverse_dependencies") 
  n_deps_excat <-  pull_ex("dependencies")
  news_curr_excat <-  pull_ex("news_current") 
  n_vig_excat <-  pull_ex("has_vignettes")
  src_cntrl_excat <-  pull_ex("has_source_control")
  site_excat <-  pull_ex("has_website")
  
  # pkgs$final_risk <- NULL
  pkgs <-
    pkgs |>
    dplyr::mutate(
      final_risk = 
        dplyr::case_when(
          
          dwnlds_cat == "Medium" &
            # exceptions()
            rev_deps_cat %in% rev_deps_excat &
            n_deps_cat %in% n_deps_excat &
            # pkgs$bug_stat_cat == "Low" & # {riskscore} data not usable
            news_curr_cat == news_curr_excat &
            n_vig_cat == n_vig_excat &
            src_cntrl_cat == src_cntrl_excat &
            website_cat == site_excat ~ "Low",
          
          dwnlds_cat == "High" &
            dwnlds > 10000 & # snuck this in
            
            # exceptions()
            rev_deps_cat %in% rev_deps_excat &
            n_deps_cat %in% n_deps_excat &
            # pkgs$bug_stat_cat == "Low" & # {riskscore} data not usable
            news_curr_cat == news_curr_excat &
            n_vig_cat == n_vig_excat &
            src_cntrl_cat == src_cntrl_excat &
            website_cat == site_excat ~ "Medium",
          
          .default = dwnlds_cat
        )
    ) |>
    dplyr::select(package, version, final_risk, dplyr::everything())
  
  # Pkgs that shifted thanks to exceptions
  cat("\n--> Exceptions to 'downloads_1yr'-based decisions based on meeting ALL of the following metric criterion:\n\n")
  diff_table <- {
    pkgs$final_risk |>
      factor(levels = c("Low", "Medium", "High")) |>
      table()
  } - {
    pkgs$dwnlds_cat |>
      factor(levels = c("Low", "Medium", "High")) |>
      table()
  }
  print(
    exdf |>
      dplyr::select(Metric = metric_name, Conditions = conds, `Exception If` = exception_cats)
  )
  cat("\n")
  print(
    diff_table |>
    as.data.frame() |>
    dplyr::rename(`Risk Shifted` = Var1, Added = Freq)
  )
  
  
  # Final pkg counts in each risk category
  build_pkgs_len <-
    pkgs |>
    dplyr::filter(!final_risk %in% c("High")) |>
    dplyr::pull(package) |>
    length()
  
  cat("\n--> Final", if(pre) "'pre'" else "post -","assessment risk decision counts based off annual downloads w/ exceptions: \n----> Returned", prettyNum(build_pkgs_len, big.mark = ","), "pkgs for build.\n")
  print(
    pkgs$final_risk |>
      factor(levels = c("Low", "Medium", "High")) |>
      table() |>
      as.data.frame() |>
      dplyr::left_join(
        {round(prop.table(table(pkgs$final_risk)), 3) * 100} |>
          as.data.frame(),
        by = "Var1"
      ) |>
      dplyr::rename("Final Risk" = Var1, Cnt = Freq.x, Pct = Freq.y)
  )
  
  return(pkgs)
}
