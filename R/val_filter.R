





#' Filter Packages
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
val_filter <- function(
    pre = TRUE,
    source = "riskscore",
    avail_pkgs = available.packages() |> as.data.frame(),
    decisions = c("Low", "Medium", "High"),
    else_cat = "High",
    decisions_df = build_decisions_df()
) {
  # @importFrom riskscore cran_assessed_20250812 cran_scored_20250812
  
  cat("\n\nFiltering available packages. Starting w/", nrow(avail_pkgs), "pkgs.\n")
  
  # verify decisions_df is compliant
  if(!all(c("metric", "decision", "condition", "metric_type", "accept_condition") %in% colnames(decisions_df))) {
    stop("'decisions_df' is not compliant. Must contain columns: 'metric', 'decision', 'condition', 'metric_type', 'accept_condition'")
  }
  
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
    
    #
    # ---- Prepare workable fields to categorize & filter  ----
    #
    # This will be the #1 decider for filtering packages
    # extract list values into a numeric vector
    if("downloads_1yr" %in% decisions_df$metric) {
      pkgs <- pkgs |>
        dplyr::mutate(
          dwnlds = ifelse(is.na(downloads_1yr), NA,
                          downloads_1yr |>
                            purrr::map(~.x[[1]]) |>
                            unlist() |>
                            as.numeric()
          ))
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
      pkgs$n_vig <- 
        ifelse(is.na(pkgs$has_vignettes), NA,
               pkgs$has_vignettes |>
                 purrr::map(~.x[[1]]) |>
                 unlist()
        )
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
  

  # ---- Primary Metrics ----
  primary_metrics <- decisions_df |>
    dplyr::filter(tolower(metric_type) == "primary")
    # dplyr::filter(tolower(metric) %in% c("downloads_1yr", "reverse_dependencies"))
  rip_cats(
    met_dec_df = primary_metrics,
    pkgs_df = pkgs,
    else_cat = else_cat
  )
  
  
  # ---- Filter Pkgs on Primary Metrics ----
  # Initiate a final_risk column & subset pkgs df to those. Why subset? Well,
  # it will save us time running thru the Exceptions code block below.
  # Later, we will join this back to the full pkgs df
  pkgs_prime <-
    pkgs |>
    dplyr::filter(primary_risk_cat == decisions[1])
  
  build_pkgs_len <-
    pkgs |>
    dplyr::filter(!dwnlds_cat %in% c("High")) |>
    dplyr::pull(package) |>
    length()
  
  

  # ---- Exceptions ----
  # Exceptions to this? Perhaps some 'high' risk pkgs
  # could move to 'medium' if they have other outstanding metrics?
  exception_metrics <- decisions_df |>
    dplyr::filter(tolower(metric_type) != "primary") 
  
  if(nrow(exception_metrics > 0)){
    rip_cats(
      met_dec_df = exception_metrics,
      pkgs_df = pkgs,
      else_cat = else_cat
      )
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
manual_val_filter <- function(
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
