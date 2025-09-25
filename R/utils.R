#' Strip Recording (for list() objects)
#'
#' Remove .recording attribute from all elements of the assessment,
#'   whilst maintaining classes
#' 
#' @param assessment A list() object, typically the output of pkg_assess()
#' 
#' @examples
#' # riskmetric::pkg_ref("zoo", source = "pkg_cran_remote") |>
#' #   riskmetric::pkg_assess() |>
#' #   strip_recording()
#' 
#' @keywords internal
strip_recording <- function(assessment) {
  # assessment <- pkg_assessment0 # for debugging
  no_recording <-
    lapply(assessment, \(x) {
      # x <- assessment$covr_coverage # for debugging
      attributes(x)$.recording
      structure(
        x,
        .recording = NULL,
        class = setdiff(class(x),
                        "with_eval_recording")
      )
    })
  class(no_recording) <- class(assessment)
  no_recording
}




#' Strip Recording (Data Frame) - Deprecated
#'
#' Remove .recording attribute from all elements of the assessment,
#'   whilst maintaining classes
#' 
#' @param assessment A list() object, typically the output of pkg_assess()
#' 
#' @importFrom purrr walk
#' 
#' @examples
#' # riskmetric::pkg_ref("zoo", source = "pkg_cran_remote") |>
#' #   dplyr::as_tibble() |>
#' #   riskmetric::pkg_assess() |>
#' #   strip_recording_df()
#' 
#' @keywords internal
strip_recording_df <- function(assessment) {
  # assessment <- pkg_assessment0 |> dplyr::select(-c(1:3))# for debugging
  these_cols <- names(assessment)
  purrr::walk(these_cols, \(col_name) {
    # col_name <- these_cols[1] # for debugging
    # col_name <- 'news_current'
    cat("\n\nCol Name =", col_name, "\n")
    val <- assessment[[col_name]]
    lite_val <- 
        structure(
          val[[1]],
          .recording = NULL,
          class = setdiff(class(val[[1]]),
                          "with_eval_recording")
        )
    if(length(lite_val) != 1) {
      lite_val <- list(lite_val)
      attributes(lite_val) <- attributes(assessment[[col_name]])
    }

    # object.size(assessment[[col_name]])
    # object.size(lite_val)
    assessment[[col_name]] <<- lite_val
    })
  assessment
  # object.size(pkg_assessment0 |> dplyr::select(-c(1:3)))
  # object.size(assessment)
}



#' To the Limit
#'
#' Helper function to parse lower & upper limit out of the condition string
#' 
#' @param condition A character vector of conditions (as strings)
#' @param low Logical. If TRUE, extract the lower limit, else the upper limit
#' 
#' @importFrom purrr map_dbl
#' @importFrom rlang expr parse_expr is_call
#' 
#' @examples
#' to_the_limit("~ is.na(.x)", low = FALSE)
#' to_the_limit("~ .x < 120000", low = FALSE)
#' to_the_limit("~ dplyr::between(.x, 120000, 240000)", low = FALSE)
#' to_the_limit("~ .x > 240000", low = FALSE)
#' 
#' @keywords internal
to_the_limit <- function(condition, low = TRUE) {
  purrr::map_dbl(condition, \(cond){
    # cond <- decisions_df$condition[2] # for debugging
    # print(cond)
    expr <- rlang::expr(!!rlang::parse_expr(cond))
    if (rlang::is_call(expr[[2]], name = c("<", ">", "<=", ">="))) {
      limit <- as.numeric(expr[[2]][[3]])
      if (rlang::is_call(expr[[2]], name = c("<", "<="))) {
        return(if(low) -Inf else limit)
      } else {
        return(if(low) limit else Inf)
      }
    } else if (rlang::is_call(expr[[2]], name = "dplyr::between") | rlang::is_call(expr[[2]], name = "between")) {
      limit <- as.numeric(expr[[2]][[if(low) 3 else 4]])
      return(limit)
    } else if (rlang::is_call(expr[[2]], name = "==")) {
      limit <- as.numeric(expr[[2]][[3]])
      return(limit)
    } else if (rlang::is_call(expr[[2]], name = "is.na")) {
      return(NA_real_)
    } else {
      # warning(glue::glue("Could not parse lower limit from condition '{cond}'. Setting to NA."))
      return(NA_real_)
    }
  })
}




#' Build Decisions Data.Frame
#'
#' Helper function that creates the minimally necessary data.frame for
#' val_filter() that includes columns "metric", "decision", "condition",
#' "metric_type", and "accept_condition".
#'
#' @param decision_lst A character vector of decision categories, ordered from
#'   highest risk to lowest risk.
#' @param rules_lst A named list of lists, where each sub-list contains:
#' - cond: A named list of formulas, where the names correspond to decision
#'   categories in `decision_lst`, and the formulas define the conditions for
#'   each category.
#' - type: A character string indicating whether the metric is "primary" or
#'   "exception".
#' - accept_cats: A character vector of decision categories (& their conditions)
#'   that are acceptable for this metric.
#' - min_value: (Optional) A numeric value indicating the minimum acceptable
#'   value for the primary metric(s).
#'
#' @importFrom rlang expr_text is_empty
#' @importFrom dplyr mutate select row_number left_join filter distinct tibble
#' @importFrom purrr map map_chr imap_dfr pmap_chr pmap_lgl set_names
#' @importFrom glue glue
#'
#'
#' @examples build_decisions_df()
#'
#' @return A data.frame with columns:
#' - metric: The name of the metric.
#' - decision: The decision category.
#' - condition: The condition as a string.
#' - lower_limit: The lower limit of the condition.
#' - upper_limit: The upper limit of the condition.
#' - metric_type: The type of the metric ("Primary" or "Exception").
#' - accept_condition: A logical indicating whether the condition is acceptable.
#'
#' @export
build_decisions_df <- function(
    
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
  ) {
  
  #
  # Clean up rules list & decision categories 
  #

  # Verify that conditions (conds) are valid - aka, they only declared
  # decision categories (in `decision_list`). Else, we have to drop that rule
  rule_metric_nm <- names(rules_lst)
  metric_cond_cats <-
    purrr::map(rule_metric_nm, ~ rules_lst[[.x]][["cond"]] |> names()) |>
    purrr::set_names(rule_metric_nm)
  metric_nms <- names(metric_cond_cats)
  valid_cats_in_conds <-
    purrr::map_lgl(metric_nms, ~ {if(all(metric_cond_cats[[.x]] %in% decision_lst)) TRUE else FALSE}) |>
    purrr::set_names(metric_nms) 
  # valid_cats_in_conds <- metric_cond_cats |> unlist() %in% decision_lst
  if (any(!valid_cats_in_conds)) {
    metrics_which_conds_invalid <- valid_cats_in_conds[which(!valid_cats_in_conds)] |> names()
    warning(glue::glue("\nThe decision category(ies) referenced in these metric 'cond' list do not match those allowed in 'decision_lst': {paste(metrics_which_conds_invalid, collapse = ', ')}. Dropping those rules from 'rule_lst'.\n"))
    rules_lst <- rules_lst[valid_cats_in_conds]
  }
  
  # Verify that accept_cats are valid, else drop that rule.
  rule_metric_nm <- names(rules_lst)
  metric_exc_cats <- purrr::map(rule_metric_nm, ~ rules_lst[[.x]][["accept_cats"]]) |> purrr::set_names(rule_metric_nm)
  # valid_cats_in_exc <- metric_exc_cats |> unlist() %in% decision_lst # old
  metric_nms <- names(metric_exc_cats)
  valid_cats_in_exc <-
    purrr::map_lgl(metric_nms, ~ {if(all(metric_exc_cats[[.x]] %in% decision_lst)) TRUE else FALSE}) |>
    purrr::set_names(metric_nms) 
  if (any(!valid_cats_in_exc)) {
    metrics_which_exc_invalid <- valid_cats_in_exc[which(!valid_cats_in_exc)] |> names()
    warning(glue::glue("\nThe decision category(ies) referenced in these metric 'accept_cats' list do not match those allowed in 'decision_lst': {paste(metrics_which_exc_invalid, collapse = ', ')}. Dropping those rules from 'rule_lst'.\n"))
    rules_lst <- rules_lst[valid_cats_in_exc]
  }
  
  # Inform the user of which rules can & will be evaluated
  message("\n--> Building exceptions data.frame using the 'rule sets' for the following metrics:\n")
  cat("\n---->", paste(names(rules_lst), collapse = '\n----> '), "\n")

  
  # Build exceptions data.frame
  decisions_df0 <-
    
    # Use metric_cond_cats to build an initial a data frame with metric &
    # applicable decision categories.
    purrr::imap_dfr(metric_cond_cats, ~ {
      # .x <- metric_cond_cats[[1]]; .y <- names(metric_cond_cats)[1] # for debugging
      dplyr::tibble(
        metric = rep(.y, length(.x)),
        decision = .x
      )
    }) |>
    # Make sure the (factor) order is preserved
    dplyr::mutate(decision = factor(decision, levels = decision_lst)) |>
    
    # Create a numeric ID field that uniquely identifies decision within metric
    dplyr::mutate(decision_id = dplyr::row_number(), .by = c("metric", "decision")) |>
    
    # add condition from rules list into data.frame column
    dplyr::mutate(
      condition = purrr::pmap_chr(
        list(metric, decision, decision_id), \(met, dec, did) {
          # met <- "downloads_1yr"; dec <- "High"; did <- 2 # for debugging
          met_cond <- rules_lst[[met]][["cond"]] 
          met_cond[which(names(met_cond) %in% dec)][[did]] |> rlang::expr_text()
            # Or use rlang::expr_text()?
        })
    ) |> 
    # decisions_df$cond |> class()
    dplyr::mutate(
      # extract lower & Upper limit of the condition
      lower_limit = to_the_limit(condition),
      upper_limit = to_the_limit(condition, low = FALSE),
      # rationale = NA_character_,
      metric_type = ifelse(metric == "downloads_1yr", "Primary", "Exception")
    ) |>
    dplyr::select(-decision_id)
  
  
  # Add on the "accept_condition" column
  if (!rlang::is_empty(rules_lst)) {
    decisions_df <- 
      decisions_df0 |>
      # Join in accept_cats from rules list
      dplyr::left_join(
        purrr::imap_dfr(rules_lst, ~ {
          # .x <- rules_lst[[1]]; .y <- names(rules_lst)[1] # for debugging
          dplyr::tibble(
            metric = .y,
            accept_cats = list(.x[["accept_cats"]])
          )
        }),
        by = "metric"
      ) |>
      # Flag whether decision category is an exception category
      dplyr::mutate(
        accept_condition = purrr::pmap_lgl(
          list(decision, accept_cats), \(dec, accpt_cats) {
            # dec <- decisions_df$decision[1]; exc_cats <- decisions_df$accept_cats[[1]] # for debugging
            return(dec %in% accpt_cats)
          }
        )
      ) |>
      dplyr::select(-accept_cats)
  } else {
    decisions_df <- 
      decisions_df0 |>
      dplyr::mutate(
        accept_condition = if(tolower(metric_type) == "primary") TRUE else FALSE
      )
  }
  return(decisions_df)
}

#' Get Case Whens Statements
#'
#' A helper function that builds the dplyr::case_when() statements from the
#' decisions data.frame for each metric.
#' 
#' @param met_dec_df A data.frame of decisions, typically created by
#'  build_decisions_df()
#' @param met_names A character vector of the derived metric column names
#' (e.g., "downloads_1yr", "reverse_dependencies", etc.)
#' @param else_cat A character string indicating the default category to assign
#' when none of the conditions are met.
#' 
#' @importFrom purrr map_chr set_names
#' @importFrom glue glue
#' @importFrom stringr str_flatten_comma str_c
#' @importFrom rlang parse_exprs
#' @importFrom dplyr filter
#' 
#' @examples
#' get_case_whens(
#'   build_decisions_df() |> dplyr::mutate(derived_col = metric),
#'   c("downloads_1yr"),
#'   "High"
#' )
#' 
#' @keywords internal
get_case_whens <- function(met_dec_df, met_names, else_cat) {
  cond_exprs <- purrr::map_chr(met_names, ~ {
    # .x <- met_names[1]
    dec_df <- met_dec_df |>
      dplyr::filter(derived_col == .x)
    
    mn <- gsub('.x', .x, dec_df$condition)
    c(
      glue::glue("{gsub('~', '', mn)} ~ '{dec_df$decision}'"),
      glue::glue(".default = '{else_cat}'")
    ) |>
      stringr::str_flatten_comma() %>%
      stringr::str_c("dplyr::case_when(", ., ")")
  }) |>
    purrr::set_names(paste0(met_names, "_cat")) |>
    rlang::parse_exprs()
  cond_exprs
}


#' Generate Decision Category Assignments
#'
#' A helper function that applies the dplyr::case_when() statements from the
#' decisions data.frame to the package metrics data.frame, creating new columns
#' with the decision categories for each metric.
#'
#' @param met_dec_df A data.frame of decisions, typically created by
#'   build_decisions_df()
#' @param pkgs_df A data.frame of package metrics, typically created by
#'   available.packages()
#' @param else_cat A character string indicating the default category to assign
#'   when none of the conditions are met.
#'
#' @importFrom dplyr mutate rowwise ungroup
#' @importFrom purrr pwalk
#' @importFrom glue glue
#' @importFrom rlang !!! syms
#'
#' @examples
#' # rip_cats(
#' #   build_decisions_df() |> dplyr::mutate(derived_col = metric),
#' #   available.packages() |> as.data.frame() |> dplyr::select(pkg, downloads_1yr = dplyr::starts_with("downloads_1yr")),
#' #   "High"
#' # )
#'
#' @keywords internal
rip_cats <- function(
    met_dec_df,
    pkgs_df,
    else_cat
) {
  
  met_der <- met_dec_df |>
    dplyr::distinct(metric, derived_col)
  
  purrr::pwalk(list(
    met_der$metric,
    met_der$derived_col 
  ), \(met, der) {
    
    # for debugging
    # met <- met_der$metric[1] 
    # der <- met_der$derived_col[1]
    
    cat(glue::glue("\n\n--> Decisions based off '{met}' metric:\n\n"))
    cond_exprs <- get_case_whens(met_dec_df, der, else_cat)
    
    # pkgs_df$dwnlds_cat <- NULL
    pkgs_df <<- pkgs_df |>
      dplyr::rowwise() |> # Boo! Rowwise is really slow. We need to find a better way eventually.
      dplyr::mutate(!!! cond_exprs) |>
      dplyr::ungroup()
    
    
    # Report of changes for  alone
    # metric_lab <- "" # Could add a label
    Var1 <- pkgs_df[[glue::glue("{der}_cat")]]
    print(
      Var1 |>
        factor(levels = levels(met_dec_df$decision)) |>
        table() |>
        as.data.frame() |>
        dplyr::left_join(
          {round(prop.table(table(Var1)), 3) * 100} |>
            as.data.frame(),
          by = "Var1"
        ) |>
        dplyr::select(Risk = Var1, Cnt = Freq.x, Pct = Freq.y)
    )
  })
  # pkgs$dwnlds_cat <- NULL
  return_pkgs <- pkgs_df |>
    dplyr::mutate(
      # convert cat vars into factors so we can use pmax() on them
      across(ends_with("_cat"), ~ factor(.x, levels = levels(met_dec_df$decision))),
      # higher risk trumps lower risk amongst all _cat vars (e.g., High > Medium > Low)
      final_risk_cat = pmin(!!!rlang::syms(paste0(met_der$derived_col, "_cat")), na.rm = TRUE)
    )
  rm(pkgs_df) # verify we don't accidentally use it later
  
  # Report of changes for primary risk alone
  if(nrow(met_der) > 1) {
    cat(glue::glue("\n\n--> Decisions based off {nrow(met_der)} 'Primary' risk metric(s):\n\n"))
    Var1 <- return_pkgs[["final_risk_cat"]]
    print(
      Var1 |>
        factor(levels = levels(met_dec_df$decision)) |>
        table() |>
        as.data.frame() |>
        dplyr::left_join(
          {round(prop.table(table(Var1)), 3) * 100} |>
            as.data.frame(),
          by = "Var1"
        ) |>
        dplyr::select(Risk = Var1, Cnt = Freq.x, Pct = Freq.y)
    )
  }
  return(return_pkgs)
}












  
