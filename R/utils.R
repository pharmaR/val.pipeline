#' Strip Recording (for list() objects)
#'
#' Remove .recording attribute from all elements of the assessment,
#'   whilst maintaining classes
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




#' Strip Recording (Data Frame) - Not Used
#'
#' Remove .recording attribute from all elements of the assessment,
#'   whilst maintaining classes
#' 
#' @examples
#' # riskmetric::pkg_ref("zoo", source = "pkg_cran_remote") |>
#' #   riskmetric::pkg_assess() |>
#' #   strip_recording()
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


#' Build Exceptions Data.Frame
#'
#' Remove .recording attribute from all elements of the assessment,
#'   whilst maintaining classes
#' 
#' @importFrom rlang is_empty
#' 
#' @keywords internal
build_decisions_df <- function(
    
    decision_lst = c("Low", "Medium", "High"),
    
    rules_lst = list(
      downloads_1yr = list(
        cond = list(
          High = ~ is.na(.x),
          High = ~ .x < 40000,
          Medium = ~ dplyr::between(.x, 40000, 80000),
          Low = ~ .x > 80000
        ),
        type = "primary",
        min_value = 10000
      ),
      reverse_dependencies = list(
        cond = list(
          High = ~ is.na(.x),
          High = ~ .x < 2,
          Medium = ~ dplyr::between(.x, 2, 7),
          Low = ~ .x > 7
        ),
        type = "exception",
        exception_cats = c("Low", "Medium")
        ),
      dependencies =  list(
        cond = list(
          High = ~ is.na(.x),
          High = ~ .x > 8,
          Medium = ~ dplyr::between(.x, 4, 8),
          Low = ~ .x < 4
        ),
        type = "exception",
        exception_cats = c("Low", "Medium")
      ),
      news_current =  list(
        cond = list(
          High = ~ is.na(.x),
          High = ~ .x != 1,
          Low = ~ .x == 1
        ),
        type = "exception",
        exception_cats = c("Low")
      ),
      # bugs_status = list(
      #   cond = list(
      #       High = ~ is.na(.x) | .x != 1,
      #       Low = ~ .x == 1
      #     ),
      #   type = "exception",
      #   exception_cats = c("Low")
      #   ),
      has_vignettes =  list(
        cond = list(
          High = ~ is.na(.x),
          High = ~ .x < 1,
          Medium = ~ .x == 1,
          Low = ~ .x > 1
        ),
        type = "exception",
        exception_cats = c("Low")
      ),
      has_source_control =  list(
        cond = list(
          High = ~ is.na(.x),
          High = ~ .x == 0,
          Low = ~ .x > 0
        ),
        type = "exception",
        exception_cats = c("Low")
      ),
      has_website =  list(
        cond = list(
          High = ~ is.na(.x),
          High = ~ .x == 0,
          Low = ~ .x > 0
        ),
        type = "exception",
        exception_cats = c("Low")
        )
      )
  ) {
  
  #
  # Clean up rules list & decision categories 
  #
  
  # old:
  # valid_cats_in_rules <- unlist(purrr::map(rules_lst, names)) %in% decision_lst # old
  
  # new:
  # Verify that conditions (conds) are valid - aka, they only declared
  # decision categories (in `decision_list`). Else, we have to drop that rule
  rule_metric_nm <- names(rules_lst)
  metric_cond_cats <-
    purrr::map(rule_metric_nm, ~ rules_lst[[.x]][["cond"]] |> names()) |>
    purrr::set_names(rule_metric_nm)
  metric_nms <- names(metric_cond_cats)
  valid_cats_in_conds <-
    purrr::map_lgl(metric_nms, ~ if(all(metric_cond_cats[[.x]] %in% decision_lst)) TRUE else FALSE) |>
    purrr::set_names(metric_nms) 
  # valid_cats_in_conds <- metric_cond_cats |> unlist() %in% decision_lst
  if (any(!valid_cats_in_conds)) {
    metrics_which_conds_invalid <- valid_cats_in_conds[which(!valid_cats_in_conds)] |> names()
    warning(glue::glue("\nThe decision category(ies) referenced in these metric 'cond' list do not match those allowed in 'decision_lst': {paste(metrics_which_conds_invalid, collapse = ', ')}. Dropping those rules from 'rule_lst'.\n"))
    rules_lst <- rules_lst[valid_cats_in_conds]
  }
  
  # Verify that exception_cats are valid, else drop that rule.
  rule_metric_nm <- names(rules_lst)
  metric_exc_cats <- purrr::map(rule_metric_nm, ~ rules_lst[[.x]][["exception_cats"]]) |> purrr::set_names(rule_metric_nm)
  # valid_cats_in_exc <- metric_exc_cats |> unlist() %in% decision_lst # old
  metric_nms <- names(metric_exc_cats)
  valid_cats_in_exc <-
    purrr::map_lgl(metric_nms, ~ if(all(metric_exc_cats[[.x]] %in% decision_lst)) TRUE else FALSE) |>
    purrr::set_names(metric_nms) 
  if (any(!valid_cats_in_exc)) {
    metrics_which_exc_invalid <- valid_cats_in_exc[which(!valid_cats_in_exc)] |> names()
    warning(glue::glue("\nThe decision category(ies) referenced in these metric 'exception_cats' list do not match those allowed in 'decision_lst': {paste(metrics_which_exc_invalid, collapse = ', ')}. Dropping those rules from 'rule_lst'.\n"))
    rules_lst <- rules_lst[valid_cats_in_exc]
  }
  
  # Inform the user of which rules can & will be evaluated
  message("\n--> Building exceptions data.frame using the 'rule sets' for the following metrics:\n")
  cat("\n---->", paste(names(rules_lst), collapse = '\n----> '), "\n")

  # old
  # expand.grid(
  #   decision = decision_cats,
  #   metric = names(rules_lst),
  #   stringsAsFactors = FALSE
  # ) |>
  # dplyr::select(metric, decision) |>
  
  to_the_limit <- function(condition, low = TRUE) {
    purrr::map_dbl(condition, \(cond){
      # cond <- exceptions_df$condition[2] # for debugging
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
  # to_the_limit(
  #   condition = exceptions_df$condition[2],
  #   low = FALSE
  # )
  
  # Build exceptions data.frame
  exceptions_df <-
    
    # Use metric_cond_cats to build an initial a data frame with metric &
    # applicable decision categories.
    purrr::imap_dfr(metric_cond_cats, ~ {
      # .x <- metric_cond_cats[[1]]; .y <- names(metric_cond_cats)[1] # for debugging
      dplyr::tibble(
        metric = rep(.y, length(.x)),
        decision = .x
      )
    }) |>
    
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
    # exceptions_df$cond |> class()
    dplyr::mutate(
      # extract lower & Upper limit of the condition
      lower_limit = to_the_limit(condition),
      upper_limit = to_the_limit(condition, low = FALSE),
      # rationale = NA_character_,
      metric_type = ifelse(metric == "downloads_1yr", "Primary", "Exception")
    ) |>
    dplyr::select(-decision_id)
  
  
  
  # Add on the decision_applied column
  if (!rlang::is_empty(rules_lst)) {
    exceptions_df <- 
      exceptions_df |>
      # Join in exception_cats from rules list
      dplyr::left_join(
        purrr::imap_dfr(rules_lst, ~ {
          # .x <- rules_lst[[1]]; .y <- names(rules_lst)[1] # for debugging
          dplyr::tibble(
            metric = .y,
            exception_cats = list(.x[["exception_cats"]])
          )
        }),
        by = "metric"
      ) |>
      # Flag whether decision category is an exception category
      dplyr::mutate(
        condition_applied = purrr::pmap_lgl(
          list(decision, exception_cats), \(dec, exc_cats) {
            # dec <- exceptions_df$decision[1]; exc_cats <- exceptions_df$exception_cats[[1]] # for debugging
            if (is.null(exc_cats)) {
              return(FALSE)
            } else {
              return(dec %in% exc_cats)
            }
          }
        ) | tolower(metric_type) == "primary"
      ) |>
      dplyr::select(-exception_cats)
  } else {
    exceptions_df <- 
      exceptions_df |>
      dplyr::mutate(
        condition_applied = if(tolower(metric_type) == "primary") TRUE else FALSE
      )
  }
  return(exceptions_df)
}

# Move to utils.R
get_case_whens <- function(met_names, else_cat) {
  cond_exprs <- purrr::map_chr(met_names, ~ {
    # .x <- met_names[1]
    dec_df <- decisions_df |>
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


# to utils.R
rip_cats <- function(
    decision_df,
    pkgs_df,
    else_cat
) {
  
  met_der <- decision_df |>
    dplyr::distinct(metric, derived_col)
  
  purrr::pwalk(list(
    met_der$metric,
    met_der$derived_col 
  ), \(met, der) {
    
    # for debugging
    # met <- met_der$metric[1] 
    # der <- met_der$derived_col[1]
    
    cat(glue::glue("\n\n--> Decisions based off '{met}' metric:\n\n"))
    cond_exprs <- get_case_whens(der, else_cat)
    
    # pkgs_df$dwnlds_cat <- NULL
    pkgs_df <<- pkgs_df |>
      dplyr::rowwise() |>
      dplyr::mutate(!!! cond_exprs) |>
      dplyr::ungroup()
    
    
    # Report of changes for  alone
    metric_lab <- "" # Could add.
    Var1 <- pkgs_df[[glue::glue("{der}_cat")]]
    print(
      Var1 |>
        factor(levels = levels(decision_df$decision)) |>
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
  pkgs <<- pkgs_df
}












  
