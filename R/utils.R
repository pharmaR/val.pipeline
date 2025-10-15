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
    # cond <- decisions_df0$condition[1] # for debugging
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

#' Update Repos Option
#'
#' Helper function to update the "CRAN" repo from `options("repos")` to use the
#' user-specified validation date. Two big assumptions with this function at the
#' moment: (1) that the "CRAN" repo is a Posit Package Manager URL, and (2) that
#' the date is at the end of the URL in "YYYY-MM-DD" format.
#'
#' @param val_date A Date object indicating the validation date to use in the
#'   CRAN repo URL.
#' @param opt_repos A named character vector of repositories, typically obtained
#'   from getOption("repos").
#'
#' @importFrom stringr str_detect str_extract str_replace
#'
#' @return A named character vector of repositories with the updated "CRAN"
#'   repo.
update_opt_repos <- function(
    val_date = Sys.Date(),
    opt_repos = getOption("repos")
) {
  
  if("CRAN" %in% toupper(names(opt_repos))) {
    cran_pos <- which("CRAN" == toupper(names(opt_repos)))
    curr_cran <- opt_repos[[cran_pos]]
    # Grab the "base url", which will be the entire text string up till the last "/"
    base_url <- dirname(curr_cran)
    
    # if val_date is today & curr_cran is already "latest" or ends in today's date, then don't update
    if(val_date == Sys.Date()) {
      if(stringr::str_detect(curr_cran, "latest") |
         stringr::str_detect(curr_cran, as.character(Sys.Date()))
      ) {
        cat(paste0("--> 'CRAN' repo is already set to latest snapshot. No update needed.\n"))
        return(opt_repos)
      } else {
        cat(paste0("--> Updating 'CRAN' repo to use latest snapshot.\n"))
        new_cran <- file.path(base_url, "latest")
        opt_repos[[cran_pos]] <- new_cran
        return(opt_repos)
      }
    } else {
      # if val_date is not today, then check"
      # is val_date not being used at all?
      if(!stringr::str_detect(curr_cran, as.character(val_date))) {
        if(stringr::str_detect(curr_cran, "latest") |
           stringr::str_detect(curr_cran, as.character(Sys.Date()))
        ) {
          cat(paste0("--> Updating 'CRAN' repo to use validation date: ", val_date, "\n"))
          new_cran <- gsub("latest", as.character(val_date), 
              gsub("\\d{4}-\\d{2}-\\d{2}", as.character(val_date), curr_cran)
          )
        } else {
          cat(paste0("\n--> 'CRAN' repo is currently set to use date: ", stringr::str_extract(curr_cran, "\\d{4}-\\d{2}-\\d{2}"), "\n"))
          old_date <- stringr::str_extract(curr_cran, "\\d{4}-\\d{2}-\\d{2}")
          if(!is.na(old_date)) {
            new_cran <- gsub(old_date, as.character(val_date), curr_cran)
          } else {
            new_cran <- paste0(base_url, as.character(val_date))
          }
        }
        
        opt_repos[[cran_pos]] <- new_cran

      } # else val_date was found
    }
    return(opt_repos)
  }
}
  

#' Pull Config
#'
#' Pull in relevant rules defined in confg.yml. Also verifies that the
#' conditions only reference decision categories that are allowed in
#' `decision_lst`.
#'
#' @param rule_type A character string indicating whether the decision
#'   categories are used to "categorize" risk levels (e.g., "Low", "Medium",
#'   "High") via val_categorize() or val_decision(). The difference being that
#'   "categorize" is going to be used to filter the initial list of packages
#'   using a remote pkg_ref() assessment, whereas "decision" is used to filter
#'   the final list of packages after a 'pkg_source' pkg_ref() assessment is
#'   produced locally on the GxP system of interest.
#' @param config_path A character string indicating the path to the config.yml
#'
#' @importFrom config get
#' @importFrom purrr map map_lgl set_names
#' @importFrom glue glue
#'
#' @examples pull_config()
#' @examples pull_config("decide_github")
#' 
#' @return A named list of lists
#' 
#' @export
pull_config <- function(
    val = NULL,
    rule_type = c("default", "remote_reduce", "decide")[1],
    config_path = system.file("config.yml", package = "val.pipeline")
) {
  
  configgy <- config::get(
    value = val, # NULL means grab everything
    config = rule_type,
    file = config_path
    # ,use_parent = TRUE # The default
  )
  
  if(!is.null(val)) {
    return(configgy)
  }
  
  # 
  # ---- Pull ----
  #
  fig_names <- names(configgy)
  decisions_pos <- which(fig_names == "decisions_lst")[1] # grab first time 'decisions_lst' shows up
  default_names <- fig_names[1:decisions_pos] 
  rule_names <- fig_names[(decisions_pos + 1): length(fig_names)]
  rule_names <- rule_names[rule_names != "inherits"]
  
  # decision_lst <- configgy[["decisions_lst"]]
  default_lst <- configgy[default_names]
  decision_lst <- default_lst[["decisions_lst"]]
  rule_lst <- configgy[rule_names]
  
  # 
  # ---- Verify ----
  #
  # Verify that conditions (conds) are valid - aka, they only declared
  # decision categories (in `decision_list`). Else, we have to drop that rule
  rule_metric_nm <- names(rule_lst)
  metric_cond_cats <-
    purrr::map(rule_metric_nm, ~ rule_lst[[.x]][["cond"]] |> names()) |>
    purrr::set_names(rule_metric_nm)
  metric_nms <- names(metric_cond_cats)
  valid_cats_in_conds <-
    purrr::map_lgl(metric_nms, ~ {if(all(metric_cond_cats[[.x]] %in% decision_lst)) TRUE else FALSE}) |>
    purrr::set_names(metric_nms) 
  # valid_cats_in_conds <- metric_cond_cats |> unlist() %in% decision_lst
  if (any(!valid_cats_in_conds)) {
    metrics_which_conds_invalid <- valid_cats_in_conds[which(!valid_cats_in_conds)] |> names()
    warning(glue::glue("\nThe decision category(ies) referenced in these metric 'cond' list do not match those allowed in 'decision_lst': {paste(metrics_which_conds_invalid, collapse = ', ')}. Dropping those rules from 'rule_lst'.\n"))
    rule_lst <- rule_lst[valid_cats_in_conds]
  }
  
  # Verify that accept_cats are valid, else drop that rule.
  rule_metric_nm <- names(rule_lst)
  metric_exc_cats <- purrr::map(rule_metric_nm, ~ rule_lst[[.x]][["accept_cats"]]) |> purrr::set_names(rule_metric_nm)
  # valid_cats_in_exc <- metric_exc_cats |> unlist() %in% decision_lst # old
  metric_nms <- names(metric_exc_cats)
  valid_cats_in_exc <-
    purrr::map_lgl(metric_nms, ~ {if(all(metric_exc_cats[[.x]] %in% decision_lst)) TRUE else FALSE}) |>
    purrr::set_names(metric_nms) 
  if (any(!valid_cats_in_exc)) {
    metrics_which_exc_invalid <- valid_cats_in_exc[which(!valid_cats_in_exc)] |> names()
    warning(glue::glue("\nThe decision category(ies) referenced in these metric 'accept_cats' list do not match those allowed in 'decision_lst': {paste(metrics_which_exc_invalid, collapse = ', ')}. Dropping those rules from 'rule_lst'.\n"))
    rule_lst <- rule_lst[valid_cats_in_exc]
  }
  
  return(list(default_lst = default_lst, rule_lst = rule_lst))
}

#' Build Decisions Data.Frame
#'
#' Helper function that creates the minimally necessary data.frame for
#' val_filter() that includes columns "metric", "decision", "condition",
#' "metric_type", and "accept_condition".
#'
#' @param decision_lst A character vector of decision categories, ordered from
#'   highest risk to lowest risk.opt_repos
#' @param rule_type A character string indicating whether the decision
#'   categories are used to "categorize" risk levels (e.g., "Low", "Medium",
#'   "High") via val_categorize() or val_decision(). The difference being that
#'   "categorize" is going to be used to filter the initial list of packages
#'   using a remote pkg_ref() assessment, whereas "decision" is used to filter
#'   the final list of packages after a 'pkg_source' pkg_ref() assessment is
#'   produced locally on the GxP system of interest.
#' @param rule_lst A named list of lists, where each sub-list contains:
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
    rule_type = c("default", "remote_reduce", "decide")[1],
    rule_lst = NULL # could input custom rules list here
  ) {
  
  if (is.null(rule_lst)) {
    cat(glue::glue("\n\n--> Building decision data.frame using rules from '{rule_type}' decision type.\n"))
    
    figgy <- pull_config(rule_type = rule_type)
    decision_lst <- figgy$default_lst$decisions_lst
    rule_lst <- figgy$rule_lst
  }
    
  #
  # Clean up rules list & decision categories 
  #
  rule_metric_nm <- names(rule_lst)
  metric_cond_cats <-
    purrr::map(rule_metric_nm, ~ rule_lst[[.x]][["cond"]] |> names()) |>
    purrr::set_names(rule_metric_nm)
    
  
  # Inform the user of which rules can & will be evaluated
  # message("\n--> Building decision data.frame using the 'rule sets' for the following metrics:\n")
  # cat("\n---->", paste(rule_metric_nm, collapse = '\n----> '), "\n")

  
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
    dplyr::mutate(decision_id = as.integer(decision)) |>
    
    # Create a numeric ID field that uniquely identifies decision within metric
    # useful when we were using more than 1 condition per decision category
    # (eg. "High" would have two conditions)... but that doesn't work well with config::get()
    dplyr::mutate(met_dec_id = dplyr::row_number(), .by = c("metric", "decision")) |>
    
    # add condition from rules list into data.frame column
    dplyr::mutate(
      condition = purrr::pmap_chr(
        list(metric, decision, met_dec_id), \(met, dec, did) {
          # met <- "downloads_1yr"; dec <- "High"; did <- 2 # for debugging
          met_cond <- rule_lst[[met]][["cond"]] 
          met_cond[which(names(met_cond) %in% dec)][[did]] |>
            rlang::expr_text() |>
            rlang::parse_expr() # lose the quotes
            # Or use rlang::expr_text()?
        })
    ) |> 
    # Add in column for metric_type
    dplyr::mutate(
      metric_type = purrr::map_chr(metric, ~ rule_lst[[.x]][["type"]] %||% "Exception")
    ) |>
    # dplyr::mutate(
      # extract lower & Upper limit of the condition
      # Note: to_the_limit() doesn't work well when there are multiple
      # expressions in one condition, so we will remove them for now
      # lower_limit = to_the_limit(condition),              # Not required
      # upper_limit = to_the_limit(condition, low = FALSE), # Not required
      # rationale = NA_character_,
    #   metric_type = ifelse(metric == "downloads_1yr", "Primary", "Exception")
    # ) |>
    
    # Add column for promote_min and auto_accept conditions
    dplyr::left_join(
      purrr::imap_dfr(rule_lst, ~ {
        # .x <- rule_lst[[1]]; .y <- names(rule_lst)[1] # for debugging
        dplyr::tibble(
          metric = .y,
          promote_min = .x[["promote_min"]] %||% NA_character_,
          auto_accept = .x[["auto_accept"]] %||% NA_character_
        )
      }),
      by = "metric"
    ) |>
    dplyr::select(-met_dec_id) # Get rid of this ID
  
  
  
  # Add on the "accept_condition" column
  if (!rlang::is_empty(rule_lst)) {
    decisions_df <- 
      decisions_df0 |>
      # Join in accept_cats from rules list
      dplyr::left_join(
        purrr::imap_dfr(rule_lst, ~ {
          # .x <- rule_lst[[1]]; .y <- names(rule_lst)[1] # for debugging
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
        accept_condition = 
          if(tolower(metric_type) == "primary" &
             decision == decision_lst[1]) TRUE else FALSE
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
#' @param ids Logical. If TRUE, return the decision_id instead of decision category.
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
get_case_whens <- function(met_dec_df, met_names, else_cat, ids = FALSE, auto_accept = FALSE) {
  
  # for debugging
  # met_dec_df <- build_decisions_df() |> dplyr::mutate(derived_col = metric)
  # met_names <- c("dwnlds")
  # else_cat <- "High"
  # ids <- FALSE
  # ids <- TRUE
  # auto_accept <- TRUE
  # auto_accept <- FALSE
  
  # If auto_accept is TRUE, Then set ids to FALSE
  if(auto_accept) {
    ids <- FALSE
  }
  
  # if pkg was accepted outside of val.pipeline, we'll need to add logic to
  # auto_accept those packages
  approved_pkgs <- pull_config(val = "approved_pkgs", rule_type = "default")
  
  cond_exprs <- purrr::map_chr(met_names, ~ {
    # .x <- met_names[1]
    dec_df <- met_dec_df |>
      dplyr::filter(derived_col == .x)
    
    # if else_cat is character & ids is TRUE, we need to convert else_cat to decision_id
    if(ids & is.character(else_cat)) {
      if(else_cat %in% met_dec_df$decision) {
        else_cat <- met_dec_df$decision_id[which(met_dec_df$decision == else_cat)] |> unique()
      } 
    }
    
    # insert a '.x' in place of .x so that the variable name takes is place
    conds <- gsub('.x', .x, dec_df$condition)
    
    # create a 'then' for the if-else-then response based on ids or auto_accept
    then <- if(ids) min(dec_df$decision_id) else {
      if(auto_accept) TRUE else glue::glue("'{levels(dec_df$decision)[1]}'")
    }
    
    # Same here: create the appropriate 'else_cat'
    else_cat <- if(ids) else_cat else {
      if(auto_accept) FALSE else glue::glue("'{else_cat}'")
    }
    
    # if pkg was accepted outside of val.pipeline, we'll need to accept it here:
    if(length(approved_pkgs) > 0) {
      approved_pkgs_cond <- glue::glue("package %in% c('{paste(approved_pkgs, collapse = \"', '\")}') ~ {then}")
    } else {
      approved_pkgs_cond <- NULL
    }
    
    # if there's an auto_accept condition, generate a condition for it
    if(any(!is.na(dec_df$auto_accept))) {
      aa_cond <- gsub('.x', .x, dec_df$auto_accept) |> unique()
      auto_accept_cond <- glue::glue("{gsub('~', '', aa_cond)} ~ {then}")
    } else {
      auto_accept_cond <- NULL
    }
    
    # build case_when statements!
    # via: approved_pkgs, auto_accept, and decision conds, in that order
    c(
      # include a condition(s) for auto_accepted categories, which should be run first
      if(auto_accept) c(approved_pkgs_cond, auto_accept_cond),
      # otherwise, regular metric-based conditions
      if(auto_accept) NULL else glue::glue("{gsub('~', '', conds)} ~ {if(ids) dec_df$decision_id else paste0('\"', dec_df$decision, '\"')}"),
      glue::glue(".default = {else_cat}")
    ) |>
      stringr::str_flatten_comma() %>%
      stringr::str_c("dplyr::case_when(", ., ")")
  }) |>
    purrr::set_names(paste0(met_names, "_cat", if(ids) "id" else if(auto_accept) "aa" else NULL)) |>
    rlang::parse_exprs()
  cond_exprs
}

#' Decision to ID
#' 
#' Helper function to convert decision category to decision_id or vice versa
#' 
#' @param decision_id_df A data.frame with columns "decision" and "decision_id"
#' @param rev Logical. If TRUE, convert decision_id to decision category, else convert decision category to decision_id
#' @param dec A character string (if rev = FALSE) or numeric/integer (if rev = TRUE) indicating the decision category or decision_id to convert
#' 
#' @examples
#' dec_id_df <- unique(build_decisions_df()[c("decision", "decision_id")])
#' decision_to_id(dec_id_df, FALSE, "High")
#' 
#' @keywords internal
#' 
decision_to_id <- function(decision_id_df, rev = FALSE, dec){
  # decision_id_df <- dec_id_df
  # dec <- "High"
  if(rev) {
    return(decision_id_df$decision[which(decision_id_df$decision_id == dec)])
  } else {
    decision_id_df$decision_id[which(decision_id_df$decision == dec)]
  }
}

#' Vectorized Decision to ID
#' 
#' Vectorized version of decision_to_id()
#' 
#' @param decision_id_df A data.frame with columns "decision" and "decision_id"
#' @param rev Logical. If TRUE, convert decision_id to decision category, else convert decision category to decision_id
#' @param dec A character vector (if rev = FALSE) or numeric/integer vector (if rev = TRUE) indicating the decision categories or decision_ids to convert
#' 
#' @examples
#' dec_id_df <- unique(build_decisions_df()[c("decision", "decision_id")])
#' decision_to_id_v(dec_id_df, FALSE, c("High", "Medium", "Low"))
#' 
#' @keywords internal
decision_to_id_v <- Vectorize(decision_to_id, vectorize.args = "dec")


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
    cond_exprs_ids <- get_case_whens(met_dec_df, der, else_cat, ids = TRUE)
    mn <- met_dec_df |> dplyr::filter(!is.na(auto_accept)) |> distinct(derived_col) |> pull(derived_col)
    cond_exprs_aa <- get_case_whens(met_dec_df, mn, else_cat, auto_accept = TRUE)
    
    # else_cat <- "High" # for debugging
    # pkgs_df$dwnlds_cat <- NULL
    pkgs_df <<- pkgs_df |>
      dplyr::rowwise() |> # Boo! Rowwise is really slow. We need to find a better way eventually.
      dplyr::mutate(!!! cond_exprs) |>
      dplyr::mutate(!!! cond_exprs_ids) %>%
      {if(length(cond_exprs_aa) > 0) dplyr::mutate(., !!! cond_exprs_aa) else .} |>
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
  
  dec_id_df <- unique(met_dec_df[c("decision", "decision_id")])
  
  return_pkgs <- pkgs_df |>
    dplyr::mutate(
      # convert cat vars into factors
      across(ends_with("_cat"), ~ factor(.x, levels = levels(met_dec_df$decision))),
      
      # if any column ending in "_cataa" is TRUE, then set final_risk_catid to 1 (Low)
      final_risk_cataa = ifelse(rowSums(across(ends_with("_cataa"), ~ .x), na.rm = TRUE) > 0, 1, NA_integer_),
      
      # higher risk trumps lower risk amongst all _cat vars (e.g., High > Medium > Low)
      max_catid = pmax(!!!rlang::syms(paste0(met_der$derived_col, "_catid")), na.rm = TRUE) |> as.integer(),
      final_risk_catid = dplyr::case_when(
        !is.na(final_risk_cataa) ~ final_risk_cataa,
        is.finite(max_catid) ~ max_catid,
        .default = as.integer(NA) # if this happens, need to investigate!
      ),
      final_risk_cat = decision_to_id_v(dec_id_df, rev = TRUE, final_risk_catid) 
    ) |>
    dplyr::select(-c(ends_with("_catid"), "final_risk_cataa"))
  
  # which(is.na(return_pkgs$final_risk_catid))
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












  
