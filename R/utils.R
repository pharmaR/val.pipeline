#' Strip Recording (List)
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
  no_record <-
    lapply(assessment, \(x) {
      # x <- assessment$covr_coverage # for debugging
      structure(
        x,
        .recording = NULL,
        class = setdiff(class(x),
                        "with_eval_recording")
      )
    })
  class(no_record) <- class(assessment)
  no_record
}

# t<- riskmetric::pkg_ref("zoo", source = "pkg_cran_remote") |>
#   riskmetric::pkg_assess() |>
#   strip_recording()
# names(t)
# t$remote_checks |> attributes() # see .recording
# f <- t |>
#   strip_recording()
# f$remote_checks
# f$remote_checks |> attributes() # It's gone


#' Strip Recording (Data Frame)
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

