#' Strip Recording
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

