#' Strip Recording
#'
#' Remove .recording attribute from all elements of the assessment,
#'   whilst maintaining classes
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