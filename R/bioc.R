#' Configure BiocManager to use only user-supplied repositories
#'
#' Some validation environments (typically air-gapped Posit Package Manager
#' installations) cannot reach `bioconductor.org`. In that situation
#' `BiocManager::repositories()` still emits its five hard-coded public
#' BioC URLs (`BioCsoft`, `BioCann`, `BioCexp`, `BioCworkflows`,
#' `BioCbooks`) in addition to whatever the user has set on
#' `options("repos")`, and downstream calls in `riskmetric` — for
#' example `assess_reverse_dependencies()` — fail with
#'
#' \preformatted{
#' Bioconductor version cannot be validated; no internet connection? ...
#' cannot open the connection to 'https://bioconductor.org/packages/.../VIEWS'
#' }
#'
#' This helper puts an in-session shim in place so that
#' `BiocManager::repositories()` returns *only* the caller's
#' `options("repos")` value. It is intentionally opt-in and it does not
#' mutate any package on disk.
#'
#' Two ways to opt in:
#' \itemize{
#'   \item Call `configure_bioc_repositories()` (or the wrapper
#'     [configure_bioc_repositories_if_requested()]) yourself, before
#'     running any `val.pipeline` entry point.
#'   \item Set the environment variable `VAL_PIPELINE_INTERNAL_BIOC=1`
#'     (or `TRUE`, or `true`). The `val.pipeline` entry points call
#'     `configure_bioc_repositories_if_requested()` at startup, so the
#'     shim is applied for the run without any code change.
#' }
#'
#' The shim also sets
#' `options(BiocManager.check_repositories = FALSE)` so that BiocManager
#' does not phone home to `bioconductor.org` to validate the release
#' version. If the caller has not set the `R_BIOC_VERSION` environment
#' variable, this helper leaves it untouched — that is the caller's
#' responsibility (typical value: `"3.22"`).
#'
#' @param repos Optional named character vector of repository URLs. When
#'   supplied it is used as the shim's return value; when `NULL` (the
#'   default) the shim calls `getOption("repos")` each time it is
#'   invoked, so any later `options(repos = ...)` change is picked up
#'   automatically.
#' @param quiet Logical. When `FALSE` (the default) a short informational
#'   message is emitted describing the effective repositories.
#'
#' @return Invisibly, the character vector of repository URLs that
#'   `BiocManager::repositories()` will now return.
#'
#' @seealso [configure_bioc_repositories_if_requested()]
#'
#' @examples
#' \dontrun{
#' # Point R at the internal PPM CRAN + BioC snapshots first
#' options(repos = c(
#'   CRAN = "https://ppm.example.com/cran/latest",
#'   BioC = "https://ppm.example.com/bioc/latest"
#' ))
#' Sys.setenv(R_BIOC_VERSION = "3.22")
#'
#' # Then install the shim
#' configure_bioc_repositories()
#'
#' # BiocManager now returns only the internal repos
#' BiocManager::repositories()
#' }
#'
#' @export
configure_bioc_repositories <- function(repos = NULL, quiet = FALSE) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    if (!isTRUE(quiet)) {
      message(
        "configure_bioc_repositories(): BiocManager is not installed; ",
        "nothing to do."
      )
    }
    return(invisible(character(0)))
  }

  if (!is.null(repos)) {
    stopifnot(is.character(repos))
    if (length(repos) > 0 && is.null(names(repos))) {
      stop("`repos` must be a *named* character vector.", call. = FALSE)
    }
    repos_fn <- function(...) repos
  } else {
    repos_fn <- function(...) getOption("repos")
  }

  # Skip the online version-validity check so BiocManager does not try
  # to fetch bioconductor.org/config.yaml.
  options(BiocManager.check_repositories = FALSE)

  # Replace BiocManager::repositories() with a shim that returns only
  # the user's own repos. `assignInNamespace()` is the sanctioned tool
  # for this kind of session-scoped override.
  utils::assignInNamespace("repositories", repos_fn, ns = "BiocManager")

  effective <- repos_fn()
  if (!isTRUE(quiet)) {
    if (length(effective) == 0) {
      message(
        "configure_bioc_repositories(): shim installed, but no repositories ",
        "are configured. Set `options(repos = c(CRAN = ..., BioC = ...))` ",
        "before installing the shim, or pass `repos = ...` explicitly."
      )
    } else {
      message(
        "configure_bioc_repositories(): BiocManager::repositories() will ",
        "now return:\n",
        paste0("  ", format(names(effective)), " -> ", unname(effective),
               collapse = "\n")
      )
    }
  }

  invisible(effective)
}

#' Install the BiocManager repositories shim when requested by an env var
#'
#' Wrapper around [configure_bioc_repositories()] that is a no-op unless
#' the environment variable `VAL_PIPELINE_INTERNAL_BIOC` is set to a
#' truthy value (`"1"`, `"TRUE"`, `"true"`, `"yes"`). This is what the
#' `val.pipeline` entry points call at startup so that public-network
#' users are unaffected while air-gapped users can opt in with a single
#' env var.
#'
#' @param quiet Passed through to [configure_bioc_repositories()].
#'
#' @return Invisibly, the character vector of repository URLs that the
#'   shim will return (or `character(0)` when the env var is unset or
#'   BiocManager is not installed).
#'
#' @export
configure_bioc_repositories_if_requested <- function(quiet = FALSE) {
  flag <- Sys.getenv("VAL_PIPELINE_INTERNAL_BIOC", unset = "")
  if (!nzchar(flag) || !tolower(flag) %in% c("1", "true", "yes", "y", "on")) {
    return(invisible(character(0)))
  }
  configure_bioc_repositories(quiet = quiet)
}
