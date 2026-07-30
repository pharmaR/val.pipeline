# Alias a `BioC` entry (if present) to the conventional BiocManager
# names `BioCsoft`, `BioCann`, `BioCexp`, `BioCworkflows`, and
# `BioCbooks` when those are absent. This keeps callers that look up
# `BiocManager::repositories()[["BioCsoft"]]` by name working when the
# repos vector uses a single flat internal BioC snapshot (typical PPM
# layout), while leaving existing `BioC*` entries untouched.
.val_pipeline_alias_bioc <- function(repos) {
  if (is.null(repos) || length(repos) == 0 || is.null(names(repos))) {
    return(repos)
  }
  bioc_names <- c("BioCsoft", "BioCann", "BioCexp",
                  "BioCworkflows", "BioCbooks")
  source <- if ("BioC" %in% names(repos)) {
    unname(repos[["BioC"]])
  } else if ("BioCsoft" %in% names(repos)) {
    unname(repos[["BioCsoft"]])
  } else {
    NULL
  }
  if (is.null(source) || !nzchar(source)) return(repos)
  for (nm in bioc_names) {
    if (!nm %in% names(repos)) repos[[nm]] <- source
  }
  repos
}

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
#' If the caller's repo vector contains a single flat `BioC` entry
#' (typical Posit Package Manager layout, where the whole BioC snapshot
#' lives under one URL rather than being split into `BioCsoft`,
#' `BioCann`, `BioCexp`, `BioCworkflows`, `BioCbooks`), the shim also
#' auto-aliases any missing `BioC*` names to the same URL. This keeps
#' downstream callers that index by name — for example
#' `BiocManager::repositories()[["BioCsoft"]]` inside `riskmetric`'s
#' reverse-dependency assessment — working out of the box.
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
    repos_fn <- function(...) .val_pipeline_alias_bioc(repos)
  } else {
    repos_fn <- function(...) .val_pipeline_alias_bioc(getOption("repos"))
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


#' Route `riskmetric`'s Bioconductor lookups through internal repos
#'
#' Installs two in-session shims on `riskmetric` so its Bioconductor-facing
#' code paths stop reaching out to public `bioconductor.org` URLs and
#' instead consult `options("repos")` — i.e. the same internal PPM CRAN
#' and BioC snapshots the rest of `val.pipeline` uses.
#'
#' # Shim 1: `assess_reverse_dependencies.default`
#'
#' `riskmetric::assess_reverse_dependencies.default()` calls
#' `devtools::revdep(x$name, bioconductor = TRUE)`, which in turn calls
#' `devtools:::bioc_packages()`. That helper unconditionally reads a
#' `VIEWS` file from `BiocManager::repositories()[["BioCsoft"]]`. On a
#' Posit Package Manager mirror the aggregated BioC snapshot is served
#' at `<repo>/src/contrib/PACKAGES` — there is no `<repo>/VIEWS` file at
#' the mirror root — so the read fails with
#'
#' \preformatted{
#' cannot open the connection to '.../bioc-.../latest/VIEWS'
#' }
#'
#' The shim replaces it with a version that computes reverse
#' dependencies from `utils::available.packages()` +
#' `tools::dependsOnPkgs()`. No `VIEWS` file is required.
#'
#' # Shim 2: `memoise_bioc_available`
#'
#' `riskmetric:::memoise_bioc_available()` hard-codes
#' `read.dcf(url("https://bioconductor.org/packages/release/bioc/src/contrib/PACKAGES"))`
#' to build its Bioconductor package index. That URL is contacted
#' *directly*, bypassing `BiocManager::repositories()`, so
#' [configure_bioc_repositories()] alone can't rescue it and
#' `pkg_ref("<BioCPkg>")` fails on an air-gapped host with
#'
#' \preformatted{
#' cannot open the connection to 'https://bioconductor.org/packages/release/bioc/src/contrib/PACKAGES'
#' }
#'
#' The shim replaces it with a version that queries every `BioC*` repo
#' advertised by `BiocManager::repositories()` (which
#' [configure_bioc_repositories()] has already pointed at the internal
#' mirrors) via `utils::available.packages()`. The result is memoised
#' with `memoise::memoise()`, matching the upstream contract, so
#' repeated `pkg_ref()` calls hit the cache. Upstream fix proposed at
#' \href{https://github.com/pharmaR/riskmetric/pull/402}{pharmaR/riskmetric#402}.
#'
#' Both shims are intentionally opt-in; the wrapper
#' [configure_riskmetric_offline_if_requested()] runs this helper only
#' when the run is flagged as air-gapped (env var
#' `VAL_PIPELINE_INTERNAL_BIOC` truthy, or `default: air_gapped: true`
#' in `config.yml`), so public-network users are unaffected.
#'
#' @param quiet Logical. When `FALSE` (the default) a short informational
#'   message is emitted.
#'
#' @return `TRUE` invisibly on success, `FALSE` invisibly when the
#'   `riskmetric` package is not installed.
#'
#' @seealso [configure_riskmetric_offline_if_requested()],
#'   [configure_bioc_repositories()]
#'
#' @export
configure_riskmetric_offline <- function(quiet = FALSE) {
  if (!requireNamespace("riskmetric", quietly = TRUE)) {
    if (!isTRUE(quiet)) {
      message(
        "configure_riskmetric_offline(): riskmetric is not installed; ",
        "nothing to do."
      )
    }
    return(invisible(FALSE))
  }

  revdep_offline <- function(x, ...) {
    ap <- tryCatch(utils::available.packages(),
                   error = function(e) NULL)
    revdeps <- if (is.null(ap) || nrow(ap) == 0L) {
      character(0)
    } else {
      tryCatch(
        tools::dependsOnPkgs(
          x$name,
          dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"),
          recursive    = FALSE,
          installed    = ap
        ),
        error = function(e) character(0)
      )
    }
    structure(
      sort(unique(revdeps)),
      class = c("pkg_metric_reverse_dependencies", "pkg_metric", "character")
    )
  }

  utils::assignInNamespace(
    "assess_reverse_dependencies.default",
    revdep_offline,
    ns = "riskmetric"
  )

  # riskmetric's memoise_bioc_available() hard-codes a read.dcf() against
  #   https://bioconductor.org/packages/release/bioc/src/contrib/PACKAGES
  # (see pharmaR/riskmetric R/utils_memoised.R line ~39). It's called by
  # riskmetric::pkg_bioc() / pkg_ref() to look up a BioC package's
  # version + repo. On an air-gapped host the read fails with
  #   Error in read.dcf(con): cannot open the connection to
  #   'https://bioconductor.org/packages/release/bioc/src/contrib/PACKAGES'
  # even after configure_bioc_repositories() has repointed BiocManager at
  # the internal repos, because this call bypasses BiocManager entirely.
  #
  # Replace it with a version that queries every BioC* repo advertised by
  # BiocManager::repositories() via utils::available.packages(), which
  # reads options("repos"). No public bioconductor.org URL is contacted.
  # Upstream fix proposed at pharmaR/riskmetric#402.
  bioc_available_offline <- function() {
    repos <- tryCatch(BiocManager::repositories(), error = function(e) NULL)
    bioc_repos <- if (!is.null(repos)) {
      nms <- names(repos)
      if (is.null(nms)) character(0) else repos[startsWith(nms, "BioC")]
    } else character(0)
    if (length(bioc_repos) == 0L) bioc_repos <- getOption("repos")

    ap <- tryCatch(
      utils::available.packages(repos = bioc_repos),
      error = function(e) NULL
    )
    if (is.null(ap) || nrow(ap) == 0L) {
      return(data.frame(
        Package = character(0), Version = character(0),
        Repository = character(0), stringsAsFactors = FALSE))
    }
    df <- as.data.frame(ap, stringsAsFactors = FALSE)
    df[!duplicated(df[["Package"]]), , drop = FALSE]
  }
  # memoise the offline function the same way riskmetric memoised the
  # original so repeated pkg_ref() calls hit the cache after the first
  # query and don't re-shell to available.packages().
  memoised_offline <- if (requireNamespace("memoise", quietly = TRUE)) {
    memoise::memoise(bioc_available_offline)
  } else {
    bioc_available_offline
  }
  utils::assignInNamespace(
    "memoise_bioc_available",
    memoised_offline,
    ns = "riskmetric"
  )

  if (!isTRUE(quiet)) {
    message(
      "configure_riskmetric_offline(): riskmetric's reverse-dependency ",
      "lookup and Bioconductor package index will now use ",
      "utils::available.packages() (options(\"repos\")) instead of ",
      "devtools::revdep(bioconductor = TRUE) and a hard-coded ",
      "bioconductor.org/packages/release/bioc URL."
    )
  }

  invisible(TRUE)
}

#' Install the riskmetric offline reverse-deps shim when requested
#'
#' Wrapper around [configure_riskmetric_offline()] that is a no-op
#' unless the environment variable `VAL_PIPELINE_INTERNAL_BIOC` is set
#' to a truthy value (`"1"`, `"TRUE"`, `"true"`, `"yes"`, `"on"`).
#'
#' @param quiet Passed through to [configure_riskmetric_offline()].
#'
#' @return Invisibly, `TRUE` when the shim was installed, `FALSE`
#'   otherwise.
#'
#' @export
configure_riskmetric_offline_if_requested <- function(quiet = FALSE) {
  flag <- Sys.getenv("VAL_PIPELINE_INTERNAL_BIOC", unset = "")
  if (!nzchar(flag) || !tolower(flag) %in% c("1", "true", "yes", "y", "on")) {
    return(invisible(FALSE))
  }
  configure_riskmetric_offline(quiet = quiet)
}
