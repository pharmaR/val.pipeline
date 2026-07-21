
#' Resolve a Package Set into a Dependency-Ordered Build List
#'
#' Given a seed set of packages and a dependency policy, expands the
#' set into its full (optionally recursive) dependency tree via
#' [tools::package_dependencies()], then sorts the resulting universe
#' by descending dependency frequency so foundational packages get
#' assessed first. Returns the aligned name / version / avail_pkgs
#' triple that both [val_prep_pipeline()] and [val_build()] need to
#' drive their per-package loops.
#'
#' This is the shared workhorse behind the "Which pkgs, ordered" block
#' that used to be duplicated in the top of `val_build()` and the
#' bottom of `val_prep_pipeline()`.
#'
#' @param pkg_names Character vector of seed package names. When
#'   `NULL` (only meaningful for `val_build()`'s legacy
#'   assess-everything-on-CRAN path), the returned `pkgs` is every
#'   package in `avail_pkgs` and no dep expansion is performed.
#' @param deps One of `"depends"`, `"suggests"`, `c("depends","suggests")`,
#'   or `NULL`. Passed through to the `which` argument of
#'   [tools::package_dependencies()]. `NULL` means "don't expand — use
#'   `pkg_names` as-is".
#' @param deps_recursive Logical. Whether dependency expansion is
#'   recursive. Passed straight through.
#' @param avail_pkgs Optional data frame from
#'   `as.data.frame(available.packages())`. When `NULL`, one is
#'   fetched via `utils::available.packages()`. Callers that already
#'   hold one can pass it in to avoid a round-trip.
#'
#' @return A list with:
#' \describe{
#'   \item{pkgs}{Character vector of package names in build order
#'     (descending dependency frequency, ties broken alphabetically).}
#'   \item{vers}{Character vector of versions, aligned to `pkgs`.}
#'   \item{avail_pkgs}{The dep-frequency-sorted `available.packages()`
#'     data frame. Retains a `dep_freq` column when dep expansion
#'     actually happened.}
#' }
#'
#' @importFrom dplyr arrange case_when desc filter mutate pull
#' @importFrom tools package_dependencies
#' @importFrom utils available.packages
#' @keywords internal
#' @export
resolve_pkg_tree <- function(
  pkg_names,
  deps = c("depends", "suggests")[1],
  deps_recursive = TRUE,
  avail_pkgs = NULL
) {
  if (is.null(avail_pkgs)) {
    avail_pkgs <- utils::available.packages() |> as.data.frame()
  }

  if (is.null(pkg_names)) {
    # Legacy val_build() behaviour: no seed set = assess everything
    # currently available from the configured repos.
    pkgs <- avail_pkgs$Package
  } else if (is.null(deps)) {
    # No expansion requested; treat pkg_names as the terminal set.
    full_dep_tree <- pkg_names
    pkgs <- avail_pkgs |>
      dplyr::filter(Package %in% full_dep_tree) |>
      dplyr::pull(Package)
  } else {
    deps_low <- tolower(deps)
    which_deps <- dplyr::case_when(
      all(c("depends", "suggests") %in% deps_low) ~ "most",
      deps_low == "depends"  ~ "strong",
      deps_low == "suggests" ~ "Suggests",
      .default = NULL
    )[1]
    if (!which_deps %in% c("most", "strong", "Suggests"))
      stop("problem with 'which_deps'")

    dep_tree <- tools::package_dependencies(
      packages  = pkg_names,
      which     = which_deps,
      recursive = deps_recursive
    )

    # Sort avail_pkgs so pkgs relied on by many others get assessed
    # first — if a foundational pkg fails, its reverse deps can be
    # skipped rather than wasting time on them.
    pkg_freqs <- dep_tree |> unlist(use.names = FALSE) |> table()
    avail_pkgs <- avail_pkgs |>
      dplyr::mutate(dep_freq = pkg_freqs[Package]) |>
      dplyr::mutate(dep_freq = ifelse(is.na(dep_freq), 0, dep_freq)) |>
      dplyr::arrange(dplyr::desc(dep_freq), Package)

    full_dep_tree <- dep_tree |>
      unlist(use.names = FALSE) |>
      c(names(dep_tree)) |>
      unique() |>
      sort()

    pkgs <- avail_pkgs |>
      dplyr::filter(Package %in% full_dep_tree) |>
      dplyr::pull(Package)
  }

  vers <- avail_pkgs |>
    dplyr::filter(Package %in% pkgs) |>
    dplyr::pull(Version)

  list(
    pkgs       = pkgs,
    vers       = vers,
    avail_pkgs = avail_pkgs
  )
}
