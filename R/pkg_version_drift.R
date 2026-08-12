#' Audit Per-Package Meta-Bundle Version Drift
#'
#' Compare the `<pkg>_<ver>_meta.rds` files on disk under
#' `val_dir/assessed/` against the packages + versions that `val_build()`
#' *would* dispatch on the next run, and classify each mismatch. Useful
#' when `val_build()`'s cached-skip count (based on pkg + version) is
#' smaller than `length(list.files(assessed, pattern = "_meta.rds$"))`
#' (based on pkg-name alone) and you want to see exactly which packages
#' account for the gap.
#'
#' @details
#' The "expected version" is resolved in this order:
#'
#' 1. `prep$vers` when a `val_prep` object is supplied (single source of
#'    truth used by `val_build()`).
#' 2. `avail_pkgs$Version` (indexed by `avail_pkgs$Package`) when
#'    `prep` is `NULL` but `avail_pkgs` is supplied.
#' 3. A fresh `available.packages()` call using the caller's current
#'    `options("repos")` when neither is supplied. Cheap safety net;
#'    prefer 1 or 2 when auditing a run whose snapshot repos differ
#'    from the current session's.
#'
#' Each package with an on-disk bundle or an expected version is
#' classified into one of:
#'
#' - `"match"`: on-disk version equals the expected version.
#' - `"drifted"`: at least one on-disk bundle exists at a version
#'   different from the expected one. The old bundle is stale; the pkg
#'   will be re-dispatched by `val_build()` on the next run.
#' - `"multi_version"`: two or more on-disk versions for the same
#'   package (the run wrote multiple bundles across resumes). At least
#'   one is stale.
#' - `"unexpected"`: the pkg has an on-disk bundle but no expected
#'   version (e.g. the pkg is no longer in the current dispatch set).
#' - `"missing"`: the pkg has an expected version but no on-disk
#'   bundle (never assessed).
#'
#' @param val_dir Character(1). Path to the run directory (the parent
#'   of `assessed/`). Same shape as the output of `val_build()`.
#' @param prep Optional `val_prep` list from `val_prep_pipeline()`.
#'   Preferred source of expected versions.
#' @param avail_pkgs Optional data.frame with `Package` and `Version`
#'   columns (same shape as `available.packages()` output). Used when
#'   `prep` is not supplied.
#' @param verbose Logical(1). Print a status count summary. Default
#'   `TRUE`.
#'
#' @return A tibble with one row per package (union of on-disk +
#'   expected sets), columns:
#'   - `pkg`: package name
#'   - `expected_ver`: character or `NA` when the pkg isn't in the
#'     dispatch set
#'   - `on_disk_vers`: comma-separated on-disk versions, or `NA` when
#'     no bundle exists
#'   - `n_on_disk`: integer count of on-disk bundles
#'   - `status`: one of `"match"`, `"drifted"`, `"multi_version"`,
#'     `"unexpected"`, `"missing"`
#'
#' @examples
#' \dontrun{
#' # After a run finished (or partially finished):
#' prep <- val_prep_pipeline(...)
#' drift <- assess_pkg_version_drift(val_dir = "riskassessment/R_4.5.2/20260812",
#'                                   prep = prep)
#' # Show only mismatches:
#' subset(drift, status != "match")
#' }
#'
#' @export
assess_pkg_version_drift <- function(val_dir,
                                     prep = NULL,
                                     avail_pkgs = NULL,
                                     verbose = TRUE) {
  stopifnot(is.character(val_dir), length(val_dir) == 1L, nzchar(val_dir))
  assessed <- file.path(val_dir, "assessed")
  if (!dir.exists(assessed)) {
    stop("val_dir has no 'assessed/' subdirectory: ", val_dir, call. = FALSE)
  }

  meta_files <- list.files(assessed, pattern = "_meta\\.rds$")

  # Split "<pkg>_<ver>_meta.rds" on the LAST underscore. CRAN pkg names
  # can't contain `_`; versions can contain `.` and `-` but not `_`, so
  # the last `_` before `_meta.rds` cleanly separates the two.
  parsed <- if (length(meta_files) == 0L) {
    data.frame(pkg = character(), ver = character(),
               stringsAsFactors = FALSE)
  } else {
    stems <- sub("_meta\\.rds$", "", meta_files)
    m <- regmatches(stems, regexec("^(.+)_([^_]+)$", stems))
    ok <- vapply(m, function(x) length(x) == 3L, logical(1))
    if (any(!ok)) {
      warning(sum(!ok),
              " meta filename(s) did not match `<pkg>_<ver>_meta.rds`; ",
              "skipping: ", paste(meta_files[!ok], collapse = ", "),
              call. = FALSE)
    }
    data.frame(
      pkg = vapply(m[ok], `[[`, character(1), 2L),
      ver = vapply(m[ok], `[[`, character(1), 3L),
      stringsAsFactors = FALSE
    )
  }

  # Resolve expected versions.
  expected <- if (!is.null(prep)) {
    if (is.null(prep$pkgs) || is.null(prep$vers)) {
      stop("`prep` must contain `pkgs` and `vers` fields.", call. = FALSE)
    }
    data.frame(pkg = prep$pkgs, expected_ver = prep$vers,
               stringsAsFactors = FALSE)
  } else if (!is.null(avail_pkgs)) {
    if (!all(c("Package", "Version") %in% colnames(avail_pkgs))) {
      stop("`avail_pkgs` must have `Package` and `Version` columns.",
           call. = FALSE)
    }
    data.frame(pkg = as.character(avail_pkgs[, "Package"]),
               expected_ver = as.character(avail_pkgs[, "Version"]),
               stringsAsFactors = FALSE)
  } else {
    ap <- utils::available.packages()
    data.frame(pkg = as.character(ap[, "Package"]),
               expected_ver = as.character(ap[, "Version"]),
               stringsAsFactors = FALSE)
  }
  expected <- expected[!duplicated(expected$pkg), , drop = FALSE]

  # Per-pkg on-disk collapse.
  on_disk <- if (nrow(parsed) == 0L) {
    data.frame(pkg = character(),
               on_disk_vers = character(),
               n_on_disk = integer(),
               stringsAsFactors = FALSE)
  } else {
    ag <- split(parsed$ver, parsed$pkg)
    data.frame(
      pkg = names(ag),
      on_disk_vers = vapply(ag,
                            function(v) paste(sort(unique(v)),
                                              collapse = ", "),
                            character(1)),
      n_on_disk = vapply(ag,
                         function(v) length(unique(v)),
                         integer(1)),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  }

  out <- merge(expected, on_disk, by = "pkg", all = TRUE, sort = TRUE)
  out$n_on_disk[is.na(out$n_on_disk)] <- 0L

  # Classify.
  out$status <- with(out, ifelse(
    is.na(on_disk_vers),
    "missing",
    ifelse(
      is.na(expected_ver),
      "unexpected",
      ifelse(
        n_on_disk > 1L,
        "multi_version",
        ifelse(on_disk_vers == expected_ver, "match", "drifted")
      )
    )
  ))

  out <- out[, c("pkg", "expected_ver", "on_disk_vers",
                 "n_on_disk", "status"), drop = FALSE]
  rownames(out) <- NULL

  if (isTRUE(verbose)) {
    tbl <- table(out$status)
    val_msg(paste0("\n--> Package version drift audit: ",
                   nrow(out), " pkg(s) total\n"),
            min_level = "minimal")
    for (k in names(tbl)) {
      val_msg(paste0("     ", formatC(k, width = 14, flag = "-"),
                     tbl[[k]], "\n"),
              min_level = "minimal")
    }
  }

  tibble::as_tibble(out)
}
