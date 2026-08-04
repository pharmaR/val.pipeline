#' Summarize `timings.csv` from a `val_build()` Run
#'
#' Reads the per-package / per-phase timings that [val_build()] writes
#' to `timings.csv` (long format: `pkg`, `ver`, `phase`, `seconds`;
#' introduced in #87) and returns three ready-to-inspect tibbles:
#' `per_pkg` (total wall time per package), `per_phase` (aggregate
#' stats per phase across all packages), and `wide` (one row per
#' package, one column per phase, missing phases as `NA`).
#'
#' This is a **standalone analysis helper** — it does not depend on
#' any state produced by [val_pipeline()]. Point it at a
#' `timings.csv`, a run directory that contains one, or a data frame
#' already read into R, and it just works. Suitable for both
#' interactive profiling of a completed run and cross-run comparison
#' (call it once per run, then diff the returned tibbles).
#'
#' When called interactively without an explicit `top_n = 0`, the
#' helper also prints a short top-N summary (top-N pkgs by total
#' seconds + phase-level aggregates) to make it useful as a one-liner
#' from the console.
#'
#' @param x One of:
#'   \itemize{
#'     \item Character(1) path to a `timings.csv` file.
#'     \item Character(1) path to a run directory (`R_<ver>/<YYYYMMDD>/`)
#'       that contains a `timings.csv` — the file is located
#'       automatically.
#'     \item A data.frame / tibble already read into R, as long as it
#'       has columns `pkg`, `phase`, and `seconds`. A `ver` column is
#'       optional and passed through when present.
#'   }
#' @param top_n Integer. When printing the interactive summary, how
#'   many packages to show in the top-total table. `0` disables the
#'   print step entirely (useful when embedding the call inside another
#'   pipeline). Default `10`.
#' @param quiet Logical. When `TRUE`, suppresses the interactive
#'   print step regardless of `top_n`. Default `!interactive()`.
#'
#' @return Invisibly, a list of three tibbles:
#'   \describe{
#'     \item{`per_pkg`}{One row per package: `pkg`, `ver` (if present),
#'       `total_s`, `n_phases`. Sorted descending by `total_s`.}
#'     \item{`per_phase`}{One row per phase across all packages: `phase`,
#'       `total_s`, `mean_s`, `median_s`, `p95_s`, `n_pkgs`. Sorted
#'       descending by `total_s`.}
#'     \item{`wide`}{One row per package, one column per phase.
#'       Missing (pkg, phase) combinations come through as `NA`.}
#'   }
#'
#' @examples
#' \dontrun{
#' # From a completed run directory:
#' val_timings_summary("/data/shared/riskassessments/R_4.5.2/20260621")
#'
#' # Direct path to the CSV works too:
#' res <- val_timings_summary(
#'   "/data/shared/riskassessments/R_4.5.2/20260621/timings.csv",
#'   top_n = 20
#' )
#' res$per_phase
#'
#' # Cross-run diff:
#' a <- val_timings_summary("R_4.5.2/20260621", quiet = TRUE)
#' b <- val_timings_summary("R_4.5.2/20260721", quiet = TRUE)
#' dplyr::full_join(a$per_phase, b$per_phase,
#'                  by = "phase", suffix = c("_a", "_b"))
#' }
#'
#' @importFrom dplyr arrange as_tibble desc group_by mutate n rename
#'   summarise ungroup
#' @importFrom stats median quantile
#' @importFrom tidyr pivot_wider
#' @importFrom utils head read.csv
#'
#' @export
val_timings_summary <- function(x,
                                top_n = 10L,
                                quiet = !interactive()) {
  stopifnot(
    length(top_n) == 1L,
    is.numeric(top_n) || is.integer(top_n),
    !is.na(top_n), top_n >= 0L,
    is.logical(quiet), length(quiet) == 1L, !is.na(quiet)
  )
  top_n <- as.integer(top_n)

  tim <- resolve_timings_input(x)

  required <- c("pkg", "phase", "seconds")
  missing_cols <- setdiff(required, names(tim))
  if (length(missing_cols) > 0L) {
    stop("timings input is missing required column(s): ",
         paste(missing_cols, collapse = ", "),
         ". Expected columns: pkg, ver (optional), phase, seconds.",
         call. = FALSE)
  }

  # Coerce seconds -> numeric; drop rows we can't score.
  tim$seconds <- suppressWarnings(as.numeric(tim$seconds))
  n_bad <- sum(is.na(tim$seconds))
  if (n_bad > 0L) {
    warning("Dropping ", n_bad,
            " row(s) with non-numeric / NA 'seconds' from timings.",
            call. = FALSE)
    tim <- tim[!is.na(tim$seconds), , drop = FALSE]
  }
  if (nrow(tim) == 0L) {
    stop("No usable rows in timings input after coercion.", call. = FALSE)
  }

  has_ver <- "ver" %in% names(tim)

  # Per-package totals -----------------------------------------------------
  per_pkg <- if (has_ver) {
    dplyr::as_tibble(tim) |>
      dplyr::group_by(pkg, ver) |>
      dplyr::summarise(total_s = sum(seconds),
                       n_phases = dplyr::n(),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total_s))
  } else {
    dplyr::as_tibble(tim) |>
      dplyr::group_by(pkg) |>
      dplyr::summarise(total_s = sum(seconds),
                       n_phases = dplyr::n(),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total_s))
  }

  # Per-phase aggregates ---------------------------------------------------
  per_phase <- dplyr::as_tibble(tim) |>
    dplyr::group_by(phase) |>
    dplyr::summarise(
      total_s  = sum(seconds),
      mean_s   = mean(seconds),
      median_s = stats::median(seconds),
      p95_s    = as.numeric(stats::quantile(seconds, 0.95, na.rm = TRUE)),
      n_pkgs   = length(unique(pkg)),
      .groups  = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_s))

  # Wide table -------------------------------------------------------------
  id_cols <- if (has_ver) c("pkg", "ver") else "pkg"
  wide <- dplyr::as_tibble(tim) |>
    tidyr::pivot_wider(
      id_cols     = dplyr::all_of(id_cols),
      names_from  = "phase",
      values_from = "seconds",
      values_fn   = sum
    )

  out <- list(per_pkg = per_pkg, per_phase = per_phase, wide = wide)

  if (!quiet && top_n > 0L) {
    print_timings_summary(out, top_n = top_n)
  }

  invisible(out)
}


# Locate the actual data frame. Accepts:
#   * data.frame -> used verbatim
#   * character(1) path to timings.csv -> read_csv
#   * character(1) path to a directory containing timings.csv -> read_csv
resolve_timings_input <- function(x) {
  if (is.data.frame(x)) return(x)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`x` must be a data.frame or a length-1 character path.",
         call. = FALSE)
  }
  if (!file.exists(x)) {
    stop("Path does not exist: ", x, call. = FALSE)
  }
  csv_path <- if (dir.exists(x)) {
    cand <- file.path(x, "timings.csv")
    if (!file.exists(cand)) {
      stop("Directory has no 'timings.csv': ", x,
           ". Point `x` at a run directory produced by val_build() ",
           "(has timings.csv alongside qual_metadata.rds), or at the ",
           "CSV directly.", call. = FALSE)
    }
    cand
  } else {
    x
  }
  utils::read.csv(csv_path, stringsAsFactors = FALSE)
}


# Compact console summary. Kept separate so callers can turn it off
# and re-use the returned list on their own.
print_timings_summary <- function(res, top_n) {
  n_pp <- min(top_n, nrow(res$per_pkg))
  n_ph <- nrow(res$per_phase)
  cat("\nval_timings_summary(): top ", n_pp, " pkg(s) by total wall time\n",
      sep = "")
  print(utils::head(res$per_pkg, n_pp))
  cat("\nval_timings_summary(): per-phase aggregates (", n_ph, " phase(s))\n",
      sep = "")
  print(res$per_phase)
  invisible(NULL)
}
