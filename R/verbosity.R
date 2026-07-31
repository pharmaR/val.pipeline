
#' Console verbosity control for `val_*` functions
#'
#' `val.pipeline` emits a large volume of `cat()`-based progress messages
#' during a full `val_pipeline()` run. These helpers give users a single
#' knob (`verbose`) to dial that output up or down without editing source
#' code, and give the package a drop-in `cat()` replacement (`val_msg()`)
#' that respects the current tier.
#'
#' # Tiers
#'
#' * `"quiet"` (0) — silent. Only `warning()` / `stop()` still fire.
#' * `"minimal"` (1) — pipeline + phase banners, a one-line summary per
#'   package as it lands (name, version, risk decision, elapsed time),
#'   and top-level count / runtime summary lines. Intended for
#'   production runs where console output should be scannable.
#' * `"normal"` (2, the default) — everything `val.pipeline` emitted
#'   prior to the introduction of `verbose`. Matches the pre-existing
#'   console output so users upgrading in place see no change unless
#'   they opt in.
#' * `"verbose"` (3) — every `--> downloaded / untarred / initial
#'   reference complete / meta bundle saved` crumb plus per-metric
#'   auto-accept diagnostics. Intended for debugging odd behaviour on a
#'   single package.
#'
#' # Setting the tier
#'
#' Either pass `verbose = "minimal"` (etc.) explicitly to each
#' `val_pipeline()` / `val_build()` / `val_pkg()` / `val_categorize()` /
#' `val_decision()` / `val_pipeline_report()` call, or set a session
#' default with `options(val.pipeline.verbose = "minimal")`. The
#' function argument wins when both are set.
#'
#' @name val-verbosity
#' @keywords internal
NULL

# Ordered levels: index-of-level determines "how much to print". Higher
# values are chattier. The `_at_least()` check compares indices.
.val_verbosity_levels <- c(quiet = 0L,
                           minimal = 1L,
                           normal = 2L,
                           verbose = 3L)


#' Resolve a `verbose` argument to a canonical tier string
#'
#' Accepts the many friendly forms (`NULL`, `TRUE`/`FALSE`, an integer
#' 0-3, or a case-insensitive tier name) and returns one of
#' `c("quiet","minimal","normal","verbose")`. When `x` is `NULL` the
#' session option `val.pipeline.verbose` is consulted, falling back to
#' `"normal"`.
#'
#' @param x The user-supplied `verbose` argument (or `NULL` to read
#'   the session default).
#' @return Character(1), one of `c("quiet","minimal","normal","verbose")`.
#' @keywords internal
resolve_verbose <- function(x = NULL) {
  levels <- names(.val_verbosity_levels)

  if (is.null(x)) {
    x <- getOption("val.pipeline.verbose", "normal")
    # A pathologically-set option (e.g. via yaml round-trip) should not
    # crash the whole pipeline; fall back to "normal" instead.
    if (is.null(x)) return("normal")
  }

  if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    return(if (isTRUE(x)) "normal" else "quiet")
  }

  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    idx <- as.integer(x)
    hit <- names(.val_verbosity_levels)[.val_verbosity_levels == idx]
    if (length(hit) == 1L) return(hit)
    stop("Invalid `verbose` integer: ", x,
         ". Must be one of 0 (quiet), 1 (minimal), 2 (normal), 3 (verbose).",
         call. = FALSE)
  }

  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    xl <- tolower(x)
    if (xl %in% levels) return(xl)
    stop("Invalid `verbose` value: '", x, "'. Must be one of: ",
         paste0("\"", levels, "\"", collapse = ", "), ".",
         call. = FALSE)
  }

  stop("`verbose` must be NULL, a length-1 logical, an integer 0-3, or ",
       "one of ", paste0("\"", levels, "\"", collapse = ", "), ".",
       call. = FALSE)
}


#' Is the current session verbosity at least `min_level`?
#'
#' Reads the session option `val.pipeline.verbose` (falling back to
#' `"normal"`) and compares its tier index against `min_level`.
#'
#' @param min_level Character(1), one of
#'   `c("quiet","minimal","normal","verbose")`.
#' @return `TRUE` when the current tier is at least `min_level`.
#' @keywords internal
val_verbosity_at_least <- function(min_level = "normal") {
  stopifnot(is.character(min_level),
            length(min_level) == 1L,
            min_level %in% names(.val_verbosity_levels))
  curr <- resolve_verbose(NULL)
  unname(.val_verbosity_levels[[curr]] >= .val_verbosity_levels[[min_level]])
}


#' Is the current log-file tier at least `min_level`?
#'
#' Reads the session option `val.pipeline.log_level` (falling back to
#' `"normal"`) and compares its tier index against `min_level`. The
#' log-file tier is intentionally decoupled from the console tier:
#' users often want a quiet console (`verbose = "minimal"`) alongside
#' a rich on-disk log (`log_level = "normal"` or `"verbose"`).
#'
#' @param min_level Character(1), one of
#'   `c("quiet","minimal","normal","verbose")`.
#' @return `TRUE` when the log-file tier is at least `min_level`.
#' @keywords internal
val_log_at_least <- function(min_level = "normal") {
  stopifnot(is.character(min_level),
            length(min_level) == 1L,
            min_level %in% names(.val_verbosity_levels))
  lvl <- getOption("val.pipeline.log_level", "normal")
  if (is.null(lvl) || is.na(lvl)) lvl <- "normal"
  lvl <- tolower(as.character(lvl))
  if (!lvl %in% names(.val_verbosity_levels)) lvl <- "normal"
  unname(.val_verbosity_levels[[lvl]] >= .val_verbosity_levels[[min_level]])
}


#' Should this message be teed to the run log?
#'
#' Returns `TRUE` when both a log file is configured
#' (`options("val.pipeline.log_file")` is a non-empty path) and the
#' current log-file tier is at least `min_level`. Used by
#' [val_msg()], [val_print()], and [val_pkg_summary_line()] to
#' decide whether to `cat()` a copy of their output to disk.
#'
#' @param min_level Character(1). The `min_level` the caller passed
#'   to `val_msg()` / `val_print()`.
#' @return `list(active = <lgl>, path = <chr or NULL>)`.
#' @keywords internal
val_log_target <- function(min_level = "normal") {
  path <- getOption("val.pipeline.log_file", NULL)
  if (is.null(path) || !nzchar(path)) return(list(active = FALSE, path = NULL))
  if (!val_log_at_least(min_level)) return(list(active = FALSE, path = path))
  list(active = TRUE, path = path)
}


#' `cat()` that respects the current verbosity tier
#'
#' Drop-in replacement for `cat()` used throughout the `val_*` internals.
#' No-ops when the current session tier is below `min_level`. Warnings
#' and errors are never gated -- only console chatter is.
#'
#' Additionally tees the message to
#' `options("val.pipeline.log_file")` when that option is set and the
#' current log-file tier (`options("val.pipeline.log_level")`) is at
#' least `min_level`. The log tier is independent of the console tier
#' so a `verbose = "minimal"` run can still capture a full `"normal"`
#' record on disk.
#'
#' @param ... Passed through verbatim to [cat()].
#' @param min_level Character(1). The minimum tier at which this message
#'   should appear. One of `c("minimal", "normal", "verbose")`.
#' @keywords internal
val_msg <- function(..., min_level = "normal") {
  if (val_verbosity_at_least(min_level)) cat(...)
  tgt <- val_log_target(min_level)
  if (tgt$active) {
    # POSIX guarantees atomic O_APPEND for writes < PIPE_BUF (~4 KiB),
    # so concurrent workers appending line-sized messages to the same
    # log over NFSv4.2 don't corrupt each other's output. Larger
    # multi-line writes may interleave, but the val_* internals emit
    # short lines.
    try(cat(..., file = tgt$path, append = TRUE), silent = TRUE)
  }
  invisible(NULL)
}


#' `print()` that respects the current verbosity tier
#'
#' Companion to `val_msg()` for lines that use `print()` (typically to
#' render a small data.frame or table). No-ops when the current session
#' tier is below `min_level`.
#'
#' Also tees the print output to
#' `options("val.pipeline.log_file")` when that option is set and the
#' log-file tier is at least `min_level`.
#'
#' @param x The object to print.
#' @param min_level Character(1). The minimum tier at which the print
#'   should appear. One of `c("minimal", "normal", "verbose")`.
#' @param ... Extra arguments forwarded to [print()].
#' @keywords internal
val_print <- function(x, min_level = "normal", ...) {
  if (val_verbosity_at_least(min_level)) print(x, ...)
  tgt <- val_log_target(min_level)
  if (tgt$active) {
    try({
      out <- utils::capture.output(print(x, ...))
      cat(paste0(paste(out, collapse = "\n"), "\n"),
          file = tgt$path, append = TRUE)
    }, silent = TRUE)
  }
  invisible(NULL)
}


#' Initialize the run log file
#'
#' Sets `options(val.pipeline.log_file = log_file)` and writes an
#' optional header line so downstream `val_msg()` calls append to it.
#' No-op when `log_file` is `NULL` or empty. Creates the parent
#' directory if it does not already exist. Safe to call multiple times
#' -- subsequent calls overwrite the option value but do not truncate
#' the file (all writes use `append = TRUE`).
#'
#' @param log_file Character(1) path. Pass `NULL` to disable teeing.
#' @param header Optional character string to append at initialization.
#'   Typically a "=== val_build() @ <timestamp> ===" banner.
#' @return Invisibly, the resolved log file path (or `NULL`).
#' @keywords internal
init_val_log <- function(log_file, header = NULL) {
  if (is.null(log_file) || !nzchar(log_file)) {
    options(val.pipeline.log_file = NULL)
    return(invisible(NULL))
  }
  parent <- dirname(log_file)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  options(val.pipeline.log_file = log_file)
  if (!is.null(header) && nzchar(header)) {
    try(cat(header, file = log_file, append = TRUE), silent = TRUE)
  }
  invisible(log_file)
}


#' Time an expression and record the elapsed seconds
#'
#' Evaluates `expr`, `val_msg()`'s a `"[timing] <label>: N.NNs"` line
#' at the `"normal"` tier, and appends the elapsed seconds to a
#' per-package accumulator stored in
#' `options("val.pipeline.pkg_timings")` (a named list mapping label
#' -> numeric vector of seconds). [val_pkg()] resets this accumulator
#' at the top of every package assessment and attaches its final
#' contents to the returned `meta_list` as `$timings`; [val_build()]
#' then aggregates all packages' `$timings` into a `timings.csv`
#' under `val_dir`.
#'
#' If the same `label` is fenced more than once inside a single
#' package (e.g. an "initial" and "final" assess pass both fencing
#' `"assess"`), both durations are retained in insertion order --
#' downstream aggregation can sum, mean, or explode as needed.
#'
#' Timings are recorded even when `expr` errors, so a failure inside
#' a fenced block still surfaces "we spent N seconds here before it
#' blew up" in the log / csv.
#'
#' @param label Character(1). Short identifier for the block, e.g.
#'   `"assess_final"`, `"report"`. Used as the list key in the
#'   accumulator and as the tag in the emitted log line.
#' @param expr An unevaluated R expression, typically supplied in
#'   braces: `val_time_block("report", { <expensive call> })`.
#' @return The value of `expr`.
#' @keywords internal
val_time_block <- function(label, expr) {
  stopifnot(is.character(label), length(label) == 1L, nzchar(label))
  t0 <- Sys.time()
  on.exit({
    dur <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    tstore <- getOption("val.pipeline.pkg_timings", list())
    if (!is.list(tstore)) tstore <- list()
    prev <- tstore[[label]]
    tstore[[label]] <- if (is.null(prev)) dur else c(prev, dur)
    options(val.pipeline.pkg_timings = tstore)
    val_msg(sprintf("[timing] %s: %s\n", label,
                    format_runtime_seconds(dur)),
            min_level = "normal")
  }, add = TRUE)
  force(expr)
}


#' Reset the per-package timing accumulator
#'
#' Clears `options("val.pipeline.pkg_timings")` so a fresh package
#' assessment starts with an empty timing dict. Called at the top of
#' [val_pkg()]. Returned invisibly.
#'
#' @return Invisibly, an empty list.
#' @keywords internal
reset_pkg_timings <- function() {
  options(val.pipeline.pkg_timings = list())
  invisible(list())
}


#' Fetch the current per-package timing accumulator
#'
#' Returns the current contents of `options("val.pipeline.pkg_timings")`
#' as a named list. Values are numeric vectors of elapsed seconds
#' (each element = one call to [val_time_block()] with that label).
#' [val_pkg()] fetches this at the end of an assessment and attaches
#' it to the returned `meta_list` for downstream aggregation.
#'
#' @return Named list of numeric vectors.
#' @keywords internal
get_pkg_timings <- function() {
  tstore <- getOption("val.pipeline.pkg_timings", list())
  if (!is.list(tstore)) list() else tstore
}


#' Emit a one-line-per-package summary at the "minimal" tier
#'
#' Formats a compact "\code{   [decision]  <pkg> v<ver>  (elapsed)}"
#' landing line for a single package. Called from `val_pkg()` and from
#' `val_build()`'s skipped-dependency branch so every package lands
#' exactly one visible line in `verbose = "minimal"` mode.
#'
#' @param pkg,ver Character(1). Package name and version.
#' @param decision Character(1). Risk decision string (e.g. `"Low"`,
#'   `"Medium"`, `"High"`).
#' @param elapsed_secs Numeric(1) or `NULL`. Elapsed seconds for the
#'   assessment, formatted via [format_runtime_seconds()] when
#'   non-`NULL`. Pass `NULL` for skipped / cached packages.
#' @param suffix Character(1), optional trailing tag (e.g.
#'   `"(cached)"`, `"(dep-skip)"`) rendered after the elapsed time.
#' @param pkg_idx,pkg_total Integer(1) or `NULL`. Optional
#'   position-in-run counter (e.g. `1` and `1195`) rendered as
#'   `"(1/1195)"` between the timestamp and the decision tag.
#'   When either is `NULL` (the default), the counter is omitted so
#'   standalone callers (e.g. a direct `val_pkg()` call outside a
#'   `val_build()` loop) don't need to fabricate a fake index.
#' @param timestamp `POSIXct(1)`, `character(1)`, or `NULL`. Optional
#'   wall-clock stamp for when this package's summary is emitted.
#'   Rendered as `"[HH:MM]"` in `US/Eastern`. When `NULL` (the
#'   default), `Sys.time()` is used so callers don't have to compute
#'   it themselves.
#' @keywords internal
val_pkg_summary_line <- function(pkg,
                                 ver,
                                 decision,
                                 elapsed_secs = NULL,
                                 suffix = NULL,
                                 pkg_idx = NULL,
                                 pkg_total = NULL,
                                 timestamp = NULL) {
  if (!val_verbosity_at_least("minimal")) return(invisible(NULL))

  # Wall-clock stamp. Compute here rather than in the caller so every
  # summary line reflects the moment it actually prints, not the
  # moment the package started assessing (which for slow pkgs can be
  # tens of minutes apart).
  if (is.null(timestamp)) timestamp <- Sys.time()
  ts_str <- if (inherits(timestamp, c("POSIXct", "POSIXlt", "POSIXt"))) {
    format(timestamp, "%H:%M", tz = "US/Eastern")
  } else if (is.character(timestamp) && length(timestamp) == 1L) {
    timestamp
  } else {
    format(Sys.time(), "%H:%M", tz = "US/Eastern")
  }
  ts_str <- paste0("[", ts_str, "]")

  # Position-in-run counter. Rendered right-aligned so long runs stay
  # visually columnar (e.g. `   (1/1195)` vs `(1195/1195)`).
  if (!is.null(pkg_idx) && !is.null(pkg_total) &&
      is.finite(as.numeric(pkg_idx)) &&
      is.finite(as.numeric(pkg_total))) {
    total_w <- nchar(as.character(pkg_total))
    idx_str <- formatC(as.character(pkg_idx), width = total_w)
    counter_str <- paste0("(", idx_str, "/", pkg_total, ") ")
  } else {
    counter_str <- ""
  }

  dec_str <- formatC(paste0("[", as.character(decision), "]"),
                     width = 9, flag = "-")
  pkg_v <- paste0(pkg, " v", ver)
  pkg_v_str <- formatC(pkg_v, width = 32, flag = "-")

  elapsed_str <- if (is.null(elapsed_secs) ||
                       !is.numeric(elapsed_secs) ||
                       !is.finite(elapsed_secs) ||
                       elapsed_secs < 0) {
    ""
  } else {
    paste0("(", format_runtime_seconds(elapsed_secs), ")")
  }

  suffix_str <- if (is.null(suffix) || !nzchar(suffix)) "" else {
    paste0(" ", suffix)
  }

  cat(paste0("   ", ts_str, " ", counter_str, dec_str, " ", pkg_v_str, " ",
             elapsed_str, suffix_str, "\n"))
  tgt <- val_log_target("minimal")
  if (tgt$active) {
    try(cat(paste0("   ", ts_str, " ", counter_str, dec_str, " ",
                   pkg_v_str, " ", elapsed_str, suffix_str, "\n"),
            file = tgt$path, append = TRUE), silent = TRUE)
  }
  invisible(NULL)
}


#' Apply a `verbose` argument for the duration of the calling function
#'
#' Sets `options(val.pipeline.verbose = <resolved>)` and registers an
#' `on.exit()` handler in the calling function's frame to restore the
#' previous value. Public `val_*` entry points call this once, near the
#' top, so every internal helper (including ones in `R/utils.R` that
#' aren't given a `verbose` arg) can just call `val_msg()` and pick up
#' the correct tier.
#'
#' @param verbose The `verbose` argument from the calling function.
#' @return Invisibly, the resolved tier string.
#' @keywords internal
apply_verbose <- function(verbose) {
  level <- resolve_verbose(verbose)
  old <- options(val.pipeline.verbose = level)
  do.call(
    "on.exit",
    list(substitute(options(o), list(o = old)), add = TRUE, after = FALSE),
    envir = parent.frame()
  )
  invisible(level)
}
