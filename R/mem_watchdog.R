#' Per-Package Memory Watchdog Helpers
#'
#' Lightweight helpers that let [val_build()] / [val_pkg()] record per-package
#' peak RSS to a run-scoped TSV (`<val_dir>/mem_watchdog.tsv`). The goal is
#' pure observability: sizing `workers` on future runs from real numbers
#' instead of eyeballed guesses. See #122.
#'
#' On Linux, `sample_peak_rss_mb()` reads `VmHWM:` from `/proc/<pid>/status`,
#' which is the kernel-tracked high-water mark for that R subprocess since it
#' was booted. On macOS / Windows we fall back to the (non-peak) current RSS
#' via `ps::ps_memory_info()` — still useful, and clearly documented in the
#' `sampler` column so downstream summaries can flag it.
#'
#' The TSV is written line-at-a-time with `cat(append = TRUE)`; under Linux
#' NFSv4.2 (Workbench) `O_APPEND` writes shorter than PIPE_BUF are atomic, so
#' concurrent worker appends interleave cleanly. Same reasoning as the run
#' log wiring landed for #87.
#'
#' @keywords internal
#' @name mem_watchdog
NULL


#' Sample the peak RSS (in MB) for a process
#'
#' @param pid Integer(1). Process ID. Defaults to `Sys.getpid()`.
#'
#' @return A list with `peak_rss_mb` (numeric(1); `NA_real_` on failure) and
#'   `sampler` (character(1); one of `"vmhwm"`, `"ps"`, or `"unavailable"`).
#'
#' @keywords internal
sample_peak_rss_mb <- function(pid = Sys.getpid()) {
  status_path <- paste0("/proc/", pid, "/status")
  if (file.exists(status_path)) {
    txt <- tryCatch(readLines(status_path, warn = FALSE),
                    error = function(e) character(0))
    hit <- grep("^VmHWM:", txt, value = TRUE)
    if (length(hit) == 1L) {
      kb <- suppressWarnings(as.numeric(
        sub(".*?([0-9]+).*", "\\1", hit)
      ))
      if (is.finite(kb)) {
        return(list(peak_rss_mb = round(kb / 1024, 1),
                    sampler = "vmhwm"))
      }
    }
  }

  if (requireNamespace("ps", quietly = TRUE)) {
    got <- tryCatch({
      h <- ps::ps_handle(pid = as.integer(pid))
      bytes <- ps::ps_memory_info(h)[["rss"]]
      round(bytes / (1024 * 1024), 1)
    }, error = function(e) NA_real_)
    if (is.finite(got)) {
      return(list(peak_rss_mb = got, sampler = "ps"))
    }
  }

  list(peak_rss_mb = NA_real_, sampler = "unavailable")
}


#' Append one row to the mem_watchdog TSV
#'
#' Creates the file with a header on first write, then appends thereafter.
#' Concurrent worker appends are safe: writes are one line, fit in
#' PIPE_BUF, and use `O_APPEND` semantics via [cat()] with `append = TRUE`.
#'
#' @param path Character(1). Path to the TSV, e.g. `<val_dir>/mem_watchdog.tsv`.
#' @param row Named list / one-row data frame. Must include the columns
#'   listed in `mem_watchdog_cols()`.
#'
#' @return `TRUE` on success, `FALSE` on any I/O failure (silently — the
#'   watchdog must never take a run down).
#'
#' @keywords internal
append_watchdog_row <- function(path, row) {
  cols <- mem_watchdog_cols()
  vals <- vapply(cols, function(nm) {
    v <- row[[nm]]
    if (is.null(v) || length(v) == 0L) return("")
    if (is.na(v)) return("")
    as.character(v)
  }, character(1))

  line <- paste(vals, collapse = "\t")

  tryCatch({
    if (!file.exists(path)) {
      cat(paste(cols, collapse = "\t"), "\n", sep = "",
          file = path, append = FALSE)
    }
    cat(line, "\n", sep = "", file = path, append = TRUE)
    TRUE
  }, error = function(e) FALSE)
}


#' Column schema for `mem_watchdog.tsv`
#' @keywords internal
mem_watchdog_cols <- function() {
  c("timestamp", "pkg", "version", "worker_pid",
    "peak_rss_mb", "elapsed_sec", "sampler", "errored")
}


#' Read + coerce `mem_watchdog.tsv`
#'
#' @param path Character(1). Path to a `mem_watchdog.tsv`.
#'
#' @return A tibble with the schema in `mem_watchdog_cols()`, or `NULL` if
#'   the file doesn't exist or can't be parsed.
#'
#' @keywords internal
read_mem_watchdog_tsv <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(
    utils::read.delim(path, sep = "\t", header = TRUE,
                      stringsAsFactors = FALSE,
                      check.names = FALSE, quote = ""),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0L) return(NULL)

  need <- c("pkg", "peak_rss_mb")
  if (!all(need %in% names(df))) return(NULL)

  df$peak_rss_mb <- suppressWarnings(as.numeric(df$peak_rss_mb))
  if ("elapsed_sec" %in% names(df)) {
    df$elapsed_sec <- suppressWarnings(as.numeric(df$elapsed_sec))
  }
  if ("errored" %in% names(df)) {
    df$errored <- tolower(as.character(df$errored)) %in% c("true", "1", "t", "yes")
  }
  tibble::as_tibble(df)
}


#' Summarize a `mem_watchdog.tsv` and (optionally) suggest a `workers` value
#'
#' Called from [val_finalize()] to print a compact summary and, when the
#' current host's `free -g` output is available, suggest a `workers` value
#' for the next run keyed on the p95 per-pkg peak.
#'
#' @param path Character(1). Path to a `mem_watchdog.tsv`.
#' @param available_ram_gb Numeric(1) or `NULL`. Available RAM in GB used
#'   to synthesize the `workers` suggestion. When `NULL`, uses whatever
#'   `MemAvailable:` from `/proc/meminfo` reports (Linux only); otherwise
#'   no suggestion is emitted.
#' @param reserve_gb Numeric(1). GB to hold back for OS + orchestrator R +
#'   spike headroom. Defaults to 8.
#' @param top_n Integer(1). Rows to include in the "heaviest" summary
#'   printed to console. Defaults to 10. The report template renders 25
#'   independently.
#'
#' @return Invisibly, a list with fields `n`, `p50_mb`, `p95_mb`, `max_mb`,
#'   `top`, `suggested_workers` (may be `NULL`), and `sampler_mix`.
#'
#' @keywords internal
summarize_mem_watchdog <- function(path,
                                   available_ram_gb = NULL,
                                   reserve_gb = 8,
                                   top_n = 10L) {
  df <- read_mem_watchdog_tsv(path)
  if (is.null(df)) {
    return(invisible(NULL))
  }

  vals <- df$peak_rss_mb[is.finite(df$peak_rss_mb)]
  if (length(vals) == 0L) return(invisible(NULL))

  p50 <- as.numeric(stats::quantile(vals, 0.50, na.rm = TRUE))
  p95 <- as.numeric(stats::quantile(vals, 0.95, na.rm = TRUE))
  mx  <- max(vals, na.rm = TRUE)

  sampler_mix <- if ("sampler" %in% names(df)) {
    tab <- table(df$sampler, useNA = "no")
    paste(paste0(names(tab), "=", unname(tab)), collapse = ", ")
  } else {
    ""
  }

  top <- df[order(-df$peak_rss_mb), , drop = FALSE]
  top <- utils::head(top, top_n)

  if (is.null(available_ram_gb)) {
    meminfo <- "/proc/meminfo"
    if (file.exists(meminfo)) {
      txt <- tryCatch(readLines(meminfo, warn = FALSE),
                      error = function(e) character(0))
      hit <- grep("^MemAvailable:", txt, value = TRUE)
      if (length(hit) == 1L) {
        kb <- suppressWarnings(as.numeric(
          sub(".*?([0-9]+).*", "\\1", hit)
        ))
        if (is.finite(kb)) available_ram_gb <- kb / (1024 * 1024)
      }
    }
  }

  suggested_workers <- NULL
  if (is.numeric(available_ram_gb) && length(available_ram_gb) == 1L &&
        is.finite(available_ram_gb) && p95 > 0) {
    budget_gb <- max(available_ram_gb - reserve_gb, 0)
    p95_gb    <- p95 / 1024
    suggested_workers <- max(1L, as.integer(floor(budget_gb / p95_gb)))
  }

  invisible(list(
    n = length(vals),
    p50_mb = round(p50, 1),
    p95_mb = round(p95, 1),
    max_mb = round(mx, 1),
    top = top,
    suggested_workers = suggested_workers,
    sampler_mix = sampler_mix,
    available_ram_gb = available_ram_gb,
    reserve_gb = reserve_gb
  ))
}
