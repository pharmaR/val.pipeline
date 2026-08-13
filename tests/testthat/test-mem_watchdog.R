test_that("sample_peak_rss_mb() returns a numeric MB and a sampler tag", {
  s <- sample_peak_rss_mb()
  expect_type(s, "list")
  expect_named(s, c("peak_rss_mb", "sampler"))
  expect_true(is.numeric(s$peak_rss_mb))
  expect_length(s$peak_rss_mb, 1L)
  expect_true(is.character(s$sampler))
  expect_length(s$sampler, 1L)
  expect_true(s$sampler %in% c("vmhwm", "ps", "unavailable"))
  if (s$sampler != "unavailable") {
    expect_true(is.finite(s$peak_rss_mb))
    expect_gt(s$peak_rss_mb, 0)
  }
})

test_that("sample_peak_rss_mb() copes with an invalid pid", {
  s <- sample_peak_rss_mb(pid = 999999999L)
  # Either "unavailable" or ps errored -> NA_real_. Either way, no crash.
  expect_length(s$peak_rss_mb, 1L)
  expect_true(is.numeric(s$peak_rss_mb))
})

test_that("append_watchdog_row() writes header + row and appends cleanly", {
  tmp <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp), add = TRUE)

  row1 <- list(
    timestamp = "2026-08-13 12:00:00",
    pkg = "dplyr", version = "1.1.4", worker_pid = 42L,
    peak_rss_mb = 512.5, elapsed_sec = 3.2, sampler = "vmhwm",
    errored = FALSE
  )
  row2 <- list(
    timestamp = "2026-08-13 12:00:05",
    pkg = "ggplot2", version = "3.5.0", worker_pid = 43L,
    peak_rss_mb = 1024.0, elapsed_sec = 4.7, sampler = "vmhwm",
    errored = TRUE
  )

  ok1 <- append_watchdog_row(tmp, row1)
  ok2 <- append_watchdog_row(tmp, row2)
  expect_true(ok1)
  expect_true(ok2)

  lines <- readLines(tmp)
  expect_length(lines, 3L)
  expect_match(lines[1], "^timestamp\tpkg\tversion\tworker_pid\tpeak_rss_mb")
  expect_match(lines[2], "dplyr\t1.1.4\t42\t512.5")
  expect_match(lines[3], "ggplot2\t3.5.0\t43\t1024\t4.7\tvmhwm\tTRUE")
})

test_that("read_mem_watchdog_tsv() returns NULL for a missing file", {
  expect_null(read_mem_watchdog_tsv(tempfile()))
})

test_that("read_mem_watchdog_tsv() coerces numeric + logical columns", {
  tmp <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp), add = TRUE)
  append_watchdog_row(tmp, list(
    timestamp = "t", pkg = "a", version = "1.0", worker_pid = 1L,
    peak_rss_mb = 100.5, elapsed_sec = 2.0, sampler = "vmhwm",
    errored = FALSE
  ))
  append_watchdog_row(tmp, list(
    timestamp = "t", pkg = "b", version = "2.0", worker_pid = 2L,
    peak_rss_mb = 200.0, elapsed_sec = 3.0, sampler = "vmhwm",
    errored = TRUE
  ))

  df <- read_mem_watchdog_tsv(tmp)
  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 2L)
  expect_true(is.numeric(df$peak_rss_mb))
  expect_true(is.numeric(df$elapsed_sec))
  expect_true(is.logical(df$errored))
  expect_equal(df$errored, c(FALSE, TRUE))
})

test_that("summarize_mem_watchdog() returns NULL for missing / empty TSV", {
  expect_null(summarize_mem_watchdog(tempfile()))
})

test_that("summarize_mem_watchdog() computes p50/p95/max + top-N + suggestion", {
  tmp <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp), add = TRUE)
  # 20 samples: peaks range 100..2000 MB.
  for (i in 1:20) {
    append_watchdog_row(tmp, list(
      timestamp = "t", pkg = paste0("pkg", i), version = "1.0",
      worker_pid = 100L + i,
      peak_rss_mb = 100 * i,
      elapsed_sec = i, sampler = "vmhwm", errored = FALSE
    ))
  }

  # available_ram_gb = 40, reserve = 8 -> budget 32 GB.
  # p95 of 100..2000 ~= 1900 MB = 1.855 GB -> ~17 workers.
  s <- summarize_mem_watchdog(tmp, available_ram_gb = 40, reserve_gb = 8,
                              top_n = 5L)
  expect_named(s, c("n", "p50_mb", "p95_mb", "max_mb", "top",
                    "suggested_workers", "sampler_mix",
                    "available_ram_gb", "reserve_gb"))
  expect_equal(s$n, 20L)
  expect_equal(s$max_mb, 2000)
  expect_true(s$p95_mb > s$p50_mb)
  expect_equal(nrow(s$top), 5L)
  # Heaviest first.
  expect_equal(s$top$peak_rss_mb[1], 2000)
  expect_true(is.integer(s$suggested_workers))
  expect_gte(s$suggested_workers, 1L)
})

test_that("summarize_mem_watchdog() with tiny RAM budget suggests >= 1 worker", {
  tmp <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp), add = TRUE)
  append_watchdog_row(tmp, list(
    timestamp = "t", pkg = "huge", version = "1.0", worker_pid = 1L,
    peak_rss_mb = 100000, elapsed_sec = 1, sampler = "vmhwm",
    errored = FALSE
  ))
  s <- summarize_mem_watchdog(tmp, available_ram_gb = 10, reserve_gb = 8)
  # p95 = 100 GB but budget is only 2 GB -> floor(2/100) = 0, floored to 1.
  expect_equal(s$suggested_workers, 1L)
})
