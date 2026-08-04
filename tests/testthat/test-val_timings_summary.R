make_fake_timings <- function() {
  data.frame(
    pkg     = c("dplyr", "dplyr", "dplyr",
                "ggplot2", "ggplot2",
                "slowpkg", "slowpkg", "slowpkg"),
    ver     = c("1.2.1", "1.2.1", "1.2.1",
                "3.5.1", "3.5.1",
                "0.1.0", "0.1.0", "0.1.0"),
    phase   = c("install", "assess", "covr_coverage",
                "install", "assess",
                "install", "assess", "covr_coverage"),
    seconds = c(3, 1, 10,
                2, 1,
                50, 5, 3600),
    stringsAsFactors = FALSE
  )
}


test_that("val_timings_summary() returns per_pkg / per_phase / wide tibbles", {
  res <- val_timings_summary(make_fake_timings(), quiet = TRUE)

  expect_named(res, c("per_pkg", "per_phase", "wide"))
  expect_s3_class(res$per_pkg,   "tbl_df")
  expect_s3_class(res$per_phase, "tbl_df")
  expect_s3_class(res$wide,      "tbl_df")

  # per_pkg: correct totals, sorted desc by total_s.
  expect_equal(res$per_pkg$pkg,     c("slowpkg", "dplyr", "ggplot2"))
  expect_equal(res$per_pkg$total_s, c(3655, 14, 3))
  expect_equal(res$per_pkg$n_phases, c(3L, 3L, 2L))
  expect_true("ver" %in% names(res$per_pkg))

  # per_phase: correct aggregates, sorted desc by total_s.
  expect_equal(res$per_phase$phase[1], "covr_coverage")
  expect_equal(res$per_phase$total_s[res$per_phase$phase == "install"], 55)
  expect_equal(res$per_phase$n_pkgs[res$per_phase$phase == "install"], 3L)

  # wide: one row per pkg, one col per phase.
  expect_setequal(names(res$wide),
                  c("pkg", "ver", "install", "assess", "covr_coverage"))
  # Missing (ggplot2, covr_coverage) came through as NA.
  gg_cov <- res$wide$covr_coverage[res$wide$pkg == "ggplot2"]
  expect_true(is.na(gg_cov))
})


test_that("val_timings_summary() reads from a CSV path", {
  csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(make_fake_timings(), csv, row.names = FALSE)

  res <- val_timings_summary(csv, quiet = TRUE)
  expect_equal(res$per_pkg$pkg, c("slowpkg", "dplyr", "ggplot2"))
})


test_that("val_timings_summary() reads timings.csv from a run directory", {
  dir <- withr::local_tempdir()
  utils::write.csv(make_fake_timings(),
                   file.path(dir, "timings.csv"),
                   row.names = FALSE)

  res <- val_timings_summary(dir, quiet = TRUE)
  expect_equal(res$per_pkg$pkg, c("slowpkg", "dplyr", "ggplot2"))
})


test_that("val_timings_summary() errors clearly when the directory has no timings.csv", {
  dir <- withr::local_tempdir()
  expect_error(
    val_timings_summary(dir, quiet = TRUE),
    "no 'timings.csv'"
  )
})


test_that("val_timings_summary() errors when the file / directory doesn't exist", {
  expect_error(
    val_timings_summary(tempfile(), quiet = TRUE),
    "Path does not exist"
  )
})


test_that("val_timings_summary() errors when required columns are missing", {
  bad <- data.frame(pkg = "x", seconds = 1)  # no 'phase'
  expect_error(
    val_timings_summary(bad, quiet = TRUE),
    "missing required column.*phase"
  )
})


test_that("val_timings_summary() works when 'ver' is absent", {
  df <- make_fake_timings()
  df$ver <- NULL

  res <- val_timings_summary(df, quiet = TRUE)
  expect_false("ver" %in% names(res$per_pkg))
  expect_false("ver" %in% names(res$wide))
  expect_equal(res$per_pkg$pkg, c("slowpkg", "dplyr", "ggplot2"))
})


test_that("val_timings_summary() drops non-numeric 'seconds' with a warning", {
  df <- make_fake_timings()
  df$seconds <- as.character(df$seconds)
  df$seconds[1] <- "bogus"

  expect_warning(
    res <- val_timings_summary(df, quiet = TRUE),
    "Dropping 1 row"
  )
  # dplyr install phase now missing 3s -> per_pkg total drops.
  expect_equal(res$per_pkg$total_s[res$per_pkg$pkg == "dplyr"], 11)
})


test_that("val_timings_summary() collapses duplicate (pkg, phase) rows in wide", {
  # If a pkg somehow logs the same phase twice (parallel workers,
  # retry), summing is the right default for the wide table.
  df <- rbind(make_fake_timings(),
              data.frame(pkg = "dplyr", ver = "1.2.1",
                         phase = "install", seconds = 7,
                         stringsAsFactors = FALSE))
  res <- val_timings_summary(df, quiet = TRUE)

  # per_pkg total_s picks up the extra 7s.
  expect_equal(res$per_pkg$total_s[res$per_pkg$pkg == "dplyr"], 21)
  # wide$install for dplyr = 3 + 7 = 10 (single non-list numeric cell,
  # NOT a length-2 list -- confirms values_fn = sum kicked in).
  wide_row <- res$wide[res$wide$pkg == "dplyr", ]
  expect_type(wide_row$install, "double")
  expect_equal(wide_row$install, 10)
})


test_that("val_timings_summary() invisibly returns the list even when printing", {
  # top_n > 0 + quiet = FALSE triggers the print path; the return value
  # must still be the same list so the helper is composable.
  res <- withr::with_output_sink(
    tempfile(),
    val_timings_summary(make_fake_timings(),
                        top_n = 5, quiet = FALSE)
  )
  expect_named(res, c("per_pkg", "per_phase", "wide"))
})


test_that("val_timings_summary() validates top_n and quiet args", {
  df <- make_fake_timings()
  expect_error(val_timings_summary(df, top_n = -1, quiet = TRUE))
  expect_error(val_timings_summary(df, top_n = c(1, 2), quiet = TRUE))
  expect_error(val_timings_summary(df, quiet = NA))
})
