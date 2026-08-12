test_that("assess_pkg_version_drift() classifies match / drifted / multi_version / unexpected / missing", {
  tmp <- tempfile("drift_")
  dir.create(file.path(tmp, "assessed"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  assessed <- file.path(tmp, "assessed")

  # On-disk: dplyr match, ggplot2 drifted, tidyr multi_version, orphan unexpected
  saveRDS(list(x = 1), file.path(assessed, "dplyr_1.1.4_meta.rds"))
  saveRDS(list(x = 1), file.path(assessed, "ggplot2_3.4.4_meta.rds"))
  saveRDS(list(x = 1), file.path(assessed, "tidyr_1.3.0_meta.rds"))
  saveRDS(list(x = 1), file.path(assessed, "tidyr_1.3.1_meta.rds"))
  saveRDS(list(x = 1), file.path(assessed, "orphan_0.0.1_meta.rds"))

  # Expected set: dplyr matches, ggplot2 bumped to 3.5.0, tidyr bumped to 1.3.1,
  # cli missing entirely.
  prep <- list(
    pkgs = c("dplyr", "ggplot2", "tidyr", "cli"),
    vers = c("1.1.4",  "3.5.0",   "1.3.1",  "3.6.0")
  )

  out <- assess_pkg_version_drift(tmp, prep = prep, verbose = FALSE)

  expect_setequal(out$pkg,
                  c("dplyr", "ggplot2", "tidyr", "cli", "orphan"))
  get_status <- function(p) out$status[out$pkg == p]
  expect_equal(get_status("dplyr"),   "match")
  expect_equal(get_status("ggplot2"), "drifted")
  expect_equal(get_status("tidyr"),   "multi_version")
  expect_equal(get_status("cli"),     "missing")
  expect_equal(get_status("orphan"),  "unexpected")

  # on_disk_vers collapses across bundles.
  expect_equal(out$on_disk_vers[out$pkg == "tidyr"], "1.3.0, 1.3.1")
  expect_equal(out$n_on_disk[out$pkg == "tidyr"], 2L)
})

test_that("assess_pkg_version_drift() falls back to avail_pkgs when prep is NULL", {
  tmp <- tempfile("drift_")
  dir.create(file.path(tmp, "assessed"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  saveRDS(list(x = 1),
          file.path(tmp, "assessed", "dplyr_1.1.4_meta.rds"))

  ap <- data.frame(Package = c("dplyr", "cli"),
                   Version = c("1.1.5", "3.6.0"),
                   stringsAsFactors = FALSE)
  out <- assess_pkg_version_drift(tmp, avail_pkgs = ap, verbose = FALSE)

  get_status <- function(p) out$status[out$pkg == p]
  expect_equal(get_status("dplyr"), "drifted")
  expect_equal(get_status("cli"),   "missing")
})

test_that("assess_pkg_version_drift() errors when val_dir has no assessed/", {
  tmp <- tempfile("drift_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  expect_error(
    assess_pkg_version_drift(tmp, prep = list(pkgs = character(), vers = character())),
    "assessed"
  )
})

test_that("assess_pkg_version_drift() handles an empty assessed/ dir", {
  tmp <- tempfile("drift_")
  dir.create(file.path(tmp, "assessed"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  prep <- list(pkgs = c("dplyr"), vers = c("1.1.4"))
  out <- assess_pkg_version_drift(tmp, prep = prep, verbose = FALSE)
  expect_equal(nrow(out), 1L)
  expect_equal(out$status, "missing")
})
