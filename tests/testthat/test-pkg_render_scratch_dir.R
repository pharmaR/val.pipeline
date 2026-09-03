test_that("pkg_render_scratch_dir allocates unique paths per invocation", {
  reports <- withr::local_tempdir()
  pkg_v <- "somepkg_1.2.3"

  a <- pkg_render_scratch_dir(reports, pkg_v)
  b <- pkg_render_scratch_dir(reports, pkg_v)

  expect_false(identical(a, b))
  expect_false(file.exists(a))
  expect_false(file.exists(b))
  expect_true(startsWith(basename(a), paste0(".render_", pkg_v, "_")))
  expect_true(startsWith(basename(b), paste0(".render_", pkg_v, "_")))
  expect_equal(normalizePath(dirname(a), mustWork = FALSE),
               normalizePath(reports, mustWork = FALSE))
})

test_that("pkg_render_scratch_dir does not collide with a legacy fixed dir", {
  # A previous release used a fixed `.render_<pkg>_<ver>/` path. If a run
  # was killed before its on.exit() cleanup fired, that stale tree would
  # be re-used by the next invocation and crash quarto_render(). Confirm
  # a pre-existing legacy dir is never returned.
  reports <- withr::local_tempdir()
  pkg_v <- "somepkg_1.2.3"
  legacy <- file.path(reports, paste0(".render_", pkg_v))
  dir.create(legacy, recursive = TRUE)

  for (i in seq_len(5)) {
    allocated <- pkg_render_scratch_dir(reports, pkg_v)
    expect_false(identical(
      normalizePath(allocated, mustWork = FALSE),
      normalizePath(legacy,    mustWork = FALSE)
    ))
    expect_false(file.exists(allocated))
  }
})
