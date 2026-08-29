test_that("probe_writable_dir succeeds on a writable dir", {
  d <- withr::local_tempdir()
  res <- probe_writable_dir(d)
  expect_true(res$ok)
  expect_identical(res$reason, "")
  # Probe file should be cleaned up.
  expect_length(
    list.files(d, all.files = TRUE, no.. = TRUE, pattern = "^\\.val_pipeline_probe_"),
    0L
  )
})

test_that("probe_writable_dir reports 'missing' when dir absent", {
  d <- file.path(withr::local_tempdir(), "does_not_exist")
  res <- probe_writable_dir(d, create = FALSE)
  expect_false(res$ok)
  expect_identical(res$reason, "missing")
})

test_that("probe_writable_dir creates the dir when create=TRUE", {
  root <- withr::local_tempdir()
  d <- file.path(root, "child")
  res <- probe_writable_dir(d, create = TRUE)
  expect_true(res$ok)
  expect_true(dir.exists(d))
})

test_that("probe_writable_dir reports empty path", {
  res <- probe_writable_dir("")
  expect_false(res$ok)
  expect_identical(res$reason, "empty path")
})

test_that("probe_writable_dir surfaces eacces on a read-only dir", {
  skip_on_os("windows")
  skip_if(Sys.info()[["effective_user"]] == "root",
          "root bypasses filesystem perms")
  d <- withr::local_tempdir()
  Sys.chmod(d, mode = "0500")  # r-x for owner, no write
  withr::defer(Sys.chmod(d, mode = "0700"))
  res <- probe_writable_dir(d)
  expect_false(res$ok)
  expect_identical(res$reason, "eacces")
})

test_that("assert_writable_dirs returns invisibly on success", {
  d1 <- withr::local_tempdir()
  d2 <- withr::local_tempdir()
  expect_invisible(assert_writable_dirs(c(a = d1, b = d2)))
})

test_that("assert_writable_dirs raises a labelled error listing offenders", {
  d_ok  <- withr::local_tempdir()
  d_bad <- file.path(withr::local_tempdir(), "missing")
  expect_error(
    assert_writable_dirs(c(good = d_ok, bad = d_bad), context = "test"),
    class = "val_pipeline_write_probe_failure"
  )
  err <- tryCatch(
    assert_writable_dirs(c(good = d_ok, bad = d_bad), context = "test"),
    val_pipeline_write_probe_failure = function(e) e
  )
  expect_match(conditionMessage(err), "\\[test\\]")
  expect_match(conditionMessage(err), "bad:")
  expect_match(conditionMessage(err), "missing")
  # The good path must not be listed as an offender.
  expect_no_match(conditionMessage(err), "good:")
})
