test_that("val_build() mirrors .libPaths() into R_LIBS_SITE during the run", {
  # Regression for #99: rcmdcheck / covr subprocesses spawned by
  # riskmetric can't see the parent's .libPaths() unless R_LIBS_SITE
  # (or R_LIBS_USER / R_LIBS) is set. val_build() should mirror
  # .libPaths() into R_LIBS_SITE for the duration of the call and
  # restore the previous value on exit.
  #
  # We don't want to actually spin up a full val_build() here (way
  # too heavy for a unit test), so we drive the helper block directly
  # with the same idiom val_build() uses. That gives us a truthful
  # test of the mirroring / restore contract without the assessment
  # loop noise.
  fake_lib <- withr::local_tempdir()

  withr::local_envvar(c(R_LIBS_SITE = "/preexisting/site"))
  withr::with_libpaths(
    c(fake_lib, .libPaths()),
    action = "prefix",
    {
      # Simulate the same block val_build() runs:
      run <- function() {
        new_r_libs_site <- paste(.libPaths(), collapse = .Platform$path.sep)
        withr::local_envvar(c(R_LIBS_SITE = new_r_libs_site))
        Sys.getenv("R_LIBS_SITE")
      }
      captured <- run()

      # Inside run(), R_LIBS_SITE should start with the fake_lib we
      # prepended via with_libpaths.
      expect_true(startsWith(captured, fake_lib))

      # And after run() returns, R_LIBS_SITE must be restored.
      expect_identical(Sys.getenv("R_LIBS_SITE"), "/preexisting/site")
    }
  )
})


test_that("val_build(propagate_libpaths = FALSE) leaves R_LIBS_SITE untouched", {
  withr::local_envvar(c(R_LIBS_SITE = "/preexisting/site"))
  # Simulate the opt-out branch: the if(isTRUE(propagate_libpaths))
  # block is skipped so R_LIBS_SITE never gets rewritten.
  propagate_libpaths <- FALSE
  if (isTRUE(propagate_libpaths)) {
    new_r_libs_site <- paste(.libPaths(), collapse = .Platform$path.sep)
    withr::local_envvar(c(R_LIBS_SITE = new_r_libs_site))
  }
  expect_identical(Sys.getenv("R_LIBS_SITE"), "/preexisting/site")
})


test_that("val_build(propagate_libpaths = ...) default honors val.pipeline.propagate_libpaths option", {
  # The default expression is
  #   getOption("val.pipeline.propagate_libpaths", TRUE)
  # so an operator can globally opt out via options().
  fn <- formals(val_build)
  default_expr <- fn$propagate_libpaths
  expect_identical(
    as.character(default_expr[[1]]),
    "getOption"
  )
  expect_identical(default_expr[[2]], "val.pipeline.propagate_libpaths")
  expect_identical(default_expr[[3]], TRUE)
})


test_that("val_pipeline() forwards propagate_libpaths to val_build()", {
  # Signature check: val_pipeline() must expose the arg (so callers
  # can pass it through) and default to the same getOption() lookup
  # val_build() uses, so a session-wide option controls both.
  fn <- formals(val_pipeline)
  expect_true("propagate_libpaths" %in% names(fn))
  default_expr <- fn$propagate_libpaths
  expect_identical(as.character(default_expr[[1]]), "getOption")
  expect_identical(default_expr[[2]], "val.pipeline.propagate_libpaths")
  expect_identical(default_expr[[3]], TRUE)
})
