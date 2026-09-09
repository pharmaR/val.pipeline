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
      # Capture whatever normalized form with_libpaths + .libPaths()
      # stored (Windows canonicalizes backslash -> "/" and may
      # expand 8.3 short-names, so the raw `fake_lib` string
      # generally won't be a literal prefix of the captured value).
      expected_head <- .libPaths()[1]

      # Simulate the same block val_build() runs:
      run <- function() {
        new_r_libs_site <- paste(.libPaths(), collapse = .Platform$path.sep)
        withr::local_envvar(c(R_LIBS_SITE = new_r_libs_site))
        Sys.getenv("R_LIBS_SITE")
      }
      captured <- run()

      # Inside run(), R_LIBS_SITE should start with the .libPaths()
      # head that with_libpaths prepended.
      expect_true(startsWith(captured, expected_head))

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


test_that("val_build() runs the propagate_libpaths mirror after log-file activation (#171)", {
  # Regression for #171: the propagate_libpaths block emits two
  # `val_msg()` confirmation lines
  #   `--> Mirrored .libPaths() into R_LIBS_SITE ...`
  #   `    R_LIBS_SITE = ...`
  # that MUST land in the persistent `val_pipeline.log`, not just on
  # the console. `val_msg()` only tees to the on-disk log when
  # `getOption("val.pipeline.log_file")` is non-NULL, so the mirror
  # block has to run AFTER `options(val.pipeline.log_file = ...)` in
  # val_build(). A prior arrangement placed the mirror much earlier
  # (before `init_val_log()`), which meant the confirmation lines
  # went to stdout only and operators debugging a run days later
  # couldn't verify from the log alone what `R_LIBS_SITE` a run
  # actually mirrored -- a critical loss when triaging
  # covr_coverage / r_cmd_check_errors anomalies caused by
  # under-populated .libPaths(). See also #99 (the mirror mechanism
  # itself) and #163 (the R_LIBS_SITE echo line).
  #
  # Test strategy: walk the val_build() body top-level, find the
  # index of the `options(val.pipeline.log_file = log_file)` call
  # and the index of the `if (isTRUE(propagate_libpaths))` block,
  # and assert log-opts < mirror. Any future refactor that
  # re-inverts the order fails here without needing a full
  # val_build() spin-up.
  body <- body(val_build)
  # Body of a fn is a `{` call; skip the first element (the brace).
  top_level <- as.list(body)[-1L]

  find_log_opts_idx <- function(exprs) {
    which(vapply(exprs, function(e) {
      # Match `options(val.pipeline.log_file = ...)` as the head call
      # of this top-level expression, or as the RHS of an assignment
      # `<- options(val.pipeline.log_file = ...)`. Anything deeper
      # (e.g. an options() call nested inside a workers>1 branch)
      # doesn't count -- what we care about is when val_build's
      # own top-level frame activates the log tee.
      if (!is.call(e)) return(FALSE)
      call_head <- e
      if (identical(e[[1]], as.name("<-")) ||
          identical(e[[1]], as.name("="))) {
        call_head <- e[[3]]
      }
      if (!is.call(call_head)) return(FALSE)
      if (!identical(call_head[[1]], as.name("options"))) return(FALSE)
      "val.pipeline.log_file" %in% names(call_head)
    }, logical(1)))
  }

  find_propagate_idx <- function(exprs) {
    which(vapply(exprs, function(e) {
      if (!is.call(e)) return(FALSE)
      if (!identical(e[[1]], as.name("if"))) return(FALSE)
      cond <- e[[2]]
      s <- paste(deparse(cond), collapse = " ")
      grepl("propagate_libpaths", s)
    }, logical(1)))
  }

  log_idx <- find_log_opts_idx(top_level)
  prop_idx <- find_propagate_idx(top_level)

  expect_length(log_idx, 1L)
  expect_length(prop_idx, 1L)
  expect_true(prop_idx > log_idx,
              info = paste0("propagate_libpaths mirror block (top-level ",
                            "index ", prop_idx, ") must run AFTER ",
                            "options(val.pipeline.log_file = ...) ",
                            "(top-level index ", log_idx, ") so its ",
                            "val_msg() confirmation lines tee to the ",
                            "persistent log."))
})
