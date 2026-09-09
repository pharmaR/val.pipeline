test_that("pull_covr_home_env() is a no-op when HOME is set to a real dir", {
  withr::with_envvar(new = c(HOME = tempdir()), {
    expect_length(pull_covr_home_env(), 0L)
  })
})

test_that("pull_covr_home_env() returns a valid dir when HOME is unset", {
  # withr::with_envvar treats NA as "unset the variable", which is
  # the exact condition we saw in Posit Workbench Local Jobs (#173).
  withr::with_envvar(new = c(HOME = NA), {
    env <- pull_covr_home_env()
    expect_length(env, 1L)
    expect_named(env, "HOME")
    expect_true(nzchar(env[["HOME"]]))
    # Whatever we picked, it must exist -- pandoc rejects a bogus
    # HOME just as loudly as an unset one.
    expect_true(dir.exists(env[["HOME"]]))
    # And it must NOT be the literal tilde string -- that's a
    # signal that path.expand() failed to expand, and pandoc would
    # try to `mkdir "~"` under the cwd.
    expect_false(identical(env[["HOME"]], "~"))
  })
})

test_that("pull_covr_home_env() returns a valid dir when HOME is empty string", {
  # Empty-string HOME behaves like unset for our purposes -- we
  # still need to provide a valid dir.
  withr::with_envvar(new = c(HOME = ""), {
    env <- pull_covr_home_env()
    expect_length(env, 1L)
    expect_named(env, "HOME")
    expect_true(dir.exists(env[["HOME"]]))
  })
})
