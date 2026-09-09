# Tests for the covr coverage caveat probes (val.pipeline #169):
#   * missing_suggests_for_pkg()
#   * test_skip_if_not_installed_refs()
#   * compose_covr_caveat()
#
# The two probes are pure filesystem-readers -- no network, no
# subprocess -- so the tests build a tiny fake package source under
# `withr::local_tempdir()` and assert on the outputs.
#
# `find.package()` is the only source of "installed-ness". We stub it
# with a local mock so the tests don't depend on which packages
# happen to be on the CI runner's `.libPaths()`.

make_fake_pkg_source <- function(root, desc_fields = list(),
                                 test_files = list()) {
  # `desc_fields` is a named list of DESCRIPTION field -> value.
  # `test_files` is a named list of "relative/path.R" -> character
  # vector of file contents.
  pkg <- file.path(root, "fakepkg")
  dir.create(pkg)
  base <- list(
    Package = "fakepkg",
    Version = "0.0.1",
    Title = "Fake",
    Description = "Fake",
    License = "MIT"
  )
  fields <- utils::modifyList(base, desc_fields)
  desc <- do.call(rbind, list(unlist(fields)))
  colnames(desc) <- names(fields)
  write.dcf(desc, file = file.path(pkg, "DESCRIPTION"))

  if (length(test_files) > 0L) {
    dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
    for (nm in names(test_files)) {
      f <- file.path(pkg, "tests", nm)
      dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
      writeLines(test_files[[nm]], f)
    }
  }
  pkg
}

with_stubbed_installed <- function(installed, code) {
  # Stub the internal `is_pkg_installed()` wrapper via testthat 3e's
  # `local_mocked_bindings()`. Every name in `installed` returns
  # TRUE; every other name returns FALSE. Mocking a val.pipeline-
  # defined function (as opposed to base's `find.package`) is what
  # makes this work -- imported bindings aren't mockable.
  is_pkg_installed_stub <- function(pkg) isTRUE(pkg %in% installed)
  testthat::local_mocked_bindings(
    is_pkg_installed = is_pkg_installed_stub,
    .package = "val.pipeline"
  )
  force(code)
}


# --- missing_suggests_for_pkg() ---------------------------------------------

test_that("missing_suggests_for_pkg() returns character(0) on bad inputs", {
  expect_identical(missing_suggests_for_pkg(NULL), character(0))
  expect_identical(missing_suggests_for_pkg(NA_character_), character(0))
  expect_identical(missing_suggests_for_pkg(""), character(0))
  expect_identical(missing_suggests_for_pkg(c("a", "b")), character(0))
  expect_identical(
    missing_suggests_for_pkg("/definitely/does/not/exist"),
    character(0)
  )
})

test_that("missing_suggests_for_pkg() handles pkg with no DESCRIPTION", {
  tmp <- withr::local_tempdir()
  pkg <- file.path(tmp, "no_desc")
  dir.create(pkg)
  expect_identical(missing_suggests_for_pkg(pkg), character(0))
})

test_that("missing_suggests_for_pkg() handles pkg with no Suggests field", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(tmp)
  with_stubbed_installed(character(0), {
    expect_identical(missing_suggests_for_pkg(pkg), character(0))
  })
})

test_that("missing_suggests_for_pkg() returns missing deps in order", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    desc_fields = list(Suggests = "alpha,\n    bravo (>= 1.0),\n    charlie")
  )
  with_stubbed_installed(installed = c("bravo"), {
    out <- missing_suggests_for_pkg(pkg)
    expect_identical(out, c("alpha", "charlie"))
  })
})

test_that("missing_suggests_for_pkg() returns character(0) when all installed", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    desc_fields = list(Suggests = "alpha, bravo")
  )
  with_stubbed_installed(installed = c("alpha", "bravo"), {
    expect_identical(missing_suggests_for_pkg(pkg), character(0))
  })
})

test_that("missing_suggests_for_pkg() strips version constraints", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    desc_fields = list(Suggests = "delta (>= 2.1.0), echo(>=1.0)")
  )
  with_stubbed_installed(character(0), {
    expect_identical(missing_suggests_for_pkg(pkg), c("delta", "echo"))
  })
})


# --- test_skip_if_not_installed_refs() --------------------------------------

test_that("test_skip_if_not_installed_refs() handles bad inputs", {
  expect_identical(test_skip_if_not_installed_refs(NULL), character(0))
  expect_identical(test_skip_if_not_installed_refs(NA_character_),
                   character(0))
  expect_identical(test_skip_if_not_installed_refs(""), character(0))
  expect_identical(
    test_skip_if_not_installed_refs("/definitely/does/not/exist"),
    character(0)
  )
})

test_that("test_skip_if_not_installed_refs() returns character(0) when no tests/", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(tmp)
  expect_identical(test_skip_if_not_installed_refs(pkg), character(0))
})

test_that("test_skip_if_not_installed_refs() picks up double-quoted refs", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    test_files = list(
      "testthat/test-foo.R" = c(
        'test_that("foo", {',
        '  skip_if_not_installed("readr")',
        '  expect_equal(1, 1)',
        '})'
      )
    )
  )
  expect_identical(test_skip_if_not_installed_refs(pkg), "readr")
})

test_that("test_skip_if_not_installed_refs() picks up single-quoted refs", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    test_files = list(
      "testthat/test-bar.R" = c(
        "test_that('bar', {",
        "  skip_if_not_installed('haven')",
        "  expect_equal(1, 1)",
        "})"
      )
    )
  )
  expect_identical(test_skip_if_not_installed_refs(pkg), "haven")
})

test_that("test_skip_if_not_installed_refs() picks up testthat:: qualifier", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    test_files = list(
      "testthat/test-baz.R" = c(
        'testthat::skip_if_not_installed("here")'
      )
    )
  )
  expect_identical(test_skip_if_not_installed_refs(pkg), "here")
})

test_that("test_skip_if_not_installed_refs() dedups + sorts across files", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    test_files = list(
      "testthat/test-a.R" = c(
        'skip_if_not_installed("readr")',
        'skip_if_not_installed("readr")',
        'skip_if_not_installed("tplyr")'
      ),
      "testthat/test-b.R" = c(
        'testthat::skip_if_not_installed("haven")',
        'skip_if_not_installed("readr")'
      )
    )
  )
  expect_identical(
    test_skip_if_not_installed_refs(pkg),
    c("haven", "readr", "tplyr")
  )
})

test_that("test_skip_if_not_installed_refs() ignores unrelated skip fns", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    test_files = list(
      "testthat/test-c.R" = c(
        'skip_on_cran()',
        'skip_if(FALSE)',
        'skip_if_not("readr")  # different function entirely'
      )
    )
  )
  expect_identical(test_skip_if_not_installed_refs(pkg), character(0))
})


# --- compose_covr_caveat() --------------------------------------------------

test_that("compose_covr_caveat() returns NULL when nothing missing", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    desc_fields = list(Suggests = "alpha"),
    test_files = list(
      "testthat/test-a.R" = 'skip_if_not_installed("alpha")'
    )
  )
  with_stubbed_installed(installed = c("alpha"), {
    expect_null(compose_covr_caveat(pkg))
  })
})

test_that("compose_covr_caveat() surfaces silent_skip_pkgs intersection", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    desc_fields = list(Suggests = "readr, tplyr, haven"),
    test_files = list(
      "testthat/test-parse.R" = c(
        'skip_if_not_installed("readr")',
        'skip_if_not_installed("haven")'
      )
    )
  )
  # readr + tplyr missing; haven installed. The intersection with
  # skip_if_not_installed refs (readr, haven) is just readr.
  with_stubbed_installed(installed = c("haven"), {
    out <- compose_covr_caveat(pkg)
    expect_type(out, "list")
    expect_identical(out$missing_suggests, c("readr", "tplyr"))
    expect_identical(out$silent_skip_pkgs, "readr")
  })
})

test_that("compose_covr_caveat() populates missing_suggests even w/o skip refs", {
  tmp <- withr::local_tempdir()
  pkg <- make_fake_pkg_source(
    tmp,
    desc_fields = list(Suggests = "alpha, bravo")
  )
  with_stubbed_installed(character(0), {
    out <- compose_covr_caveat(pkg)
    expect_identical(out$missing_suggests, c("alpha", "bravo"))
    expect_identical(out$silent_skip_pkgs, character(0))
  })
})

test_that("meta bundle -> qual_metadata transform preserves covr_caveat as list-cols (#169 review)", {
  # Direct exercise of the streaming-pass transform val_finalize()
  # applies to each `_meta.rds` bundle: pull the two variable-length
  # covr_caveat_* character vectors out of `bundle`, `list_flatten()`
  # the rest, and re-wrap the caveat slots as list-cols so a
  # `bind_rows()` across a mixed cohort survives ragged shapes and
  # missing-fields on legacy bundles. Deliberately does NOT drive
  # `val_finalize()` end-to-end -- that requires a val_dir scaffold
  # (config.yml, val_start, reject_iteration deps, ...) that would
  # brittle-couple this test to unrelated finalize plumbing.
  # Mirror the exact selector/wrap sequence in
  # R/val_finalize.R so a shape regression there fails here too.
  transform_one <- function(bundle) {
    caveat_miss   <- bundle[["covr_caveat_missing_suggests"]]
    caveat_silent <- bundle[["covr_caveat_silent_skip_pkgs"]]
    bundle[["covr_caveat_missing_suggests"]] <- NULL
    bundle[["covr_caveat_silent_skip_pkgs"]] <- NULL
    x <- purrr::list_flatten(bundle)
    x$covr_caveat_missing_suggests <- list(caveat_miss)
    x$covr_caveat_silent_skip_pkgs <- list(caveat_silent)
    dplyr::as_tibble(x)
  }

  alpha <- list(
    pkg = "alpha", ver = "0.0.0",
    covr_caveat_missing_suggests = c("readr", "tplyr"),
    covr_caveat_silent_skip_pkgs = "readr"
  )
  # Bravo emulates a pre-#169 bundle: the fields don't exist at all.
  bravo <- list(pkg = "bravo", ver = "0.0.0")

  qm0 <- dplyr::bind_rows(transform_one(alpha), transform_one(bravo))

  expect_true("covr_caveat_missing_suggests" %in% names(qm0))
  expect_true("covr_caveat_silent_skip_pkgs" %in% names(qm0))
  expect_true(is.list(qm0$covr_caveat_missing_suggests))
  expect_true(is.list(qm0$covr_caveat_silent_skip_pkgs))

  alpha_row <- qm0[qm0$pkg == "alpha", ]
  expect_identical(alpha_row$covr_caveat_missing_suggests[[1]],
                   c("readr", "tplyr"))
  expect_identical(alpha_row$covr_caveat_silent_skip_pkgs[[1]], "readr")

  bravo_row <- qm0[qm0$pkg == "bravo", ]
  expect_null(bravo_row$covr_caveat_missing_suggests[[1]])
  expect_null(bravo_row$covr_caveat_silent_skip_pkgs[[1]])
})
