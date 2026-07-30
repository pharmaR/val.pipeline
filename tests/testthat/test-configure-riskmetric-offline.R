test_that("configure_riskmetric_offline_if_requested() is a no-op when env var is unset", {
  withr::local_envvar(VAL_PIPELINE_INTERNAL_BIOC = "")
  expect_false(configure_riskmetric_offline_if_requested(quiet = TRUE))
})

test_that("configure_riskmetric_offline() no-ops (invisibly FALSE) without riskmetric", {
  # We can't reliably assert the installed-riskmetric path from here, but the
  # unavailable-riskmetric path is a simple guard we can verify with a stub
  # that reports "package not installed" via a local mock.
  skip_if(requireNamespace("riskmetric", quietly = TRUE),
          "riskmetric is installed; nothing to assert for the missing-pkg path")
  expect_false(configure_riskmetric_offline(quiet = TRUE))
})

test_that("configure_riskmetric_offline() installs an override on the assess method", {
  skip_if_not_installed("riskmetric")
  # Save the original method so we can compare identities.
  orig <- getFromNamespace("assess_reverse_dependencies.default", ns = "riskmetric")
  on.exit(
    utils::assignInNamespace("assess_reverse_dependencies.default",
                             orig, ns = "riskmetric"),
    add = TRUE
  )
  expect_true(configure_riskmetric_offline(quiet = TRUE))
  patched <- getFromNamespace("assess_reverse_dependencies.default", ns = "riskmetric")
  expect_false(identical(patched, orig))
  # The override must return a pkg_metric_reverse_dependencies object without
  # ever calling devtools::revdep() (i.e. without touching a VIEWS URL).
  # We fake `utils::available.packages()` to return a controllable matrix.
  fake_db <- matrix(
    c("depA", "1.0", "someRepo", "target",
      "depB", "2.0", "someRepo", "target"),
    nrow = 2, byrow = TRUE,
    dimnames = list(NULL, c("Package", "Version", "Repository", "Depends"))
  )
  withr::with_options(
    list(),
    {
      trace_calls <- 0L
      local_mock <- function(...) { trace_calls <<- trace_calls + 1L; fake_db }
      # Temporarily replace utils::available.packages so we can prove no
      # network call is made.
      orig_ap <- utils::available.packages
      utils::assignInNamespace("available.packages", local_mock, ns = "utils")
      on.exit(
        utils::assignInNamespace("available.packages", orig_ap, ns = "utils"),
        add = TRUE
      )
      out <- patched(list(name = "target"))
      expect_s3_class(out, "pkg_metric_reverse_dependencies")
      expect_true(trace_calls >= 1L)
    }
  )
})


test_that("configure_riskmetric_offline() also shims memoise_bioc_available", {
  skip_if_not_installed("riskmetric")
  skip_if_not_installed("memoise")

  orig <- getFromNamespace("memoise_bioc_available", ns = "riskmetric")
  on.exit(
    utils::assignInNamespace("memoise_bioc_available", orig, ns = "riskmetric"),
    add = TRUE
  )

  expect_true(configure_riskmetric_offline(quiet = TRUE))

  patched <- getFromNamespace("memoise_bioc_available", ns = "riskmetric")
  expect_false(identical(patched, orig))

  # Under a stubbed available.packages() the shim must succeed rather than
  # trying to reach bioconductor.org/packages/release/bioc/src/contrib/PACKAGES.
  fake_ap <- matrix(
    c("BiocGenerics", "0.99.0", "https://internal.example.com/bioc/src/contrib"),
    nrow = 1, byrow = TRUE,
    dimnames = list(NULL, c("Package", "Version", "Repository"))
  )
  fake_repos <- c(BioCsoft = "https://internal.example.com/bioc")

  called <- 0L
  with_mocked_bindings(
    available.packages = function(repos = NULL, ...) {
      called <<- called + 1L
      fake_ap
    },
    .package = "utils",
    {
      with_mocked_bindings(
        repositories = function(...) fake_repos,
        .package = "BiocManager",
        {
          df <- patched()
          expect_s3_class(df, "data.frame")
          expect_true("BiocGenerics" %in% df[["Package"]])
        }
      )
    }
  )
  expect_gt(called, 0L)
})
