test_that("configure_bioc_repositories_if_requested() is a no-op when env var is unset", {
  withr::local_envvar(VAL_PIPELINE_INTERNAL_BIOC = "")
  out <- configure_bioc_repositories_if_requested(quiet = TRUE)
  expect_identical(out, character(0))
})

test_that("configure_bioc_repositories_if_requested() honors truthy env values", {
  skip_if_not_installed("BiocManager")
  withr::local_envvar(VAL_PIPELINE_INTERNAL_BIOC = "1")
  withr::local_options(list(
    repos = c(CRAN = "https://example.test/cran",
              BioC = "https://example.test/bioc")
  ))
  out <- configure_bioc_repositories_if_requested(quiet = TRUE)
  # The shim now auto-populates the classic BioC* aliases (BioCsoft,
  # BioCann, BioCexp, BioCworkflows, BioCbooks) so downstream lookups
  # like BiocManager::repositories()[["BioCsoft"]] resolve. The caller's
  # CRAN + BioC entries must survive untouched.
  expect_true(all(c("CRAN", "BioC") %in% names(out)))
  expect_identical(unname(out[["CRAN"]]), "https://example.test/cran")
  expect_identical(unname(out[["BioC"]]), "https://example.test/bioc")
  repos_after <- BiocManager::repositories()
  expect_identical(unname(repos_after[["CRAN"]]), "https://example.test/cran")
  expect_identical(unname(repos_after[["BioC"]]), "https://example.test/bioc")
})

test_that("configure_bioc_repositories() accepts an explicit `repos` vector", {
  skip_if_not_installed("BiocManager")
  withr::local_options(list(repos = c(CRAN = "https://ignored.test/cran")))
  out <- configure_bioc_repositories(
    repos = c(CRAN = "https://explicit.test/cran",
              BioC = "https://explicit.test/bioc"),
    quiet = TRUE
  )
  # The caller's CRAN + BioC entries must survive untouched. The shim
  # additionally populates the BioC* aliases — that is covered by the
  # dedicated aliasing test below and is not re-asserted here.
  expect_true(all(c("CRAN", "BioC") %in% names(out)))
  expect_identical(unname(out[["CRAN"]]), "https://explicit.test/cran")
  expect_identical(unname(out[["BioC"]]), "https://explicit.test/bioc")
  repos_after <- BiocManager::repositories()
  expect_identical(unname(repos_after[["CRAN"]]), "https://explicit.test/cran")
  expect_identical(unname(repos_after[["BioC"]]), "https://explicit.test/bioc")
})

test_that("configure_bioc_repositories() aliases a flat BioC entry to the BioC* names", {
  skip_if_not_installed("BiocManager")
  out <- configure_bioc_repositories(
    repos = c(CRAN = "https://explicit.test/cran",
              BioC = "https://explicit.test/bioc"),
    quiet = TRUE
  )
  expect_true(all(c("CRAN", "BioC", "BioCsoft", "BioCann", "BioCexp",
                    "BioCworkflows", "BioCbooks") %in% names(out)))
  expect_identical(unname(out[["BioCsoft"]]), "https://explicit.test/bioc")
  expect_identical(unname(BiocManager::repositories()[["BioCsoft"]]),
                   "https://explicit.test/bioc")
})

test_that("configure_bioc_repositories() rejects an unnamed `repos` vector", {
  expect_error(
    configure_bioc_repositories(
      repos = c("https://example.test/cran"),
      quiet = TRUE
    ),
    "named"
  )
})
