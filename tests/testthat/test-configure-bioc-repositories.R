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
  expect_named(out, c("CRAN", "BioC"))
  # After the shim, BiocManager::repositories() should return exactly our repos.
  expect_identical(BiocManager::repositories(),
                   c(CRAN = "https://example.test/cran",
                     BioC = "https://example.test/bioc"))
})

test_that("configure_bioc_repositories() accepts an explicit `repos` vector", {
  skip_if_not_installed("BiocManager")
  withr::local_options(list(repos = c(CRAN = "https://ignored.test/cran")))
  out <- configure_bioc_repositories(
    repos = c(CRAN = "https://explicit.test/cran",
              BioC = "https://explicit.test/bioc"),
    quiet = TRUE
  )
  expect_identical(out,
                   c(CRAN = "https://explicit.test/cran",
                     BioC = "https://explicit.test/bioc"))
  expect_identical(BiocManager::repositories(),
                   c(CRAN = "https://explicit.test/cran",
                     BioC = "https://explicit.test/bioc"))
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
