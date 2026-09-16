# Fixture helper: builds an available.packages()-shaped matrix.
#
# testthat 3's local_mocked_bindings() with .package = "utils" is the
# ecosystem-native way to intercept utils::available.packages(), which
# is the only outward network call in build_bioc_blocklist(). No new
# test-time dep required.

.mock_ap_matrix <- function(pkgs, versions = NULL,
                            repo = "https://bioconductor.org/packages/3.22/bioc/src/contrib") {
  if (is.null(versions)) versions <- rep("1.0.0", length(pkgs))
  m <- cbind(Package = pkgs, Version = versions, Repository = repo)
  rownames(m) <- pkgs
  m
}


test_that("is_bioc_repo() matches alias, URL, both, and neither", {
  expect_equal(
    is_bioc_repo(c(CRAN = "https://cran.rstudio.com",
                   BioC = "https://bioconductor.org/packages/3.22/bioc")),
    c(FALSE, TRUE)
  )
  # Alias-only match: alias contains "bioc", URL doesn't.
  expect_true(is_bioc_repo(c(BIOC_MIRROR = "https://internal.example.com/bio-r")))
  # URL-only match: alias generic, URL contains "bioc".
  expect_true(is_bioc_repo(c(sci = "https://ppm.example.com/bioconductor/latest")))
  # Non-OSS consolidated case (alias AND URL both match "bioc").
  expect_true(is_bioc_repo(c(BioC = "https://sce-ppm.example.com/bioc-r4.5/latest")))
  # No BioC at all.
  expect_equal(
    is_bioc_repo(c(CRAN = "https://cran.rstudio.com",
                   posit = "https://packagemanager.posit.co/cran/latest")),
    c(FALSE, FALSE)
  )
  # Unnamed input still works (all names default to "").
  expect_equal(
    is_bioc_repo(c("https://cran.rstudio.com",
                   "https://bioconductor.org/packages/3.22/bioc")),
    c(FALSE, TRUE)
  )
})


test_that("build_bioc_blocklist() expands universe and tags reasons", {
  qm <- data.frame(
    pkg = c("affy", "limma", "AnnotationDbi", "dplyr"),
    repo_name = c("BioC", "BioC", "BioC", "CRAN"),
    final_decision = c("Low", "Low", "High", "Low"),
    stringsAsFactors = FALSE
  )
  ap_fake <- .mock_ap_matrix(c("affy", "limma", "AnnotationDbi",
                               "GenomeInfoDb", "SummarizedExperiment"))
  # Universe (5) - Low-risk BioC allowlist (affy, limma) = blocklist (3):
  #   AnnotationDbi -> assessed_High
  #   GenomeInfoDb, SummarizedExperiment -> not_assessed

  testthat::local_mocked_bindings(
    available.packages = function(...) ap_fake,
    .package = "utils"
  )

  bl <- build_bioc_blocklist(
    qm,
    opt_repos = list(BioC = "https://bioconductor.org/packages/3.22/bioc",
                     CRAN = "https://cran.rstudio.com"),
    min_universe = 1L
  )

  expect_setequal(bl$package,
                  c("AnnotationDbi", "GenomeInfoDb", "SummarizedExperiment"))
  expect_equal(bl$reason[bl$package == "AnnotationDbi"], "assessed_High")
  expect_equal(bl$reason[bl$package == "GenomeInfoDb"],  "not_assessed")
  expect_equal(bl$reason[bl$package == "SummarizedExperiment"], "not_assessed")
  # dplyr is CRAN — must not appear in either allow or block.
  expect_false("dplyr" %in% bl$package)
})


test_that("build_bioc_blocklist() recognises URL-only aliases in qm mask", {
  # Alias 'sci' with a BioC URL: qm$repo_name is 'sci' (no "bioc" substring).
  # Previously grepl-only detection would exclude these Low-risk pkgs from
  # the allowlist and wrongly blocklist them.
  qm <- data.frame(
    pkg = c("affy", "AnnotationDbi"),
    repo_name = c("sci", "sci"),
    final_decision = c("Low", "High"),
    stringsAsFactors = FALSE
  )
  ap_fake <- .mock_ap_matrix(c("affy", "AnnotationDbi", "GenomeInfoDb"))

  testthat::local_mocked_bindings(
    available.packages = function(...) ap_fake,
    .package = "utils"
  )

  bl <- build_bioc_blocklist(
    qm,
    opt_repos = list(sci  = "https://bioconductor.org/packages/3.22/bioc",
                     CRAN = "https://cran.rstudio.com"),
    min_universe = 1L
  )

  expect_false("affy" %in% bl$package)                # allowlisted
  expect_true("AnnotationDbi" %in% bl$package)        # blocklisted (High)
  expect_equal(bl$reason[bl$package == "AnnotationDbi"], "assessed_High")
  expect_true("GenomeInfoDb" %in% bl$package)         # blocklisted (not_assessed)
})


test_that("build_bioc_blocklist() recognises riskmetric-derived labels (BioCsoft)", {
  # Real prod runs have qm$repo_name = "BioCsoft" even when the config
  # alias is "BioC" (riskmetric re-parses the URL and returns the more
  # specific subdomain). The substring-fallback leg of the mask must
  # catch this case.
  qm <- data.frame(
    pkg = c("affy", "AnnotationDbi"),
    repo_name = c("BioCsoft", "BioCsoft"),
    final_decision = c("Low", "High"),
    stringsAsFactors = FALSE
  )
  ap_fake <- .mock_ap_matrix(c("affy", "AnnotationDbi"))

  testthat::local_mocked_bindings(
    available.packages = function(...) ap_fake,
    .package = "utils"
  )

  bl <- build_bioc_blocklist(
    qm,
    opt_repos = list(BioC = "https://bioconductor.org/packages/3.22/bioc"),
    min_universe = 1L
  )

  expect_false("affy" %in% bl$package)
  expect_true("AnnotationDbi" %in% bl$package)
})


test_that("build_bioc_blocklist() errors on no BioC repos", {
  qm <- data.frame(pkg = "dplyr", final_decision = "Low",
                   stringsAsFactors = FALSE)
  expect_error(
    build_bioc_blocklist(
      qm,
      opt_repos = list(CRAN = "https://cran.rstudio.com")
    ),
    regexp = "No BioC repos matched"
  )
})


test_that("build_bioc_blocklist() enforces min_universe floor", {
  qm <- data.frame(pkg = "affy", repo_name = "BioC",
                   final_decision = "Low", stringsAsFactors = FALSE)
  ap_tiny <- .mock_ap_matrix(c("affy", "limma"))

  testthat::local_mocked_bindings(
    available.packages = function(...) ap_tiny,
    .package = "utils"
  )
  expect_error(
    build_bioc_blocklist(
      qm,
      opt_repos = list(BioC = "https://bioconductor.org/packages/3.22/bioc"),
      min_universe = 100L
    ),
    regexp = "floor"
  )
})


test_that("build_bioc_blocklist() errors on empty available.packages()", {
  qm <- data.frame(pkg = "affy", repo_name = "BioC",
                   final_decision = "Low", stringsAsFactors = FALSE)
  ap_empty <- matrix(character(0), nrow = 0, ncol = 3,
                     dimnames = list(NULL,
                                     c("Package", "Version", "Repository")))

  testthat::local_mocked_bindings(
    available.packages = function(...) ap_empty,
    .package = "utils"
  )
  expect_error(
    build_bioc_blocklist(
      qm,
      opt_repos = list(BioC = "https://bioconductor.org/packages/3.22/bioc")
    ),
    regexp = "returned 0 rows"
  )
})


test_that("build_bioc_blocklist() warns when blocklist is empty", {
  qm <- data.frame(
    pkg = c("affy", "limma"),
    repo_name = c("BioC", "BioC"),
    final_decision = c("Low", "Low"),
    stringsAsFactors = FALSE
  )
  ap_fake <- .mock_ap_matrix(c("affy", "limma"))

  testthat::local_mocked_bindings(
    available.packages = function(...) ap_fake,
    .package = "utils"
  )
  expect_warning(
    bl <- build_bioc_blocklist(
      qm,
      opt_repos = list(BioC = "https://bioconductor.org/packages/3.22/bioc"),
      min_universe = 1L
    ),
    regexp = "Blocklist is empty"
  )
  expect_equal(nrow(bl), 0L)
})


test_that("build_bioc_blocklist() supports 'package' key col too", {
  # Some qual_assessments-shaped frames use 'package' rather than 'pkg'.
  qm <- data.frame(
    package = c("affy", "AnnotationDbi"),
    repo_name = c("BioC", "BioC"),
    final_decision = c("Low", "High"),
    stringsAsFactors = FALSE
  )
  ap_fake <- .mock_ap_matrix(c("affy", "AnnotationDbi"))

  testthat::local_mocked_bindings(
    available.packages = function(...) ap_fake,
    .package = "utils"
  )
  bl <- build_bioc_blocklist(
    qm,
    opt_repos = list(BioC = "https://bioconductor.org/packages/3.22/bioc"),
    min_universe = 1L
  )
  expect_setequal(bl$package, "AnnotationDbi")
})
