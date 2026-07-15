


test_that("val_decision() requires pkg parameter", {
  expect_error(
    val_decision(pkg = NULL),
    "Must provide a package name"
  )
})

test_that("val_decision() requires valid source", {
  expect_error(
    val_decision(pkg = "test", source_df = "invalid"),
    "Invalid source specified.*data.frame"
  )
})

test_that("val_decision() requires source with assessment and scores", {
  expect_error(
    val_decision(pkg = "test", source_df = list(wrong = "data")),
    "Invalid source.*assessment.*scores"
  )
})

test_that("val_decision() validates decisions_df structure", {
  invalid_df <- data.frame(wrong = "columns")
  
  expect_error(
    val_decision(
      pkg = "test",
      source_df = list(assessment = list(), scores = list()),
      decisions_df = invalid_df
    ),
    "decisions_df.*not compliant"
  )
})

test_that("val_decision() processes package with minimal data", {
  mock_source <- 
    data.frame(
      package = "testpkg",
      reverse_dependencies = 5,
      downloads_1yr = 1000,
      news_current = 0.8
    )
  
  mock_avail <- data.frame(
    Package = "testpkg",
    Version = "1.0.0",
    Repository = "https://cran.r-project.org/src/contrib",
    stringsAsFactors = FALSE
  )
  
  old_repos <- getOption("repos")
  options(repos = c(CRAN = "https://cran.r-project.org"))
  on.exit(options(repos = old_repos))
  
  expect_output(
    result <- val_decision(
      pkg = "testpkg",
      source_df = mock_source,
      avail_pkgs = mock_avail
    )
    ,"Risk Decision"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("package" %in% names(result))
  # new *_cat and *_cataa vars are created for certain metrics in config
  expect_true(all(c("final_risk", "downloads_1yr_cat", "downloads_1yr_cataa") %in% names(result)))
  expect_equal(result$package, "testpkg")


  
  # Try a really high download count, to see if final_risk and "_cataa" vars change
  mock_source <- 
    data.frame(
      package = "testpkg",
      reverse_dependencies = 5,
      downloads_1yr = 8000000,
      news_current = 0.8
    )
  expect_output(
    result <- val_decision(
        pkg = "testpkg",
        source_df = mock_source,
        avail_pkgs = mock_avail
      )
    ,"Risk Decision"
  )
    
  expect_true(result$downloads_1yr_cataa == TRUE) 
  expect_true(result$final_risk == "Low") 
  
})

test_that("val_decision() handles NA values in assessment", {
  mock_source <- 
    data.frame(
      package = "testpkg",
      downloads_1yr = NA
    )
  
  mock_avail <- data.frame(
    Package = "testpkg",
    Version = "1.0.0",
    Repository = "https://cran.r-project.org/src/contrib",
    stringsAsFactors = FALSE
  )
  
  old_repos <- getOption("repos")
  options(repos = c(CRAN = "https://cran.r-project.org"))
  on.exit(options(repos = old_repos))
  
  expect_output(
    result <- val_decision(
      pkg = "testpkg",
      source_df = mock_source,
      avail_pkgs = mock_avail
    )
  ,
    "Risk Decision"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(result$final_risk == "High") # else cat when NA
})


test_that("val_decision() excludes specified metrics", {
  mock_source <- 
    data.frame(
      package = "testpkg",
      downloads_1yr = 1000,
      reverse_dependencies = 1
    )
  
  mock_avail <- data.frame(
    Package = "testpkg",
    Version = "1.0.0",
    Repository = "https://cran.r-project.org/src/contrib",
    stringsAsFactors = FALSE
  )
  
  old_repos <- getOption("repos")
  options(repos = c(CRAN = "https://cran.r-project.org"))
  on.exit(options(repos = old_repos))
  
  expect_output(
      result <- val_decision(
        pkg = "testpkg",
        source_df = mock_source,
        avail_pkgs = mock_avail,
        excl_metrics = "downloads_1yr"
      )
    ,"Risk Decision"
  )
  
  expect_true(result$final_risk == "High")
  expect_false("downloads_1yr" %in% names(result))
})


test_that("val_decision() preserves primary _cat cols when secondary runs", {
  # Non-CRAN pkg + non-Low primary => secondary phase runs. Before this change
  # primary `<metric>_cat` cols were dropped before the secondary
  # rip_cats_by_pkg call, which meant downstream callers couldn't see which
  # primary metrics drove a non-Low outcome (issue #37).
  mock_source <- data.frame(
    package = "testpkg",
    downloads_1yr = 150000,
    covr_coverage = 50,      # Medium primary
    reverse_dependencies = 5,
    dependencies = 40,       # High secondary
    news_current = 0.8,
    r_cmd_check_errors = 0,
    r_cmd_check_warnings = 0,
    has_examples = 50,
    export_help = 50,
    has_vignettes = 2,
    has_source_control = 1,
    has_website = 1
  )
  mock_avail <- data.frame(
    Package = "testpkg",
    Version = "1.0.0",
    Repository = "https://bioconductor.org/packages/3.22/bioc/src/contrib",
    stringsAsFactors = FALSE
  )
  old_repos <- getOption("repos")
  options(repos = c(BioC = "https://bioconductor.org/packages/3.22/bioc"))
  on.exit(options(repos = old_repos))

  expect_output(
    result <- val_decision(
      pkg = "testpkg",
      source_df = mock_source,
      avail_pkgs = mock_avail
    ),
    "Risk Decision"
  )

  # Primary metric _cat col (covr_coverage) survived the secondary phase
  expect_true("covr_coverage_cat" %in% names(result))
  # Secondary metric _cat col also present
  expect_true("dependencies_cat" %in% names(result))
  # And drivers can be extracted end-to-end
  drv <- extract_risk_drivers(result)
  expect_true(!is.na(drv))
  # dependencies is High and final_risk is High, so it must be flagged
  expect_true(grepl("dependencies", drv))
})
