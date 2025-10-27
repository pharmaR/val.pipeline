


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
  ,"Final Risk Summary"
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
    ,"Final Risk Summary"
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
    "Final Risk Summary"
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
    ,"Final Risk Summary"
  )
  
  expect_true(result$final_risk == "High")
  expect_false("downloads_1yr" %in% names(result))
})
