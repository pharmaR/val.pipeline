
options("repos")
devtools::load_all()

# Since running this script is such a computationally intensive process, the
# start of this script would actually begin by filtering packages
# based on pkg downloads, and then we'd feed that list to pkg_names...
# Eventually, this 'dev' script will become a new function called val_pipeline()



# will just build 'zoo'
z <- val_build(
  pkg_names = 'zoo',
  ref = "source", 
  metric_pkg = "risk.assessr",
  deps = NULL, # deps = c("depends"), deps = NULL
  # val_date = Sys.Date(), # Default
  out = 'dev/riskassessments'
)

# if ran after, will build only lattice because there is one dep
# be warned, this takes forever with ref == 'source'
l <- val_build('zoo', out = 'dev/riskassessments', deps = "depends")


# Inspect the assessment dir
# valdate <- gsub("-", "", Sys.Date())
valdate <- "20250731"
val_dir <- file.path(
  "dev/riskassessments",
  glue::glue('R_{getRversion()}'),
  valdate
  )
assessed <- file.path(val_dir, "assessed")
meta_files <- list.files(assessed, pattern = "_meta.rds$")
ass_files <- list.files(assessed, pattern = "_assessments.rds$")

# choose a pkg
pkg_name <- "zoo"
meta_pkg <- meta_files[stringr::str_detect(meta_files, pkg_name)]
meta <- readRDS(file.path(assessed, meta_pkg)) 
ass_pkg <- ass_files[stringr::str_detect(ass_files, pkg_name)]
ass <- readRDS(file.path(assessed, ass_pkg)) 

# explore outputs
meta
names(ass)
ass$covr_coverage$totalcoverage
ass$downloads_1yr |> prettyNum(big.mark = ",")

# val_build(pkg_names = c('aamatch'), deps = NULL) # No coverage

#
# Test out two pkgs, so we can make a useful output to return from val_build
#
outtie <- val_build(
  pkg_names = 'zoo',
  ref = "remote", 
  metric_pkg = "riskmetric",
  deps = c("depends"),
  val_date = Sys.Date(), # Default
  rerun = FALSE,
  out = 'dev/riskassessments'
)
outtie$pkgs_df
