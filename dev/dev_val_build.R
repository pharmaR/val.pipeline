
options("repos")
devtools::load_all()

# Reminder of the default options
# val_build(
#   pkg_names = NULL, 
#   deps = c("depends", "suggests"), 
#   out = 'riskassessment',
#   opt_repos = c(val_build_repo = "https://cran.r-project.org")
# )

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



# val_build(pkg_names = c('aamatch'), deps = NULL)

