
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
  deps = NULL,
  out = 'dev/riskassessments'
)

# Inspect the assessment
val_dir <- file.path("dev/riskassessments",glue::glue('R_{getRversion()}'),gsub("-", "", Sys.Date()))
assessed <- file.path(val_dir, "assessed")
ass <- readRDS(file.path(assessed, "zoo_1.8-14_assessments.rds")) 
ass$covr_coverage$totalcoverage

# if ran after, will build only lattice because there is one dep
l <- val_build('zoo', out = 'dev/riskassessments', deps = "depends")

# val_build(pkg_names = c('aamatch'), deps = NULL)

