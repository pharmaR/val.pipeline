
options("repos")
devtools::load_all()

# Reminder of the default options
# val_build(
#   r_ver = getRversion(),
#   pkg_names = NULL, 
#   deps = c("depends", "suggests"), 
#   out = 'riskassessment',
#   opt_repos = c(val_build_repo = "https://cran.r-project.org")
# )

# will just build 'zoo'
val_build(
  pkg_names = 'zoo',
  deps = NULL,
  out = 'dev/riskassessments'
)

# if ran after, will build only lattice
val_build('zoo', out = 'dev/riskassessments')

# val_build(pkg_names = c('aamatch'), deps = NULL)

