
# This dev script will eventually become val_pipeline()

# options("repos")
devtools::load_all()

# Since running this script is such a computationally intensive process, the
# start of this script would actually begin by filtering packages
# based on pkg downloads, and then we'd feed that list to pkg_names...
# Eventually, this 'dev' script will become a new function called val_pipeline()


# filter packages > 20k here
# Eventually using PACKAGES file
# For now, will use pkg_cran_remote to gain a
# High level summary of pkgs
# --> riskscore PR/ workbench job in progress

# store R Version
r_ver = getRversion()

# Grab val date, output messaging
val_start <- Sys.time()
val_start_txt <- format(val_start, '%Y-%m-%d %H:%M:%S', tz = 'US/Eastern', usetz = TRUE)
val_date <- as.Date(val_start)
val_date_txt <- gsub("-", "", val_date)
cat(paste0("\n\n\nValidation pipeline initiated: R v", r_ver, " @ ", val_start_txt,"\n\n"))

# 
# ---- Inputs
#
# For now, let's just filter using cranlogs to determine downloaded pkgs
opt_repos = c(val_build_repo = "https://cran.r-project.org")
decisions = c("Low", "Medium", "High")

#
# ---- Set repos option ----
#
old <- options()
on.exit(function() options(old))
options(repos = opt_repos, pkgType = "source") # , rlang_interactive = FALSE


#
# ---- val.filter ----
#
pre_filtered_pkg_metrics <- 
  val_filter(
    pre = TRUE,
    source = "riskscore",
    avail_pkgs = available.packages() |> as.data.frame(),
    decisions = decisions,
    else_cat = "High",
    decisions_df = build_decisions_df()
    )
# Final pkg counts in each risk category
# build_pkgs_len <-
#   pkgs_final |>
#   dplyr::filter(final_risk %in% decisions[1]) |>
#   dplyr::pull(package) |>
#   length()
# 

build_pkgs <-
  pre_filtered_pkg_metrics |>
  dplyr::filter(final_risk %in% decisions[1]) |>
  dplyr::pull(package)

failed_pkgs <-
  pre_filtered_pkg_metrics |>
  dplyr::filter(!final_risk %in% decisions[1])


cat("\n--> Final Decision Category Counts for'pre' assessment risk: \n----> Returned", prettyNum(length(build_pkgs), big.mark = ","), "pkgs for build.\n")




#
# ---- val_build() ----
#

# See the full dependency tree before running val_build()
tree <- tools::package_dependencies(
  packages = "askpass",
  # packages = build_pkgs,
  db = available.packages(),
  # which = c("Suggests"),
  # which = c("Depends", "Imports", "LinkingTo"),
  which = c("Depends", "Imports", "LinkingTo", "Suggests"), # prod
  # recursive = TRUE
  recursive = FALSE
) |>
  unlist(use.names = FALSE) |>
  unique()
length(tree)

# Validation build
outtie <- val_build(
  
  # pkg_names = 'zoo',   # has a prompt, plus depends on lattice which takes a while
  pkg_names = 'askpass', # 2.5 - 3 mins when deps
  # pkg_names = build_pkgs,
  
  ref = "source", # default
  # ref = "remote",
  
  metric_pkg = "riskmetric", # default
  
  deps = c("depends", "suggests"), # default
  # deps = "depends",  # this means --> c("Depends", "Imports", "LinkingTo")
  # deps = NULL,
  
  deps_recursive = FALSE,
  # deps_recursive = TRUE, # default
  
  val_date = val_date, # Sys.Date() # is  default
  
  replace = FALSE,# default
  # replace = TRUE, 
  
  out = 'dev/riskassessments'
)


#
# ---- TODO: ----
#
# Add a logger like logRx
#

#
# val_pkg():
#
# Figure out how to deal with pkg_assess() prompt
# Doesn't happen during background job
# --> opened issue on 'riskmetric' repo
# --> basically un-avoidable - would have to mimic pak::pkg_install()
#
# Also need a decision_reason field to cite which metric failed first. Leaving
# this here until the "fail first" logic is applied.
#
# So, val_pkg() stores a list (because that works better for stripping .recording
# attributes), but val_filter() is designed to work with a {riskscore} df. We'll
# need a way to easily convert the list to a df, or make val_filter() handle lists.
#


# 
# val_filter():
#



#
# {riskreports}:
# Install latest (dev) version of quarto?
#


#
# ---- Inspect outputs ----
#
outtie$val_dir
assessed <- outtie$pkgs_df

# # Inspect the assessment dir
# # valdate <- gsub("-", "", Sys.Date())
# valdate <- "20250731"
# val_dir <- file.path(
#   "dev/riskassessments",
#   glue::glue('R_{getRversion()}'),
#   valdate
#   )
# assessed <- file.path(val_dir, "assessed")
# meta_files <- list.files(assessed, pattern = "_meta.rds$")
# ass_files <- list.files(assessed, pattern = "_assessments.rds$")
# 
# # choose a pkg
# pkg_name <- "zoo"
# meta_pkg <- meta_files[stringr::str_detect(meta_files, pkg_name)]
# meta <- readRDS(file.path(assessed, meta_pkg)) 
# ass_pkg <- ass_files[stringr::str_detect(ass_files, pkg_name)]
# ass <- readRDS(file.path(assessed, ass_pkg)) 
# 
# # explore outputs
# meta
# names(ass)
# ass$covr_coverage$totalcoverage
# ass$downloads_1yr |> prettyNum(big.mark = ",")
# 
# # val_build(pkg_names = c('aamatch'), deps = NULL) # No coverage

#
# ---- Wrap up ----
#
# determine qualified pkgs to provision for PPM
qualified <- assessed |>
  dplyr::filter(decision == "Low")

# Store as pins board?
# How to Provision PPM metadata for all pkgs








