
# This dev script will eventually become val_pipeline()

# options("repos")
devtools::load_all()

# Since running this script is such a computationally intensive process, the
# start of this script would actually begin by filtering packages
# based on pkg downloads, and then we'd feed that list to pkg_names...
# Eventually, this 'dev' script will become a new function called val_pipeline()


# filter packages > 20k here (on CRAN alone)
# Eventually using PACKAGES file
# For now, will use pkg_cran_remote to gain a
# High level summary of pkgs
# --> riskscore PR/ workbench job in progress

# store R Version
r_ver = getRversion()

# Grab val date, output messaging
val_start <- Sys.time()
val_start_txt <- format(val_start, '%Y-%m-%d %H:%M:%S', tz = 'US/Eastern', usetz = TRUE)
# val_date <-as.Date("2025-10-03")
val_date <- as.Date(val_start)
val_date_txt <- gsub("-", "", val_date)
cat(paste0("\n\n\nValidation pipeline initiated: R v", r_ver, " @ ", val_start_txt,"\n\n"))

# 
# ---- Pull config variables ----
#
# For now, let's just filter using cranlogs to determine downloaded pkgs
# opt_repos = c(val_build_repo = "https://cran.r-project.org") # put in config
opt_repos <- pull_config(val = "opt_repos", rule_type = "default") |> unlist()
opt_repos_rr <- pull_config(val = "opt_repos_remote_reduce", rule_type = "default") |> unlist()
decisions <- pull_config(val = "decisions_lst", rule_type = "default")


#
# ---- Set repos option to risk scores date ----
#
old <- options()
on.exit(function() options(old))
options(repos = opt_repos_rr, pkgType = "source", scipen = 999) # , rlang_interactive = FALSE
# options("repos") # verify

#
# ---- val_categorize() ----
#
pre_filtered_pkg_metrics <- 
  val_categorize(
    source = "riskscore",
    avail_pkgs = available.packages() |> as.data.frame(),
    decisions = decisions,
    else_cat = decisions[length(decisions)],
    decisions_df = build_decisions_df(rule_type = "remote_reduce")
    )
# see <-
#   pre_filtered_pkg_metrics |>
#     dplyr::filter(dwnlds > 1000000) 


#
# ---- Set repos option to today's date ----
#
options(repos = opt_repos, pkgType = "source", scipen = 999) # , rlang_interactive = FALSE
# options("repos") # verify


#
# ---- Reduce pkgs ----
#
# Note: has to be decisions[1] ("Low") only because of the way we allowed
# 'High' risk pkgs to get promoted to "Medium" in `pre_filtered_pkg_metrics`.
# Specifically, a 'High' Risk pkg could have a severly low annual downloads #.

# Currently, not being used
failed_pkgs <-
  pre_filtered_pkg_metrics |>
  dplyr::filter(!final_risk %in% decisions[1])
# dplyr::filter(rev_deps > 100) 
# dplyr::filter(dwnlds > 120000) 

passed_pkgs <-
  pre_filtered_pkg_metrics |>
  dplyr::filter(final_risk %in% decisions[1])

  
build_pkgs <- passed_pkgs |>
  dplyr::pull(package)


cat("\n--> Final Decision Category Counts for'pre' assessment risk: \n----> Returned", prettyNum(length(build_pkgs), big.mark = ","), "pkgs for build.\n")




#
# ---- val_build() ----
#

# See the full dependency tree before running val_build()
# these_pkgs <- "withr"
# these_pkgs <- "matrix"
# these_pkgs <- "askpass"
these_pkgs <- build_pkgs
tree <- tools::package_dependencies(
  packages = these_pkgs,
  db = available.packages(),
  # which = c("Suggests"),
  which = "strong", #c("Depends", "Imports", "LinkingTo"),
  # which = c("Depends", "Imports", "LinkingTo", "Suggests"), # prod
  recursive = TRUE
  # recursive = FALSE
) |>
  unlist(use.names = FALSE) |>
  unique()
# How many? # 621 pkgs -->  When recursive: 2,570. Only 744 when you don't include Suggests
full_tree <- c(these_pkgs, tree) |> unique()
full_tree |> length()

# temporary until we can figure out what's gone haywire with this pkg
# build_pkgs <- build_pkgs[build_pkgs != "withr"]

# Validation build
outtie <- val_build(
  
  # pkg_names = 'rlang',
  # pkg_names = 'askpass', # 2.5 - 3 mins when deps, 2 pkgs, no prompts
  # pkg_names = 'withr', 
  pkg_names = build_pkgs,
  
  ref = "source", # default
  # ref = "remote",
  
  metric_pkg = "riskmetric", # default
  
  # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  # deps = c("depends", "suggests"), 
  deps = "depends",  # default
  # deps = NULL,
  
  # deps_recursive = FALSE,
  deps_recursive = TRUE, # default
  
  val_date = val_date, # Sys.Date() # is  default
  
  replace = FALSE, # default
  # replace = TRUE,
  
  out = 'dev/riskassessments',
  
  opt_repos = opt_repos
)


#
# ---- TODO: ----
#
# Add a logger like logRx
#
# Attempt to use a:
# - GitHub pkg
# - Bionconductor Repo / pkg... waiting on {riskscore}... almost there
#
# There is a problem installing {withr}
#

#
# {riskscore}
#
# - Build Bioc output for the first time
# - Set this up on a schedule on Posit Connect
#

#
# build_decisions_df()
#


#
# val_decision()
#
# Need to work out secondary logic for github packages
#
# Should I also consider an auto_fail threshold?
# 

#
# val_pkg():
#
# Old:
# Figure out how to deal with pkg_assess() prompt
# Doesn't happen during background job
# --> opened issue on 'riskmetric' repo
# --> basically un-avoidable - would have to mimic pak::pkg_install()
#
# Run with source = "pkg_cran_remote" first to see downloads & if we need to 
# even gather covr_coverage. Note: we'll still want to run "pkg_source", but
# just remove the covr_coverage component if downloads are high.
#

# 
# val_build():
#


#
# {riskreports}:
#
# Install latest (dev) version of quarto?
#



#
# ---- Inspect outputs ----
#
outtie$val_dir
qual <- outtie$pkgs_df
# nrow(qual)
# saveRDS(qual, file.path(outtie$val_dir, paste0("qual_evidence_", val_date_txt, ".rds")))

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
  dplyr::filter(final_decision == decisions[1])

# Store as pins board?
# How to Provision PPM metadata for all pkgs





# TODO some day, but not right now:

#
# Be able to export / store work done here into the {riskassessment} app
#



