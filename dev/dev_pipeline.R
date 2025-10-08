
#
# ---- TODO: ----
#
# Add a logger like logRx
#
# Attempt to use a:
# - GitHub pkg
# - Bionconductor Repo / pkg... waiting on {riskscore}... almost there
#
#
# There is a problem installing {withr}. Test telling riskmetric to only use the
# `pkg_cran_remote` so that it doesn't get installed.
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
# Old: Figure out how to deal with pkg_assess() prompt Doesn't happen during
# background job
# --> opened issue on 'riskmetric' repo
# --> basically un-avoidable - would have to mimic pak::pkg_install()
#
#
# Update clean_install based on assessment. Or maybe not since I'll likely be
# getting rid of the clean_install measure.

# 
# val_build():
#

#
# val_pipeline():
#
# Need to be able to specify non-cran sources... like GitHub pkgs. Preferably
# by adding another repos URL pointing to package manager.
#
# What is this going to return? a CSV of the val_build() df?
#


#
# {riskreports}:
#
# Install latest (dev) version of quarto?
#


# TODO some day, but not right now:

#
# Be able to export / store work done here into the {riskassessment} app
#


#
# val_pipeline()
#
qual <- val_pipeline(
    ref = "source",
    metric_pkg = "riskmetric", 
    deps = "depends", # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
    deps_recursive = TRUE,
    val_date = Sys.Date(),
    # val_date = as.Date("2025-10-07"),
    replace = FALSE, 
    out = 'dev/riskassessments'
)
  

#
# val_build()
#
# See the full dependency tree before running val_build()
# these_pkgs <- "withr"  # messes with the entire process
# these_pkgs <- "matrix" # takes 5 mins to install
# these_pkgs <- "askpass"
# these_pkgs <- "codetools"
# these_pkgs <- build_pkgs
# tree <- tools::package_dependencies(
#   packages = these_pkgs,
#   db = available.packages(),
#   # which = c("Suggests"),
#   which = "strong", #c("Depends", "Imports", "LinkingTo"),
#   # which = c("Depends", "Imports", "LinkingTo", "Suggests"), # prod
#   recursive = TRUE
#   # recursive = FALSE
# ) |>
#   unlist(use.names = FALSE) |>
#   unique()
# # How many? # 621 pkgs -->  When recursive: 2,570. Only 744 when you don't include Suggests
# full_tree <- c(these_pkgs, tree) |> unique()
# full_tree |> length()

# temporary until we can figure out what's gone haywire with this pkg
# build_pkgs <- build_pkgs[build_pkgs != "withr"]


outtie <- val_build(
  # pkg_names = 'rlang',
  pkg_names = 'askpass', # 2.5 - 3 mins when deps, 2 pkgs, no prompts
  # pkg_names = 'withr',
  # pkg_names = 'codetools',
  # pkg_names = build_pkgs, # Not sorted
  
  # everything else
  ref = "source",
  metric_pkg = "riskmetric", 
  deps = "depends", # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  deps_recursive = TRUE,
  val_date = Sys.Date(),
  # val_date = as.Date("2025-10-07"),
  replace = FALSE, 
  out = 'dev/riskassessments'
) 

#
# val_pkg()
#
remote_pkgs <- pull_config(val = "remote_only", rule_type = "default")
avail_pkgs <- available.packages() |> as.data.frame()
val_date = Sys.Date()
val_dir <- file.path('dev/riskassessments', paste0("R_", getRversion()), gsub("-","",val_date))
# Select one package
pack = 'rlang'
# pack = 'askpass' # 2.5 - 3 mins when deps, 2 pkgs, no prompts
# pack = 'withr'
# pack = 'codetools'
pkg_meta <- val_pkg(
  pkg = pack,
  ver = avail_pkgs$Version[avail_pkgs$Package == pack],
  avail_pkgs = avail_pkgs,
  ref = if(pkg %in% remote_pkgs) 'remote' else ref,
  metric_pkg = "riskmetric", 
  out_dir = val_dir,
  val_date = val_date
  )
  