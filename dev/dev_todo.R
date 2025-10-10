
#
# ---- TODO: ----
#

#
# General tasks:
#
# Add a logger like logRx
#
# Attempt to use a:
# - GitHub pkg... waiting on PPM to be stood up, but can start to build scaffolding for package source based on options('repos') (or pkg_ref())
# - Bionconductor Repo / pkg...
#   - Bioc pkgs need to be on a different scale for risk assessments. Or we need to switch
#     our CRAN scale to be based on percentiles instead of fixed values.
#   - Bioc needs to be run w/ CRAN pkgs because the Bioc can depend on CRAN pkgs
#   - final_risk = final_risk_id |> factor(labels = decisions), # TODO: doesn't work if final_risk_id is only a few levels
#
#
# - Work out concept of having some packages previously approved via manual
#   unit test intervention: add 'approved_pkgs' in default config
#
# - Technically, val_date & opt_repos PPM date are not sync'd up yet. Same with
#   opt_repos_remote_reduce. Both should probably default to Sys.date() in config,
#   but be changed in the val_pipeline() & val_categorize() call if val_date & 
#   riskscore date differ
# 
# - opt_repos_remote_reduce should just be moved under the 'remote_reduce' config


#
# {riskscore}
#
# - Build Bioc output for the first time
# - Set this up on a schedule on Posit Connect
#    - updating the github pkg?
#    - updating a Pins Board?
#



#
# build_decisions_df()
#


#
# val_decision()
#
# - Need to work out 'secondary' logic for github/ internal packages
#
# - Should I also consider an auto_fail threshold?
# 
# - final_risk = final_risk_id |> factor(labels = decisions), # TODO: doesn't work if final_risk_id is only a few levels
#

#
# val_pkg():
#
# Old: Figure out how to deal with pkg_assess() prompt Doesn't happen during
# background job
# --> opened issue on 'riskmetric' repo
# --> basically un-avoidable - would have to mimic pak::pkg_install()
# For now, just run as background or workbench job
#
#
# - Update clean_install based on assessment. Or maybe not since I'll likely be
#   getting rid of the clean_install measure.
# 

# 
# val_build():
#


#
# val_pipeline():
#
# - Need to be able to specify non-cran sources... like GitHub pkgs. Preferably
#   by adding another repos URL pointing to package manager.
#
# - What is this going to return? a CSV of the val_build() df?
#


#
# {riskreports}:
#
# - Install latest (dev) version of quarto? Warning being thrown.
# - Clean up the reports

#
# Tasks for some day, but not right now:
#
# - Be able to export / store work done here into the {riskassessment} app
#



# 
# Package Compilation notes:
#
# w_AA:         1:46;   57 pkgs --> 1:46; 57 pkgs
# codetools-on: 0:51;   25 pkgs --> 2:37; 82 pkgs
# after_bit:    14:40; 433 pkgs --> 17:17; 640 / 827 (77.4%); avg 1.6 mpp
# remove_VGAM  
# remove_shinyBS
# 
# All 828 packages processed; 345 of which were avoided due to a dependency failing it's risk assessment.



#
# ---- Dev ----
#
val_date <- "2025-10-07"
val_dir <- file.path('dev/riskassessments', paste0("R_", getRversion()), gsub("-","",val_date))
reports <- list.files(file.path(val_dir, "reports"))
reports |> length()
# any(stringr::str_detect(reports, "bit"))



source("dev/pkg_lists.R")
# which(pkgs == "codetools")
# which(pkgs == "bit")

# chk_pkg <- "VGAM"
chk_pkg <- "xlsx"
which(pkgs == chk_pkg)
assessed <- list.files(file.path(val_dir, "assessed"))
any(stringr::str_detect(assessed, chk_pkg)) # not there

# What's next?
pack <- pkgs[which(pkgs == chk_pkg) + 1]
pack


#
# ---- val_pipeline() ----
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
# ---- val_build()----
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


#
# Validation build
#
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
# ---- val_pkg() ----
#

remote_pkgs <- pull_config(val = "remote_only", rule_type = "default")
avail_pkgs <- available.packages() |> as.data.frame()
val_date <- Sys.Date()
# val_date <- as.Date("2025-10-07")
val_dir <- file.path('dev/riskassessments', paste0("R_", getRversion()), gsub("-","",val_date))
reports <- list.files(file.path(val_dir, "reports"))
reports |> length()
any(stringr::str_detect(reports, "spacesXYZ"))


source("dev/pkg_lists.R")


# pack = 'rlang'
# pack = 'askpass' # 2.5 - 3 mins when deps, 2 pkgs, no prompts
# pack = 'withr'
# pack = 'SuppDists'

pack <- pkgs[which(pkgs == "SuppDists") + 1] # last left off:
pack

pkg_meta <- val_pkg(
  pkg = pack,
  ver = avail_pkgs$Version[avail_pkgs$Package == pack],
  avail_pkgs = avail_pkgs,
  ref = if(pkg %in% remote_pkgs) 'remote' else 'source',
  metric_pkg = "riskmetric", 
  out_dir = val_dir,
  val_date = val_date
  )
  