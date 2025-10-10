
#
# ---- TODO: ----
#


#
# General tasks:
#
#
# Attempt to use a:
# - Bionconductor Repo / pkg...
#   - Bioc pkgs need to be on a different scale for risk assessments. Or we need to switch
#     our CRAN scale to be based on percentiles instead of fixed values.
#   - Bioc needs to be run w/ CRAN pkgs because the Bioc can depend on CRAN pkgs
# val_decision()
#   - final_risk = final_risk_id |> factor(labels = decisions), # TODO: doesn't work if final_risk_id is only a few levels
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
  