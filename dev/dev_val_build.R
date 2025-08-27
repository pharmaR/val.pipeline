
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

# For now, let's just filter using cranlogs to determine downloaded pkgs
opt_repos = c(val_build_repo = "https://cran.r-project.org")

#
# ---- Set repos option ----
#
old <- options()
on.exit(function() options(old))
options(repos = opt_repos, pkgType = "source") # , rlang_interactive = FALSE


#
# ---- val.filter ----
#
build_pkgs <- val_filter()



# TODO:
# val_pkg():
# Figure out what pkg_assess() prompt is for.
# Doesn't happen during background job
# --> opened issue on 'riskmetric' repo
# --> basically un-avoidable - would have to mimic pak

# riskreports
# Install latest (dev) version of quarto?

#
# ---- val_build() ----
#
# dput(dep_1)
# tools::package_dependencies(
#   packages = "askpass", 
#   db = available.packages(),
#   which = c("Suggests"),
#   # which = c("Depends", "Imports", "LinkingTo"),
#   recursive = FALSE
# ) |>
#   unlist(use.names = FALSE)

outtie <- val_build(
  # pkg_names = 'zoo',   
  pkg_names = 'askpass', # 2.5 - 3 mins
  # pkg_names = build_pkgs,
  
  # ref = "remote",
  ref = "source",
  metric_pkg = "riskmetric",
  
  # deps = NULL,
  deps = "depends",
  # deps = c("depends", "suggests"),
  
  deps_recursive = FALSE,
  # deps_recursive = TRUE,
  
  val_date = Sys.Date(),
  # replace = TRUE,
  replace = FALSE,
  out = 'dev/riskassessments'
)
outtie$val_dir
pdf <- outtie$pkgs_df





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


# ---- val.criterion ----
# Use org-level criterion to set thresholds and
# Update final decision (if not already 'high risk')
# AND then filter packages to a final list
# Note: If pkgs file had assessments, we'd be doing this BEFORE val_build() only


# Reverse Dependencies
pkgs$reverse_dependencies |> as.numeric() |> hist(breaks = 20)
# vhist(pkgs, field = "downloads_1yr") # freezes up...
# Workshop a threshold
( pkgs |>
    filter(reverse_dependencies > 0.05) |>
    nrow()
) / tot


# ---- Code Coverage ----
# low risk:    > 70%, else
# medium risk: > 50%, else
# high risk



# ---- Dependencies ----
# Too many dependencies is problematic ... ?



# ---- Bug status ----
# A terrible bug status launches the pkg into 'medium' risk pool?



# ---- Has Source Control ----
# If no, launches the pkg into 'medium' risk pool?






