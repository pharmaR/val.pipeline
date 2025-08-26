
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
val_date <- as.Date(val_date)
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

# First whack at attempting to filter packages based on org-level criterion used
# to set thresholds (and later update final decision (if not already 'high
# risk')) AND filter packages before running. Note: If PACKAGES file had
# assessments, we'd be using that (paired with val.filter), but instead,
# we're going to use riskscore::cran_assessed_20250812

avail_pkgs <- available.packages() |> as.data.frame()


# Use "pkg_cran_remote" data from riskscore::cran_assessed_20250812
# remotes::install_github("pharmar/riskscore", force = TRUE,
#                         ref = "ac-cran-20250811")
pv <- packageVersion("riskscore") # verify ‘v0.0.1'
cat(paste0("{riskscore} Version: 'v", pv, "'\n"))

pkgs <- riskscore::cran_assessed_20250812
object.size(cran_assessed) / 1000000000


# Note! This area is to automatically exclude pkgs
# that are considered high risk so we won't have to waste compute
# assessing these pkgs. That said, we'll be filtering based
# on our definition of 'high risk' and 'high risk' only, based on
# the cran_remote ref source. 
#
# That means this area will not be used for setting decisions, because
# those decisions can only be made on the 'source' ref assessments. If
# a pkg is 'high risk', we should automatically create the metadata list
# necessary.
#
# In the future, we'd like to use {val.filter} here to get our subset.
# We'd also like to be able to extract the filtering logic as needed


#
# ---- Annual Downloads ----
#
# extract list values into a numeric vector
# cran_assessed$downloads_1yr[[7]][[1]]
pkgs$dwnlds <- pkgs$downloads_1yr |>
  purrr::map(~.x[[1]]) |>
  unlist() |>
  as.numeric() 

# Overall summary
hist(pkgs$dwnlds, breaks = 5)
summary(pkgs$dwnlds)
summary(as.factor(pkgs$dwnlds))
round(prop.table(summary(as.factor(pkgs$dwnlds))), 3) * 100


# Workshop a threshold
pkgs$dwnlds_cat <- dplyr::case_when(
  pkgs$dwnlds > 70000 ~ "Low",
  dplyr::between(pkgs$dwnlds, 12000, 70000) ~ "Medium",
  .default = "High"
)
table(pkgs$dwnlds_cat)
round(prop.table(table(pkgs$dwnlds_cat)), 3) * 100


# exceptions to this? Perhaps some pharmaverse pkgs we trust but have low downloads?


#
# ---- Reverse Dependencies ----
#
# extract list values into a numeric vector
# cran_assessed$reverse_dependencies[[7]]
pkgs$rev_deps <- pkgs$reverse_dependencies |>
  purrr::map(~length(.x[[1]])) |>
  unlist() |>
  as.numeric()

# Overall summary
hist(pkgs$rev_deps, breaks = 5)
summary(pkgs$rev_deps)
summary(as.factor(pkgs$rev_deps))
round(prop.table(summary(as.factor(pkgs$rev_deps))), 3) * 100

# Workshop a threshold
pkgs$rev_deps_cat <- dplyr::case_when(
  pkgs$rev_deps > 7 ~ "Low",
  dplyr::between(pkgs$rev_deps, 2, 7) ~ "Medium",
  .default = "High"
)
table(pkgs$rev_deps_cat)
round(prop.table(table(pkgs$rev_deps_cat)), 3) * 100




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

  
  

# TODO:
# Figure out what pkg_assess() prompt is for.
# Doesn't happen during background job
# --> opened issue on 'riskmetric' repo

# riskreports
# Install latest (dev) version of quarto?

# Update final decisions

#
# Test out two pkgs, so we can make a useful output to return from val_build
#
outtie <- val_build(
  pkg_names = 'zoo',
  # ref = "remote",
  ref = "source",
  metric_pkg = "riskmetric",
  deps = NULL,
  # deps = "depends",
  # deps = c("depends", "suggests"),
  deps_recursive = FALSE,
  # deps_recursive = TRUE,
  val_date = Sys.Date(),
  replace = TRUE,
  out = 'dev/riskassessments'
)
# outtie$pkgs_df





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






