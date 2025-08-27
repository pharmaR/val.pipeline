
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
pkgs_scored <- riskscore::cran_scored_20250812
object.size(pkgs) / 1000000000


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
# This will be the #1 decider for filtering packages
# extract list values into a numeric vector
# cran_assessed$downloads_1yr[[7]][[1]]
pkgs$dwnlds <- pkgs$downloads_1yr |>
  purrr::map(~.x[[1]]) |>
  unlist() |>
  as.numeric() 

# Overall summary
# hist(pkgs$dwnlds, breaks = 5)
# summary(pkgs$dwnlds)
# summary(as.factor(pkgs$dwnlds))
# round(prop.table(summary(as.factor(pkgs$dwnlds))), 3) * 100


# Workshop a threshold
pkgs$dwnlds_cat <- dplyr::case_when(
  pkgs$dwnlds > 80000 ~ "Low",
  dplyr::between(pkgs$dwnlds, 40000, 80000) ~ "Medium",
  .default = "High"
)
table(pkgs$dwnlds_cat)
round(prop.table(table(pkgs$dwnlds_cat)), 3) * 100

#
# ---- Exceptions ----
#
# Exceptions to this? Perhaps some 'high' risk pkgs
# could move to 'medium' if they have other outstanding metrics?
# For example, pharmaverse pkgs we trust may have low downloads?


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
# hist(pkgs$rev_deps, breaks = 5)
# summary(pkgs$rev_deps)
# summary(as.factor(pkgs$rev_deps))
# round(prop.table(summary(as.factor(pkgs$rev_deps))), 3) * 100

# Workshop a threshold
pkgs$rev_deps_cat <- dplyr::case_when(
  pkgs$rev_deps > 7 ~ "Low",
  dplyr::between(pkgs$rev_deps, 2, 7) ~ "Medium",
  .default = "High"
)
table(pkgs$rev_deps_cat)
round(prop.table(table(pkgs$rev_deps_cat)), 3) * 100
# str(pkgs$rev_deps_cat )

#
# ---- Dependencies ----
#
# Too many dependencies is problematic ... ?
# pkgs$deps <- pkgs$dependencies[[1]] |>
#   purrr::map(~.x[[1]]) |>
#   unlist() |>
#   as.numeric()

# For some reason, the dependencies metric didn't come thru in `riskscore`,
# but we don't need it since that info is available in `available.packages()`

# It can't depend on a version of R that is more higher than the R version we
# are evaluating right now
# depends_base <- avail_pkgs |>
#   dplyr::select(Package, Version, Depends) |>
#   dplyr::filter(stringr::str_detect(Depends, r_ver)) |>

pkgs <- pkgs |>
  dplyr::mutate(
    deps = 
        tools::package_dependencies(
          packages = package,
          db = available.packages(),
          which = "most", #c("Depends", "Imports", "LinkingTo"),
          recursive = FALSE
        ),
    n_deps = purrr::map_int(deps, ~length(.x))
  )


# Overall summary
# hist(pkgs$n_deps, breaks = 5)
# summary(pkgs$n_deps)
# summary(as.factor(pkgs$n_deps))
# round(prop.table(summary(as.factor(pkgs$n_deps))), 3) * 100

# Workshop a threshold
pkgs$n_deps_cat <- dplyr::case_when(
  pkgs$n_deps < 4 ~ "Low",
  dplyr::between(pkgs$n_deps, 4, 8) ~ "Medium",
  .default = "High"
)
table(pkgs$n_deps_cat)
round(prop.table(table(pkgs$n_deps_cat)), 3) * 100


#
# ---- Bug status ----
#
# # {riskscore} deemed not reliable here
# 
# # extract list values into a numeric vector
# # cran_assessed$reverse_dependencies[[7]]
# pkgs$bug_stat_d <- pkgs$bugs_status |>
#   purrr::map(~length(.x[[1]])) |>
#   unlist()
# 
# 
# pkgs <- pkgs |>
#   # dplyr::rename(version = version.x) |>
#   dplyr::left_join(
#     pkgs_scored |>
#       dplyr::select(package, version, bug_stat = bugs_status),
#     by = c("package", "version")
#   )
# 
# # Overall summary
# # hist(as.numeric(pkgs$bug_stat), breaks = 5)
# # summary(as.numeric(pkgs$bug_stat))
# # summary(as.factor(as.numeric(pkgs$bug_stat)))
# # round(prop.table(summary(as.factor(as.numeric(pkgs$bug_stat)))), 3) * 100
# 
# # Workshop a threshold
# pkgs$bug_stat_cat <- dplyr::case_when(
#   as.numeric(pkgs$bug_stat) > .49999 ~ "Low",
#   dplyr::between(as.numeric(pkgs$bug_stat), .10, .49999) ~ "Medium",
#   .default = "High"
# )
# table(is.na(pkgs$bug_stat)) # this is not reliable
# table(pkgs$bug_stat_cat)
# round(prop.table(table(pkgs$bug_stat_cat)), 3) * 100



#
# ---- news_current ----
#
# extract list values into a numeric vector
# library(dplyr)
# is.logical(pkgs$news_current[[11]][[1]])
# pkgs <- pkgs |>
#   dplyr::mutate(dplyr::across(c(news_current), ~ if("pkg_metric_error" %in% class(.x[[1]])) "pkg_metric_error" else .x)) #|>
#   dplyr::mutate(
#     news_curr =
#       if("pkg_metric_error" %in% class(news_current[[1]])) "pkg_metric_error" else news_current[[1]])
#       unlist() %>%
#       dplyr::if_else(!is.logical(.), logical(0), .)
#   )
# str(pkgs$news_curr)
# failing above

# Try again with scored df
# pkgs$news_curr <- NULL
pkgs <- pkgs |>
  # dplyr::rename(version = version.x) |>
  dplyr::left_join(
    pkgs_scored |>
      mutate(news_curr = as.numeric(news_current)) |>
      dplyr::select(package, version, news_curr),
    by = c("package", "version")
  )

# Overall summary
table(is.na(pkgs$news_curr))
table(pkgs$news_curr) # this is not reliable
round(prop.table(table(pkgs$news_curr)), 3) * 100

# Workshop a threshold
pkgs$news_curr_cat <- dplyr::case_when(
  pkgs$news_curr == 1 ~ "Low",
  .default = "High"
)
str(pkgs$news_curr_cat)
table(pkgs$news_curr_cat)
round(prop.table(table(pkgs$news_curr_cat)), 3) * 100



#
# ---- has_vignettes ----
#
# is.integer(pkgs$has_vignettes[[11]][[1]])
pkgs$n_vig <- pkgs$has_vignettes |>
  purrr::map(~.x[[1]]) |>
  unlist()
# str(pkgs$n_vig)

# Overall summary
hist(pkgs$n_vig, breaks = 5)
summary(pkgs$n_vig)
summary(as.factor(pkgs$n_vig))
round(prop.table(summary(as.factor(pkgs$n_vig))), 3) * 100

# Workshop a threshold
pkgs$n_vig_cat <- dplyr::case_when(
  pkgs$n_vig > 1 ~ "Low",
  pkgs$n_vig == 1 ~ "Medium",
  .default = "High"
)
table(pkgs$n_vig_cat)
round(prop.table(table(pkgs$n_vig_cat)), 3) * 100


#
# ---- Has Source Control ----
#
# is.integer(pkgs$has_source_control[[11]][[1]] |> length())
pkgs$src_cntrl <- pkgs$has_source_control |>
  purrr::map(~.x[[1]] |> length()) |>
  unlist()
# str(pkgs$n_vig)

# Overall summary
hist(pkgs$src_cntrl, breaks = 5)
summary(pkgs$src_cntrl)
summary(as.factor(pkgs$src_cntrl))
round(prop.table(summary(as.factor(pkgs$src_cntrl))), 3) * 100

# Workshop a threshold
pkgs$src_cntrl_cat <- dplyr::case_when(
  pkgs$src_cntrl > 0 ~ "Low",
  .default = "High"
)
table(pkgs$src_cntrl_cat)
round(prop.table(table(pkgs$src_cntrl_cat)), 3) * 100

#
# ---- has_website ----
#
# is.integer(pkgs$has_website[[11]][[1]] |> length())
pkgs$n_sites <- pkgs$has_website |>
  purrr::map(~.x[[1]] |> length()) |>
  unlist()
# str(pkgs$n_vig)

# Overall summary
hist(pkgs$n_sites, breaks = 5)
summary(pkgs$n_sites)
summary(as.factor(pkgs$n_sites))
round(prop.table(summary(as.factor(pkgs$n_sites))), 3) * 100

# Workshop a threshold
pkgs$website_cat <- dplyr::case_when(
  pkgs$n_sites > 0 ~ "Low",
  .default = "High"
)
table(pkgs$website_cat)
round(prop.table(table(pkgs$website_cat)), 3) * 100

#
# ---- Filter ----
# 

# Exceptions
# If reverse dependencies are "low" risk, AND
# dependencies are "low" risk, AND
# code coverage is "low" risk,
# then, bump lower risk level
# exceptions <- function(data) {
#   pkgs$rev_deps_cat %in% c("Low", "Medium") &
#     pkgs$n_deps_cat %in% c("Low", "Medium") &
#     # pkgs$bug_stat_cat == "Low" & # {riskscore} data not usable
#     pkgs$news_curr_cat &
#     pkgs$n_vig_cat == "Low" &
#     pkgs$src_cntrl_cat == "Low" &
#     pkgs$website_cat == "Low" 
# }



# pkgs$filter_risk <- NULL
filter_pkgs <- pkgs |>
  dplyr::select(
    package, version,
    dwnlds, dwnlds_cat,
    rev_deps, rev_deps_cat,
    n_deps, n_deps_cat,
    news_curr, news_curr_cat,
    n_vig, n_vig_cat,
    src_cntrl, src_cntrl_cat,
    n_sites, website_cat
  ) |>
  dplyr::mutate(
    filter_risk = 
      dplyr::case_when(
        
      dwnlds_cat == "Medium" &
        # exceptions()
        rev_deps_cat %in% c("Low", "Medium") &
        n_deps_cat %in% c("Low", "Medium") &
        # pkgs$bug_stat_cat == "Low" & # {riskscore} data not usable
        news_curr_cat == "Low" &
        n_vig_cat == "Low" &
        src_cntrl_cat == "Low" &
        website_cat == "Low" ~ "Low",
      
      dwnlds_cat == "High" &
        dwnlds > 10000 &
        
        # exceptions()
        rev_deps_cat %in% c("Low", "Medium") &
        n_deps_cat %in% c("Low", "Medium") &
        # pkgs$bug_stat_cat == "Low" & # {riskscore} data not usable
        news_curr_cat == "Low" &
        n_vig_cat == "Low" &
        src_cntrl_cat == "Low" &
        website_cat == "Low" ~ "Medium",
      
      .default = dwnlds_cat
    )
  ) |>
  dplyr::select(package, version, filter_risk, dplyr::everything())

# Downloads alone
table(filter_pkgs$dwnlds_cat)
round(prop.table(table(filter_pkgs$dwnlds_cat)), 3) * 100

# With exceptions
table(filter_pkgs$filter_risk)
round(prop.table(table(filter_pkgs$filter_risk)), 3) * 100

# diff
table(filter_pkgs$filter_risk) - table(filter_pkgs$dwnlds_cat)


build_pkgs <-
  filter_pkgs |>
  dplyr::filter(filter_risk %in% c("Low", "Medium")) |>
  dplyr::pull(package)
length(build_pkgs)



# TODO:
# Figure out what pkg_assess() prompt is for.
# Doesn't happen during background job
# --> opened issue on 'riskmetric' repo
# --> basically un-avoidable

# riskreports
# Install latest (dev) version of quarto?

# Update final decisions

#
# Test out two pkgs, so we can make a useful output to return from val_build
#
outtie <- val_build(
  pkg_names = 'zoo',
  # pkg_names = build_pkgs,
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






