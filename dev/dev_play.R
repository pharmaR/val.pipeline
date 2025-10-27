


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




## This can be rather slow  with a non-local CRAN mirror
## and might fail (slowly) without Internet access in that case.

options(repos = c(CRAN = "https://cloud.r-project.org"))
set.seed(11)  # but the packages chosen will change as soon as CRAN does.
pdb <- CRAN_package_db()
dim(pdb)

## DESCRIPTION fields included:
colnames(pdb)

## Summarize publication dates:
summary(as.Date(pdb$Published))

## Summarize numbers of packages according to maintainer:
summary(lengths(split(pdb$Package, pdb$Maintainer)))

## Packages with 'LASSO' in their Description:
pdb$Package[grepl("LASSO", pdb$Description)]

results <- CRAN_check_results()

## Available variables:
names(results)

## Tabulate overall check status according to flavor:
with(results, table(Flavor, Status))

details <- CRAN_check_details()

## Available variables:
names(details)

## Tabulate checks according to their status:
tab <- with(details, table(Check, Status))
tab

## Inspect some installation problems:
bad <- subset(details,
              ((Check == "whether package can be installed") &
                 (Status != "OK")))
## Show a random sample of up to 6
head(bad[sample(seq_len(NROW(bad)), NROW(bad)), ])

issues <- CRAN_check_issues()
head(issues)

## Show counts of issues according to kind:
table(issues[, "kind"])

## Summarize CRAN check status for 10 randomly-selected packages
## (reusing the information already read in):
pos <- sample(seq_len(NROW(pdb)), 10L)
summarize_CRAN_check_status(pdb[pos, "Package"],
                            results, details, issues)




#
# ---- Dev ----
#
val_date <- "2025-10-07"
val_dir <- file.path(Sys.getenv("RISK_OUTPATH", unset = getwd()), paste0("R_", getRversion()), gsub("-","",val_date))
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
    out = Sys.getenv("RISK_OUTPATH", unset = getwd())
)
  

#
# ---- val_build()----
#

source("dev/pkg_lists.R") # # build_pkgs & pkgs
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
  out = Sys.getenv("RISK_OUTPATH", unset = getwd())
) 




#
# ---- val_pkg() ----
#

remote_pkgs <- pull_config(val = "remote_only", rule_type = "default")
avail_pkgs <- available.packages() |> as.data.frame()
val_date <- Sys.Date()
# val_date <- as.Date("2025-10-07")
val_dir <- file.path(Sys.getenv("RISK_OUTPATH", unset = getwd()), paste0("R_", getRversion()), gsub("-","",val_date))

source("dev/pkg_lists.R") # build_pkgs & pkgs

### CRAN pkgs ###
# pack = 'askpass' # 2.5 - 3 mins when deps, 2 pkgs, no prompts
# pack = 'withr'
# pack <- pkgs[which(pkgs == "SuppDists") + 1] # last left off:


### BioC pkgs ###
# pack = 'Biobase'
pack = 'BiocGenerics'

### Quick Load args ###
# pkg = pack
# ver = avail_pkgs$Version[avail_pkgs$Package == pack]
# ref = if(pack %in% remote_pkgs) 'remote' else 'source'
# metric_pkg = "riskmetric"
# out_dir = val_dir

### Run it ###
pkg_meta <- val_pkg(
  pkg = pack,
  ver = avail_pkgs$Version[avail_pkgs$Package == pack],
  avail_pkgs = avail_pkgs,
  ref = if(pack %in% remote_pkgs) 'remote' else 'source',
  metric_pkg = "riskmetric", 
  out_dir = val_dir,
  val_date = val_date
  )

### Inspect output ###
pkg_meta[!names(pkg_meta )%in% c("rev_deps","depends","suggests")]  
assessed <- file.path(val_dir, "assessed")
ass_files <- list.files(assessed, pattern = "_assessments.rds$")
ass_pkg <- ass_files[stringr::str_detect(ass_files, pack)]
ass <- readRDS(file.path(assessed, ass_pkg))
ass$r_cmd_check


