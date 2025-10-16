
# Load package
devtools::load_all()

# Create qualified pkg data.frame
source("dev/pkg_lists.R") # build_pkgs & pkgs for CRAN only
# See the full dependency tree before running val_build()
# these_pkgs <- "withr"  # messes with the entire process
# these_pkgs <- "matrix" # takes 5 mins to install
these_pkgs <- "askpass"
# these_pkgs <- c("Biobase", "BiocGenerics")
# these_pkgs <- build_pkgs

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

usethis::edit_r_environ()

qual <- val_build(
  # pkg_names = build_pkgs,
  pkg_names = these_pkgs,
  ref = "source",
  metric_pkg = "riskmetric", 
  # deps = "depends", # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  deps = NULL,
  # deps_recursive = TRUE,
  deps_recursive = FALSE,
  val_date = Sys.Date(),
  # val_date = as.Date("2025-10-07"),
  replace = FALSE, 
  # use a env var for the out path
  out = Sys.getenv("RISK_OUTPATH") %|e|% getwd()
    # Sys.getenv("RISK_OUTPATH") %|e|% getwd()
)

qual_df <- qual$pkgs_df
View(qual_df)
# 
# Quick run
# 

# -- dev --
pkg_names = these_pkgs
ref = "source"
metric_pkg = "riskmetric"
# deps = "depends", # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
deps = NULL
# deps_recursive = TRUE
deps_recursive = FALSE
val_date = Sys.Date()
# val_date = as.Date("2025-10-07")
replace = FALSE
out = Sys.getenv("RISK_OUTPATH") %|e|% getwd()

# -- defaults --
# ref = "source"
# metric_pkg = "riskmetric"
# deps = "depends" # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
# deps_recursive = TRUE
# val_date = Sys.Date()
# # val_date = as.Date("2025-10-07")
# replace = FALSE
# out = Sys.getenv("RISK_OUTPATH") %|e|% getwd()


