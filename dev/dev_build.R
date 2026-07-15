
# Load package
devtools::load_all()


val_date <- Sys.Date()
val_date <- "2026-06-21"
val_date_txt <- gsub("-", "", val_date)
val_dir <- file.path(
  Sys.getenv("RISK_OUTPATH", unset = getwd()),
  glue::glue('R_{getRversion()}'),
  val_date_txt
)

# # Inspect files / pkgs assessed
assessed <- file.path(val_dir, "assessed")
meta_files <- list.files(assessed, pattern = "_meta.rds$")
ass_files <- list.files(assessed, pattern = "_assess_record.rds$")
ass_files |> length()
pkgs <- stringr::word(meta_files, sep = "_", start = 1)
pkgs |> length()

reports <- file.path(val_dir, "reports")
report_files <- list.files(reports) #, pattern = "_meta.rds$")
report_pkgs <- stringr::word(report_files, sep = "_", start = 3)
report_pkgs |> length()

pkgs[!pkgs %in% report_pkgs]

# review qual df
# qual <- readRDS(file.path(val_dir, paste0("qual_evidence_", val_date_txt, ".rds")))





# Create qualified pkg data.frame
# source("dev/pkg_lists.R") # build_pkgs & pkgs for CRAN only
# See the full dependency tree before running val_build()
# these_pkgs <- "withr"  # messes with the entire process
# these_pkgs <- "matrix" # takes 5 mins to install
# these_pkgs <- "askpass"
these_pkgs <- "logrx"
these_pkgs <- "broom"
# these_pkgs <- "dplyr"
# these_pkgs <- "tibble"
# these_pkgs <- "boot"
# these_pkgs <- c("Biobase", "BiocGenerics")
# these_pkgs <- pkgs
# these_pkgs <- c("Biobase", "BiocGenerics", "xlsx", "askpass", "dplyr", "signal")
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
# pkgs |> length()

# temporary until we can figure out what's gone haywire with this pkg
# build_pkgs <- build_pkgs[build_pkgs != "withr"]

# usethis::edit_r_environ() # to configure RISK_OUTPATH

qual <- val_build(
  # pkg_names = build_pkgs,
  pkg_names = these_pkgs, #tidyR required 'decor' pkg be installed!
  ref = "source",
  metric_pkg = "riskmetric", 
  # deps = "depends", # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  deps = NULL,
  # deps_recursive = TRUE,
  deps_recursive = FALSE,
  # val_date = Sys.Date(),
  val_date = as.Date("2026-06-21"),
  replace = TRUE, 
  # use a env var for the out path
  out = Sys.getenv("RISK_OUTPATH", unset = getwd())
    # Sys.getenv("RISK_OUTPATH", unset = getwd())
)

qual_df <- qual$pkgs_df
# View(qual_df)

# 
# Quick run
# 
# pkg_names = these_pkgs
# ref = "source"
# metric_pkg = "riskmetric"
# deps = "depends" # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
# # deps = NULL
# deps_recursive = TRUE
# # deps_recursive = FALSE
# # val_date = Sys.Date()
# val_date = as.Date("2026-06-21")
# replace = FALSE
# out = Sys.getenv("RISK_OUTPATH", unset = getwd())

# -- defaults --
# ref = "source"
# metric_pkg = "riskmetric"
# deps = "depends" # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
# deps_recursive = TRUE
# val_date = Sys.Date()
# # val_date = as.Date("2025-10-07")
# replace = FALSE
# out = Sys.getenv("RISK_OUTPATH", unset = getwd())


