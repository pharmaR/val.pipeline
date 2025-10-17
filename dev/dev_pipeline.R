
# Load package
devtools::load_all()

# Create qualified pkg data.frame
qual <- val_pipeline(
  ref = "source",
  metric_pkg = "riskmetric", 
  deps = "depends", # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  deps_recursive = TRUE,
  val_date = Sys.Date(),
  # val_date = as.Date("2025-10-07"),
  replace = FALSE, 
  out = Sys.getenv("RISK_OUTPATH") %|e|% getwd()
)

# 
# Quick run
# 

# ref = "source"
# metric_pkg = "riskmetric"
# deps = "depends" # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
# deps_recursive = TRUE
# val_date = Sys.Date()
# # val_date = as.Date("2025-10-07")
# replace = FALSE
# out = Sys.getenv("RISK_OUTPATH") %|e|% getwd()



# #
# # Inspect the assessment dir
# #
# 
# val_date_txt <- gsub("-", "", val_date)
# val_dir <- file.path(
#   Sys.getenv("RISK_OUTPATH") %|e|% getwd(),
#   glue::glue('R_{getRversion()}'),
#   val_date_txt
#   )
# 
# 
# # review qual df
# qual <- readRDS(file.path(val_dir, paste0("qual_evidence_", val_date_txt, ".rds")))
# 
# 
# 
# assessed <- file.path(val_dir, "assessed")
# meta_files <- list.files(assessed, pattern = "_meta.rds$")
# ass_files <- list.files(assessed, pattern = "_assessments.rds$")
# 
# # choose a pkg
# # pkg_name <- "zoo"
# # meta_pkg <- meta_files[stringr::str_detect(meta_files, pkg_name)]
# # meta <- readRDS(file.path(assessed, meta_pkg))
# # ass_pkg <- ass_files[stringr::str_detect(ass_files, pkg_name)]
# # ass <- readRDS(file.path(assessed, ass_pkg))
# 
# # explore outputs
# meta
# names(ass)
# ass$covr_coverage$totalcoverage
# ass$downloads_1yr |> prettyNum(big.mark = ",")
# 
# 
# 
