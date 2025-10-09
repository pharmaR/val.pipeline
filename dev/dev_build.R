
# Load package
devtools::load_all()

# Create qualified pkg data.frame
# trying to just do bioconductor packages
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
# Quick run
# 
# ref = "source"
# metric_pkg = "riskmetric"
# deps = "depends" # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
# deps_recursive = TRUE
# val_date = Sys.Date()
# # val_date = as.Date("2025-10-07")
# replace = FALSE
# out = 'dev/riskassessments'