
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
  out = Sys.getenv("RISK_OUTPATH", unset = getwd())
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
# out = Sys.getenv("RISK_OUTPATH", unset = getwd())



#
# Inspect the assessment dir
#

val_date_txt <- gsub("-", "", val_date)
val_dir <- file.path(
  Sys.getenv("RISK_OUTPATH", unset = getwd()),
  glue::glue('R_{getRversion()}'),
  val_date_txt
  )


# review qual df

decisions <- pull_config(val = "decisions_lst", rule_type = "default")
qual <- readRDS(file.path(val_dir, "qual_evidence.rds")) |>
  dplyr::mutate(decision = factor(decision, levels = decisions),
                final_decision = factor(final_decision, levels = decisions)
                )

# unknown repos
unknown_repos <- qual |>
  dplyr::filter(repos == "unknown") 
unknown_repos |> pull(pkg) |> length()

# Decisions & final decisions
dec <- table(qual$decision_reason, qual$decision)
findec <- table(qual$final_decision_reason, qual$final_decision)
diff <- findec - dec
diff

slow <- qual |>
  dplyr::filter(assessment_runtime_mins > 45) 
slow |> dplyr::select(pkg, assessment_runtime_txt)

assessed <- file.path(val_dir, "assessed")
ass_files <- list.files(assessed, pattern = "_assessments.rds$")
ass_files |> length() # assessment file count
round((ass_files |> length()) / nrow(qual) * 100, 2) # percent of total


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
