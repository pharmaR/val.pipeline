
# update pertinent pkgs as needed
# remotes::install_github("pharmar/riskscore", force = TRUE, ref = "latest") # v0.1.0
# utils::install.packages("riskmetric") # v0.2.5
# renv::snapshot()
# restart R

# Load package
devtools::load_all()

# Create qualified pkg data.frame
qual <- val_pipeline(
  ref = "source",
  metric_pkg = "riskmetric", 
  deps = c("depends"), #, "suggests"), # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
  deps_recursive = TRUE,
  val_date = as.Date("2026-06-21"), #Sys.Date(),
  # val_date = as.Date("2025-10-07"),
  replace = FALSE, 
  out = Sys.getenv("RISK_OUTPATH", unset = getwd())
)

# 
# Quick run
# # 
# 
ref = "source"
metric_pkg = "riskmetric"
deps = c("depends") #,"suggests") # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
deps_recursive = TRUE
# val_date = Sys.Date()
val_date = as.Date("2026-06-21")
replace = FALSE
out = Sys.getenv("RISK_OUTPATH", unset = getwd())
opt_repos =
  c(CRAN = "https://packagemanager.posit.co/cran/2026-06-21",
    BioC = 'https://bioconductor.org/packages/3.22/bioc')


#
# Inspect the assessment dir
#
# 
val_date = as.Date("2026-06-21")
val_date_txt <- gsub("-", "", val_date)
val_dir <- file.path(
  Sys.getenv("RISK_OUTPATH", unset = getwd()),
  glue::glue('R_{getRversion()}'),
  val_date_txt
  )



# # # Inspect files / pkgs assessed vs reports
# assessed <- file.path(val_dir, "assessed")
# meta_files <- list.files(assessed, pattern = "_meta.rds$")
# ass_files <- list.files(assessed, pattern = "_assess_record.rds$")
# ass_files |> length()
# pkgs <- stringr::word(meta_files, sep = "_", start = 1)
# pkgs |> length()
# 
# reports <- file.path(val_dir, "reports")
# report_files <- list.files(reports) #, pattern = "_meta.rds$")
# report_pkgs <- stringr::word(report_files, sep = "_", start = 3)
# report_pkgs |> length()
# 
# pkgs[!pkgs %in% report_pkgs]



# Review assessment df (below)


# Review qual df
decisions <- pull_config(val = "decisions_lst", rule_type = "default")
qual_metadata <- readRDS(file.path(val_dir, "qual_metadata.rds"))
qual <- qual_metadata #|>
  # dplyr::mutate(decision = factor(decision, levels = decisions),
  #               final_decision = factor(final_decision, levels = decisions)
  #               )

  


  
# Summarize Decisions & final decisions into table
dec <- table(qual$decision_reason, qual$decision)
dec
findec <- table(qual$final_decision_reason, qual$final_decision)
findec
diff <- findec - dec
diff



# Were final decisions rendered in qual_metadata? If not, run this:


slow <- qual |>
  dplyr::filter(assessment_runtime_mins > 45)
slow |> dplyr::select(pkg, assessment_runtime_txt)

# Search for any 'unknown' repos
unknown_repos <- qual |>
  dplyr::filter(repos == "unknown")
unknown_repos |> dplyr::select(pkg, repos)
unknown_repos |> pull(pkg) |> length()


#
# Assemble the assessment's data.frame
#
assessed <- file.path(val_dir, "assessed")
ass_files <- list.files(assessed, pattern = "_assessments.rds$")
ass_length <- ass_files |> length() # assessment file count
# Calculate percent of pkgs that ran an assessment, out of total pkgs in qual_metadata
round((ass_files |> length()) / nrow(qual) * 100, 2)
  # 72.11 percent of total


#
# If needed, rebuild assessments data.frame (qual_assessments.rds)
#

#
# Option 1:
# Start bundling rds files from assessment lists (rds) & score lists (Rds), OR...
#

# rm(assessment_bundle)
# assessment_bundle <- purrr::map(ass_files, function(file){
#   ass_cnt <- which(ass_files == file)
#   # file <- ass_files[2] # for debugging
#   score_file <- gsub("_assessments.rds$", "_scores.rds", file)
#   if(file.exists(file.path(assessed, file))) pkg_assessment <- readRDS(file.path(assessed, file))
#   if(file.exists(file.path(assessed, file))) pkg_scores <- readRDS(file.path(assessed, score_file))
#   pkg_v <- gsub("_assessments.rds$", "", file)
#   pkg <- stringr::word(pkg_v, 1, sep = "_")
#   ver <- stringr::word(pkg_v, 2, sep = "_")
# 
#   cat(paste0("\n\n#", ass_cnt, " of ", ass_length, ": ", pkg))
#   # pkg_assessment$downloads_1yr |> as.numeric()
#   work_df <- workable_assessments(
#     pkg = pkg,
#     ver = ver,
#     val_date = val_date,
#     metric_pkg = metric_pkg,
#     source = list(assessment = pkg_assessment, scores = pkg_scores),
#     source_ref = "source" # constant for this date
#   )
#   work_df
# }) |>
#   purrr::reduce(dplyr::bind_rows)



#
# Option 2:
# Assemble the assessment's data.frame by binding aesses_record's
#

# assessed <- file.path(val_dir, "assessed")
# ass_files <- list.files(assessed, pattern = "_assess_record.rds$")
# ass_length <- ass_files |> length() # assessment file count
# # round((ass_files |> length()) / nrow(qual) * 100, 2) # 74.77 percent of total

# Start bundling rds files from assessment & score Rds, OR...
# rm(assessment_bundle)
# assessment_bundle <- purrr::map(ass_files[1:3], function(file){
#   # file <- ass_files[1]
#   ass_cnt <- which(ass_files == file)
#   # file <- ass_files[2] # for debugging
#   # score_file <- gsub("_assessments.rds$", "_scores.rds", file)
#   if(file.exists(file.path(assessed, file))) pkg_assess_record <- readRDS(file.path(assessed, file))
#   pkg_v <- gsub("_assess_record.rds", "", file)
#   pkg <- stringr::word(pkg_v, 1, sep = "_")
#   ver <- stringr::word(pkg_v, 2, sep = "_")
#   
#   cat(paste0("\n\n#", ass_cnt, " of ", ass_length, ": ", pkg))
#   # pkg_assessment$downloads_1yr |> as.numeric()
#   pkg_assess_record
# }) |>
#   purrr::list_rbind()
#   # purrr::reduce(dplyr::bind_rows)


#
# Save & or load result
#
# saveRDS(assessment_bundle, file.path(val_dir, "qual_assessments.rds"))
assessment_bundle <- readRDS(file.path(val_dir, "qual_assessments.rds"))
# # saveRDS(assessment_bundle, file.path("dev", "qual_assessments_20251028.rds"))
# # assessment_bundle <- readRDS(file.path("dev", "qual_assessments_20251023.rds"))



#
# Analyze Assessments df
#
# # Count how many r_cmd_check_errors there are
# check <- assessment_bundle |>
#   dplyr::filter(!is.na(r_cmd_check_errors)) |>
#   dplyr::as_tibble()
# 
# round(nrow(check) / nrow(assessment_bundle) * 100, 2) # 24% came out clean
# 
# checked <- check |>
#   dplyr::mutate(r_cmd_check_err = r_cmd_check[[1]][[2]]) |>
#   dplyr::mutate(r_cmd_check_warn = r_cmd_check[[1]][[3]]) |>
#   dplyr::select(package, version, val_date, r_cmd_check,
#                 r_cmd_check_errors, r_cmd_check_err,
#                 r_cmd_check_warnings, r_cmd_check_warn)
# checked |>
#   dplyr::filter(r_cmd_check_err != 0) |>
#   dplyr::as_tibble()



# # # choose a pkg
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
