
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
# # 
# 
# ref = "source"
# metric_pkg = "riskmetric"
# deps = "depends" # Note: "depends" this means --> c("Depends", "Imports", "LinkingTo")
# deps_recursive = TRUE
# # val_date = Sys.Date()
# val_date = as.Date("2025-10-27")
# replace = FALSE
# out = Sys.getenv("RISK_OUTPATH", unset = getwd())
# 
# opt_repos = 
#   c(CRAN = "https://packagemanager.posit.co/cran/latest",
#     BioC = 'https://bioconductor.org/packages/3.21/bioc')


#
# Inspect the assessment dir
#

val_date = as.Date("2025-10-28")
val_date_txt <- gsub("-", "", val_date)
val_dir <- file.path(
  Sys.getenv("RISK_OUTPATH", unset = getwd()),
  glue::glue('R_{getRversion()}'),
  val_date_txt
  )


# review qual df

decisions <- pull_config(val = "decisions_lst", rule_type = "default")
# qual <- readRDS(file.path(val_dir, "qual_supplemental.rds")) |>
qual <- readRDS(file.path(val_dir, "qual_evidence.rds")) |>
  dplyr::mutate(decision = factor(decision, levels = decisions),
                final_decision = factor(final_decision, levels = decisions)
                )

# unknown repos
unknown_repos <- qual |>
  dplyr::filter(repos == "unknown")
unknown_repos |> dplyr::select(pkg, repos)
unknown_repos |> pull(pkg) |> length()

# Decisions & final decisions
dec <- table(qual$decision_reason, qual$decision)
dec
findec <- table(qual$final_decision_reason, qual$final_decision)
findec
diff <- findec - dec
diff

slow <- qual |>
  dplyr::filter(assessment_runtime_mins > 45)
slow |> dplyr::select(pkg, assessment_runtime_txt)

# Assemble the assessment's data.frame

assessed <- file.path(val_dir, "assessed")
ass_files <- list.files(assessed, pattern = "_assessments.rds$")
ass_length <- ass_files |> length() # assessment file count
round((ass_files |> length()) / nrow(qual) * 100, 2) # 74.77 percent of total


# Start bundling rds files from assessment & score Rds
rm(assessment_bundle)
assessment_bundle <- purrr::map(ass_files, function(file){
  ass_cnt <- which(ass_files == file)
  # file <- ass_files[2] # for debugging
  score_file <- gsub("_assessments.rds$", "_scores.rds", file)
  if(file.exists(file.path(assessed, file))) pkg_assessment <- readRDS(file.path(assessed, file))
  if(file.exists(file.path(assessed, file))) pkg_scores <- readRDS(file.path(assessed, score_file))
  pkg_v <- gsub("_assessments.rds$", "", file)
  pkg <- stringr::word(pkg_v, 1, sep = "_")
  ver <- stringr::word(pkg_v, 2, sep = "_")
  
  cat(paste0("\n\n#", ass_cnt, " of ", ass_length, ": ", pkg))
  # pkg_assessment$downloads_1yr |> as.numeric()
  work_df <- workable_assessments(
    pkg = pkg,
    ver = ver,
    val_date = val_date,
    metric_pkg = metric_pkg,
    source = list(assessment = pkg_assessment, scores = pkg_scores),
    source_ref = "source" # constant for this date
  )
  work_df
}) |>
  purrr::reduce(dplyr::bind_rows)
saveRDS(assessment_bundle, file.path(val_dir, "qual_assessments.rds"))
saveRDS(assessment_bundle, file.path("dev", "qual_assessments_20251028.rds"))
# assessment_bundle <- readRDS(file.path("dev", "qual_assessments_20251023.rds"))

# count how many r_cmd_check_errors there are
check <- assessment_bundle |>
  dplyr::filter(!is.na(r_cmd_check_errors)) |>
  dplyr::as_tibble()
  
round(nrow(check) / nrow(assessment_bundle) * 100, 2) # 24% came out clean

checked <- check |>
  dplyr::mutate(r_cmd_check_err = r_cmd_check[[1]][[2]]) |>
  dplyr::mutate(r_cmd_check_warn = r_cmd_check[[1]][[3]]) |>
  dplyr::select(package, version, val_date, r_cmd_check,
                r_cmd_check_errors, r_cmd_check_err,
                r_cmd_check_warnings, r_cmd_check_warn)
checked |>
  dplyr::filter(r_cmd_check_err != 0) |>
  dplyr::as_tibble()


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
