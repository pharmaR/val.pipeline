


#
# ---- Dev ----
#


val_date <- "2025-10-27"
val_dir <- file.path(Sys.getenv("RISK_OUTPATH", unset = getwd()), paste0("R_", getRversion()), gsub("-","",val_date))
reports <- list.files(file.path(val_dir, "reports"))
reports |> length()
# any(stringr::str_detect(reports, "bit"))


source("dev/pkg_lists.R")
# which(pkgs == "codetools")
# which(pkgs == "bit")

# chk_pkg <- "VGAM"
chk_pkg <- "GenomeInfoDb"
which(pkgs == chk_pkg)
chk_nxt <- pkgs[which(pkgs == chk_pkg) + 1]

assessed <- list.files(file.path(val_dir, "assessed"))
any(stringr::str_detect(assessed, chk_pkg)) # there
any(stringr::str_detect(assessed, chk_nxt)) # not there

# What's next?
pack <- pkgs[which(pkgs == chk_pkg) + 1]
pack


  

#
# ---- val_pkg() ----
#

remote_pkgs <- pull_config(val = "remote_only", rule_type = "default")
avail_pkgs <- available.packages() |> as.data.frame()
# val_date <- Sys.Date()
val_date <- as.Date("2025-10-27")
val_dir <- file.path(Sys.getenv("RISK_OUTPATH", unset = getwd()), paste0("R_", getRversion()), gsub("-","",val_date))

source("dev/pkg_lists.R") # build_pkgs & pkgs

### CRAN pkgs ###
# pack = 'askpass' # 2.5 - 3 mins when deps, 2 pkgs, no prompts
# pack = 'withr'
# pack <- pkgs[which(pkgs == "SuppDists") + 1] # last left off:


### BioC pkgs ###
# pack = 'Biobase'
# pack = 'BiocGenerics'

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
pkg_meta[!names(pkg_meta) %in% c("rev_deps","depends","suggests")]  
assessed <- file.path(val_dir, "assessed")
ass_files <- list.files(assessed, pattern = "_assessments.rds$")
ass_pkg <- ass_files[stringr::str_detect(ass_files, pack)]
ass <- readRDS(file.path(assessed, ass_pkg))
ass$r_cmd_check


