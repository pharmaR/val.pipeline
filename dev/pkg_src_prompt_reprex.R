

# Reprex Inputs!
pkg <- "zoo"
# out_dir <- getwd()
out_dir <- 'dev/riskassessments/reprex'

# ---- Setup Dirs ----
tarballs <- file.path(out_dir, 'tarballs')
sourced <- file.path(out_dir, 'sourced')
if(!dir.exists(out_dir)) dir.create(out_dir)
if(!dir.exists(tarballs)) dir.create(tarballs)
if(!dir.exists(sourced)) dir.create(sourced)


# ---- Get Pkg Version ----
pkg_v <- 
  available.packages() %>%
  as.data.frame() %>%
  dplyr::filter(Package == pkg) %>%
  dplyr::mutate(Pkg_Ver = paste(Package, Version, sep = "_")) %>%
  dplyr::pull(Pkg_Ver)
  

# ---- Download Tarball ----
tarball_url <- paste0("https://cran.r-project.org/src/contrib/", pkg_v,".tar.gz")
utils::download.file(tarball_url,
                     file.path(tarballs, basename(tarball_url)), 
                     quiet = TRUE, mode = "wb")
cat("\n-->", pkg_v,"downloaded.\n")

# ---- Untar ---- 
tar_file <- file.path(tarballs, glue::glue("{pkg_v}.tar.gz"))
utils::untar(tar_file, exdir = sourced)
cat("\n-->", pkg_v,"untarred.\n")

# ---- Ref & Assess ---- 
pkg_ref <- riskmetric::pkg_ref(file.path(sourced, pkg), source = "pkg_source")
cat("\n-->", pkg_v,"referrenced.\n")
pkg_assessment <- riskmetric::pkg_assess(pkg_ref)
cat("\n-->", pkg_v,"assessed.\n")
