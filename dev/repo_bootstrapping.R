
# Add pkg deps
utils::install.packages("pak")
pak::pak("usethis")
pak::pak("devtools")

# General pkg infrastructure
usethis::use_description(check_name = F)
usethis::use_mit_license()
usethis::use_lifecycle_badge( "Experimental" ) #Experimental, Maturing, Stable, Superseded, Archived, Dormant, Questioning
usethis::use_news_md( open = TRUE )
usethis::use_testthat()
devtools::document()


# CRAN pkgs
pak::pak("dplyr")
pak::pak("stringr")
pak::pak("purrr")
pak::pak("glue")
pak::pak("tidyr")

# GitHub pkgs
pak::pak("pharmaR/riskmetric")
pak::pak("pharmaR/riskreports")

# Add to Description
usethis::use_package("dplyr")
usethis::use_package("stringr")
usethis::use_package("purrr")
usethis::use_package("glue")
usethis::use_package("tidyr")
usethis::use_dev_package("riskmetric") #, min_version = "0.2.5") 
usethis::use_dev_package("riskreports")

# renv
pak::pak("renv")
renv::init()