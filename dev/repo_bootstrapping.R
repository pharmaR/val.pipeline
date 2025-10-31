
usethis::use_package("R", type = "Depends", min_version = "4.1.0") # because of |> pipe

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
devtools::document() # ran 

# Install packages
# CRAN pkgs
pak::pak("dplyr")
pak::pak("stringr")
pak::pak("purrr")
pak::pak("glue")
pak::pak("tidyr")

# GitHub pkgs
pak::pak("pharmaR/riskmetric")
pak::pak("pharmaR/riskreports")
pak::pak("Sanofi-Public/risk.assessr")

# Add to Description
usethis::use_package("dplyr")
usethis::use_package("stringr")
usethis::use_package("purrr")
usethis::use_package("glue")
usethis::use_package("tidyr")
usethis::use_package("tools")
usethis::use_package("config")
usethis::use_dev_package("riskmetric") #, min_version = "0.2.5") 
usethis::use_dev_package("riskreports")
usethis::use_dev_package("risk.assessr", type = "Suggests")
usethis::use_dev_package("riskscore", type = "Suggests")
# renv
pak::pak("renv")
renv::init()
