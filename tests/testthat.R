# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(val.pipeline)

# Static option:
# options(repos = c(
#   CRAN = 'https://packagemanager.posit.co/cran/latest',
#   BioC = 'https://bioconductor.org/packages/3.21/bioc'
# ))

# Config
opt_repos <- pull_config(val = "opt_repos", rule_type = "default") |> unlist()

# Set
options(repos = opt_repos, pkgType = "source", scipen = 999)
# options("repos")

test_check("val.pipeline")
