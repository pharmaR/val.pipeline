# val.pipeline

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/pharmaR/val.pipeline/actions/workflows/R-CMD-check-dev.yaml/badge.svg)](https://github.com/pharmaR/val.pipeline/actions/workflows/R-CMD-check-dev.yaml)
<!-- badges: end -->

## Purpose

This is an experimental repository with the goal of bringing together a lot of
the R Val Hub's tools under a single workflow to produce the outputs necessary
to qualify R packages for a GxP system. Note: this repository exists to showcase
"a way" to perform package validation. It is certainty not the only way, or
even the recommended way. Every organization must perform their own due
diligence to meet the documentation requirements imposed by regulatory
authorities within their risk tolerances.

## Installation

This package is only available on GitHub (for now). You can install the latest version
using the code below:

``` r
# Install {riskscore} package from GitHub
remotes::install_github("pharmaR/val.pipeline")

```

## Framework

The primary driver of `val.pipeline` is the namesake function: `val_pipeline()`.
It's a pipeline to validate R packages using specific metrics and criteria,
spelled out in the package's config file. This function orchestrates the
reduction of a large set of packages delivered through various sources
(either `pharmaR/riskscore` or a user-provided data set) based 'primary' &
'exception' criteria. Then, it builds the assessment cohort using
`val_build()`, returning a qualified list of packages and all required evidence
needed for provisioning for a package management system like PPM.


`val_build()` builds a risk assessment validation for a set of R packages from
various sources (CRAN / Bioconductor / GitHub), with the ability to include
(optionally recursive) dependencies and Suggests, then save the results in a
structured directory. The cherry on top is that this build will use logic
from another exported function, `val_decision()` to not only apply org-specific 
risk decisions too all packages assessed, but goes back around and will
re-categorize (invalidated) decisions based on whether any dependencies were
categorized as "High Risk" / "Rejected". It 
also is intelligent enough to sort the list of packages to run those with the 
most dependencies first, so that if a package fails, it doesn't waste anytime
running it's reverse dependence. 

After the pipeline applies a decision
onto each package using criteria provided in the config file, it generates a
report detailing specifics of the assessment as supporting evidence. The end
result is a directory containing the assessment results and reports for each
package evaluated.

The assessment process at the package level includes steps to download the
package source (preferred), install the package, assess the package using the
user-specified metric package (only `riskmetric` supported currently),
apply risk decisions, and build a report. Note: to save time, every
package will be assessed using a "pkg_cran_remote" reference initially to see if
any primary metrics met the"auto-accept" threshold(s), if applicable. If they 
did, then the running then computing 'covr_coverage' will be skipped.



## Usage

``` r
library(val.pipeline)

# Example config file
config <- system.file("config.yml", package = "val.pipeline")

# Run the pipeline
qualification_results <- val_pipeline(
  ref = "source",
  metric_pkg = "riskmetric", 
  deps = "depends", # Note: this means --> c("Depends", "Imports", "LinkingTo")
  deps_recursive = TRUE,
  val_date = Sys.Date(),
  replace = FALSE, 
  out = Sys.getenv("RISK_OUTPATH") %|e|% getwd()
)
```

