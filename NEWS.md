# val.pipeline (development version)

- Refresh `renv.lock` so `R-CMD-check` passes on `ubuntu-latest` and
  `windows-latest` again. Pins the CRAN URL to a stable date
  (`packagemanager.posit.co/cran/2026-06-21`) instead of `latest`, bumps
  Bioconductor to `3.22` and R to `4.5.2`, updates several core packages
  (dplyr `1.2.1`, rlang `1.2.0`, vctrs `0.7.3`, lifecycle `1.0.5`,
  riskmetric `0.2.7`, riskscore `0.1.0`), and normalizes per-package
  `Repository` fields to `"CRAN"` so `renv::restore()` resolves via the
  runner-appropriate OS URL at install time (previously they were hard-
  coded to `.../__linux__/rhel9/latest`, causing `libicui18n.so.67`
  load failures on Ubuntu 24.04). (#56)

- Found out that Posit provides their own validation documentation for several
co-horts of packages they develop, so we've added them to the config's
`approved_pkgs` config element by default. (#42)

# val.pipeline 0.0.1

* Dynamically change the packagemanager date used for `CRAN` repo to reflect the
data source at hand. For example, the `riskscore` assessment date or the val_date
specified in `val.pipeline::val_pipeline()` (#3).
* Bug fix ensuring tar files are downloaded for BioConductor packages.
* "Engage" secondary metric logic for non-CRAN pkgs
* Customized `riskreports` package report template
* Added a significant amount of `testthat` tests
* Exported assessment records for compilation / summary
* Corrected issue where some packages repos URL were "unknown"

# val.pipline 0.0.0

* Package born.
