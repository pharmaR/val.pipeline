# val.pipeline (development version)

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
