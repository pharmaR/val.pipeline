# val.pipeline (development version)

- Found out that Posit provides their own validation documentation for several
co-horts of packages they develop, so we've added them to the config's
`approved_pkgs` config element by default. (#42)
- Fix `qual_metadata.rds` so `final_decision` / `final_decision_reason` are
populated for every package (not just dependency-downgraded ones). The
interim pre-propagation frame is now saved separately as
`qual_metadata0.rds`, and the post-propagation frame is written before the
per-package meta RDS update walk so any error inside that walk cannot leave
a stale `qual_metadata.rds` on disk. (#53)
- Add `decision_reason_note` / `final_decision_reason_note` to the per-package
  meta bundle. When `decision_reason == "Risk Assessment"` and the package
  didn't land in the lowest-risk tier, this note lists the specific metrics
  whose per-metric category matched the aggregated `final_risk` — i.e. the
  metrics that drove the package into `"Medium"` / `"High"` (#37).

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
