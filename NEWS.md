# val.pipeline (development version)

- Add `decision_reason_note` / `final_decision_reason_note` to the per-package
  meta bundle. When `decision_reason == "Risk Assessment"` and the package
  didn't land in the lowest-risk tier, this note lists the specific metrics
  whose per-metric category matched the aggregated `final_risk` — i.e. the
  metrics that drove the package into `"Medium"` / `"High"` (#37).
- Extend `decision_reason_note` / `final_decision_reason_note` to the
  `"Dependency"` case: when a package is downgraded because a dep (or, if
  `deps` includes `"Suggests"`, a suggest) failed, the note now lists the
  failing dep package name(s), comma-separated. Best-effort — may
  under-report if a failing dep wasn't yet in the tracked failure set at
  the time of the check (#37).

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
