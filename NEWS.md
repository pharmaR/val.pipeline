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
- Refactor the `"Auto-Accepted"` decision reason: `decision_reason` is now
  the short label `"Auto-Accepted"` (previously the metric names were
  concatenated into the reason string), and `decision_reason_note` lists
  the auto-accepted metric name(s), comma-separated. Consistent with the
  same-shape treatment of `"Risk Assessment"` and `"Dependency"` (#37).

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
