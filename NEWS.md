# val.pipeline 0.1.48

- **Defensive Code coverage row in per-package reports.** The
  Code coverage row in the summary metric table (and the
  effective-coverage blockquote) are now wrapped in `tryCatch()`,
  so a mal-shaped `covr_coverage` field on the assessment object
  can no longer bleed a raw error like *"Invalid index: field name
  'covr_coverage' not found"* into the rendered PDF/HTML. On error
  the row falls back to `Not calculated` and a small note is
  emitted beneath the metric table naming the field + underlying
  error message. Also works around a bug in
  `riskreports::is_risk_error()` that returned `FALSE` for a real
  `pkg_metric_error` when the object also inherited `simpleError`
  (this had been surfacing riskmetric's raw
  `"object 'res' not found"` message as if it were the coverage
  percentage). Missing `covr_coverage` fields on stubbed
  assessments now carry the fuller `pkg_metric_covr_coverage`
  class chain that a real riskmetric error would have. (#161)

# val.pipeline 0.1.47

- **Isolate `capture_covr_skip_report()` in a subprocess and add a
  package skip list.** The follow-on `testthat::test_dir()` run
  introduced in #150 exposed val.pipeline to native crashes in a
  package's own test suite (some packages ship hand-rolled OpenMP
  loops that bypass our worker-side `OMP_NUM_THREADS` caps and can
  trip glibc `free(): invalid next size (fast)` heap corruption).
  `capture_covr_skip_report()` now runs the `test_dir()` call inside
  a `callr::r()` child process by default (`subprocess = TRUE`); a
  native crash there kills only the child, the worker survives, and
  the affected package simply gets `covr_skip_report = NULL`.
  Gracefully falls back to in-process when `callr` isn't installed.
  Also adds a new `covr_skip_report$skip_pkgs:` field to
  `inst/config.yml` (belt-and-suspenders: skip child spin-up for
  packages known to crash), overridable via
  `options(val.pipeline.covr_skip_report_skip_pkgs = c(...))`.
  Seeded with `np` (Nonparametric Kernel Smoothing Methods for
  Mixed Data Types) which reliably tripped the crash on the
  2026-08-29 run. Adds `callr` to `Suggests` and emits a
  `val_msg()` naming the offending package source when the child
  process fails, so operators can distinguish a native crash from
  ordinary missing skip-report data. (#159)

# val.pipeline 0.1.46

- **Pre-flight write probe.** `val_build()` now verifies that
  `val_dir`, `assessed/`, `tempdir()`, and `.libPaths()[1]` are all
  writable by the current process before dispatching any package
  assessments; every multisession worker re-probes `tempdir()` and
  `.libPaths()[1]` at task start (they can differ from the parent).
  Refuses to proceed with a clear diagnostic naming every offending
  path plus the effective uid, instead of surfacing hours later as a
  cryptic assembler EACCES like `can't create simple.o: Permission
  denied` when a package under assessment hits a read-only mount.
  Also emits the resolved per-worker `tempdir()` / `.libPaths()[1]`
  at the verbose log tier so post-mortems can identify which mount
  was in play. New helpers `probe_writable_dir()` and
  `assert_writable_dirs()`. (#157)

# val.pipeline 0.1.45

- **Harden multisession workers against environment starvation and
  native-thread oversubscription.** `val_build.R` now propagates
  `HOME`, `TMPDIR`, `XDG_CACHE_HOME`, and `XDG_DATA_HOME` across the
  multisession boundary so package install-time steps that resolve
  paths from `$HOME` (e.g. `basilisk`/`pyenv` bootstrap) no longer
  hit `/.pyenv: Permission denied` inside workers. Also caps
  `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`, `MKL_NUM_THREADS`,
  `RCPP_PARALLEL_NUM_THREADS`, and `data.table::setDTthreads()` to
  `1` per worker (override with `options(val.pipeline.worker_omp_threads
  = "N")`), preventing `workers × cores` thread multiplication that
  can trigger `free(): invalid next size (fast)` glibc heap
  corruption on BLAS-heavy tests. (#153)
  
# val.pipeline 0.1.44

- **Per-package report: fix the covr skip-summary section.** Multiple
  cleanups to the "Test skip summary" block in
  `inst/report/package/pkg_template.qmd`:
  - **Percentages fixed.** The totals table was showing `100.0%` for
    every row because the `ifelse()` percentage expression collapsed
    to a scalar on a scalar condition and then recycled across all
    rows. Rewritten with `if`/`else` so per-row percentages render
    correctly.
  - **Estimated effective `covr_coverage`** now renders as its own
    blockquote paragraph instead of inline text, so a reviewer
    skimming the report can spot it at a glance.
  - **Top skip reasons** table now leads with a one-sentence
    explanation clarifying that the entries are the raw message
    strings passed to `testthat::skip*()` by the package's own test
    suite (so cryptic messages like `"testing depth 3 is below
    current testing specification 5"` or `"TODO"` are the package
    maintainer's wording, not something val.pipeline invented).
  - **Section always renders.** Auto-accepted packages and any pkg
    whose `covr_coverage` was above the capture threshold used to
    omit the section entirely; it now emits the header plus a short
    explanation of why the data wasn't captured (auto-accept, above
    threshold, non-testthat, or capture disabled).
  - **R CMD check summary cell hardened** against unexpected
    newline / `|` content that could break table 2.1's markdown row
    parser and cause subsequent report sections to render inside
    it.
  - **Duplicate `covr_coverage` row removed.** Table 2.1 previously
    sometimes showed two coverage rows -- one labelled `Code
    coverage` (with `%`) added downstream and one labelled
    `covr_coverage` (without `%`) emitted upstream by
    `summary_table()`. The formatted `Code coverage` row is now
    kept and any raw `covr_coverage`-labelled row is dropped. (#155)

# val.pipeline 0.1.42

- **`covr_skip_report`: surface which tests are being skipped and
  why.** `val_pkg()` now runs a standalone
  `testthat::test_dir()` on the package source right after the
  final `pkg_assess()` call, under the same env-var block as
  `assess_covr_coverage` (`pull_covr_env_vars()` from #146), and
  captures per-block skip counts + top skip messages via the new
  `capture_covr_skip_report()` helper. The full report rides as an
  attribute on `pkg_assessment` (read directly by the per-package
  riskreports template); matching scalar summaries
  (`covr_n_test`, `covr_n_skip`, `covr_pct_skip`) land on
  `meta_list` so `val_finalize()` binds them into
  `qual_metadata.rds`.
  - **Per-package report**: new "Test skip summary" section in
    `inst/report/package/pkg_template.qmd` — totals table + top-10
    skip reasons. Guarded on the attribute being present, so older
    assessment RDS files render unchanged.
  - **Summary report**: new "Test skip summary (`covr_skip_report`)"
    section in `inst/report/summary/summary_template.qmd` — skip-%
    distribution buckets + top-25 packages by skip count paired
    with their `covr_coverage` %. Backfills the three scalar cols
    with `NA` so historical `qual_metadata.rds` files still render.
  - Only runs on non-auto-accepted packages (the population that
    actually runs `assess_covr_coverage`). `testthat`-based tests
    only; tinytest / RUnit / no-tests packages return `NULL`
    silently and the aggregate cols are stamped `NA_integer_` /
    `NA_real_`. (#150)
  - **Threshold gate** (perf): by default the extra
    `test_dir()` is only run for packages whose raw
    `covr_coverage` came in below **65** (matching the
    `covr_coverage` rule's Medium/Low cutoff). Packages already
    above the threshold have covr_coverage as their auto-accept
    lever, so the skip report adds no decision-relevant info
    while doubling per-pkg wall-clock. New `covr_skip_report:`
    config block (`capture`, `threshold`) governs the default;
    two new `val_pipeline()` args
    (`capture_covr_skip_report = TRUE`,
    `covr_skip_report_threshold = NULL`) override per run
    without editing config. `covr_skip_report_threshold = NULL`
    (default) falls back to the config threshold (65); pass
    `100` to capture for every non-auto-accept pkg (any real
    coverage number is < 100); pass `FALSE` to
    `capture_covr_skip_report` to disable entirely.
  - **Effective coverage estimate**: new
    `covr_effective_coverage` scalar on `meta_list` /
    `qual_metadata.rds`, computed as
    `covr_coverage / (1 - pct_skip / 100)` capped at 100.
    Rough upper-bound projection of what coverage would have
    been if the skipped test blocks had covered code at the
    average rate of the blocks that ran. Reviewer-facing signal
    for "would this pkg have passed if not for skips?" —
    labelled as an estimate in both per-package and summary
    reports so it's not treated as ground truth.


# val.pipeline 0.1.41

- **User-facing knob for `install_suggestions`.** Adds a new
  `toml_install_suggestions` argument to [val_pipeline()] and
  [val_prep_pipeline()] that plumbs through to
  [write_pipeline_toml()]. Default is `TRUE`, matching the 0.1.40
  install-Suggests-by-default behaviour; end users can now flip it to
  `FALSE` at the pipeline entry point without having to call
  `write_pipeline_toml()` by hand. (#148)

# val.pipeline 0.1.40

- **`write_pipeline_toml()` now emits `install_suggestions = true`
  per dep by default.** Every entry under `[project].dependencies` is
  rendered as an inline table `{ name = "pkg", install_suggestions =
  true }` so `rv` installs each package's Suggests when materializing
  the pipeline snapshot. This closes the gap left by the Layer A
  env-var work (#146) — with `NOT_CRAN=true` set for the covr run,
  test files gated on `testthat::skip_if_not_installed("someSuggest")`
  need their Suggests present in the pipeline library or they still
  silently skip. `rv` only supports `install_suggestions` as a
  per-dependency field (no top-level toggle exists in the rv schema
  — verified in `a2-ai/rv/src/config.rs`), so every entry gets the
  field set individually. Set the new `install_suggestions = FALSE`
  argument to opt back into the pre-0.1.39 bare-string dependency
  shape (kept for smoke-test fixtures / callers that don't want the
  install-time bloat). (#148)

# val.pipeline 0.1.39

- **Layer A covr env-var normalization**: `val_pkg()` now wraps the
  final `riskmetric::pkg_assess()` call — the one that may include
  `assess_covr_coverage` — in `withr::with_envvar()` seeded from a new
  `default: covr_env_vars:` block in `inst/config.yml`
  (`NOT_CRAN="true"`, `TESTTHAT="true"`,
  `_R_CHECK_FORCE_SUGGESTS_="false"`). Fixes the silent
  `testthat::skip_on_cran()` skips that were dropping large slices of a
  package's test suite from the covr run and pushing packages into
  the High `covr_coverage` bucket for reasons unrelated to real
  coverage. Extend the map without touching R code. Also scaffolds
  `NOT_ON_CRAN=""` and `RUN_SLOW_TESTS="false"` in the defaults so
  reviewers can flip on those common opt-in knobs without editing R
  code; both ship off. (#146, #152)

# val.pipeline 0.1.38

- CI: drop `macos-latest` from the `R-CMD-check.yaml` matrix. The
  `tomledit` 0.1.1 binary published for aarch64-apple-darwin has a
  broken extendr symbol that prevents load, and no newer release is
  available. Restore once tomledit ships a fixed binary. (#144)
- Fix `R CMD check` on ubuntu + windows (had been failing on `main`
  since 2026-08-11). Adds `ps` to `Suggests:` so the
  `requireNamespace("ps", ...)` call in `R/mem_watchdog.R` clears the
  \"'loadNamespace' or 'requireNamespace' call not declared from: 'ps'\"
  WARNING (fatal under CI's `error-on: "warning"`). Wraps five
  source-scan tests in `test-val_pkg.R`, `test-val_build.R`, and
  `test-reject_iteration.R` with the `system.file()` / `test_path()` /
  `skip_if_not(file.exists(...))` pattern (already used by
  `test-val-build-workers.R`) so they no longer error under R CMD check
  when `R/*.R` source is unavailable. Updates
  `test-bioc-remote-initial-metrics.R` test 1 to expect `NULL` — the
  `bioc_remote_initial_metrics:` key is intentionally commented out in
  `inst/config.yml`. (#144)

# val.pipeline 0.1.37

- Bump the minimum `{riskscore}` requirement to `>= 0.1.3` and pin the
  `Remotes:` entry to `pharmar/riskscore@latest` so `remotes` /
  `renv::restore()` pull from the branch that actually ships the
  compiled `assessed_latest` / `scored_latest` datasets. Refresh the
  stale `val_categorize()` docstring accordingly. (#142)

# val.pipeline 0.1.36

- Fix `update_opt_repos()` mangling PPM URLs whose slug encodes the
  snapshot date (e.g. `.../cran-r4.5-2026-07-21/latest`). The old
  nested `gsub()` swapped the slug's embedded date AND the `/latest`
  tail, producing broken paths like `.../cran-r4.5-2026-07-21/2026-07-21/src/contrib`.
  The rewrite now only touches the final path segment via
  `dirname()`/`basename()`, and treats any URL whose slug already
  encodes a date as authoritative (no-op). (#140)

# val.pipeline 0.1.35

- Summary report polish batch (#138):
  - **Run Metadata**: new `Elapsed time` row alongside the existing
    cumulative-runtime row. Sourced by parsing the first
    `=== val_build() @ <timestamp> ===` banner in
    `<val_dir>/val_pipeline.log` (written by `init_val_log()` at
    `val_build()` entry) and comparing to render time. Complements
    the cumulative row: for resumed multi-session runs, elapsed
    includes the calendar gap between sessions.
  - **Run Metadata**: `Metric package` row now includes the installed
    version, e.g. `riskmetric 0.2.5`. Sourced from
    `utils::packageVersion()` at render time.
  - **Run Metadata**: new `Pre-filter method` row inserted before
    `Metric package`, showing `riskscore <version>`. Omitted when
    riskscore isn't installed.
  - **Downgrade callout** in "Initial vs. final decision" upgraded
    from a bare bold-text block to a tinted bordered infographic
    card (pastel-orange) so it separates visibly from the section
    headers instead of blending in.
  - **Risk-level infographic cards**: new colored card strip
    (pastel green / yellow / red for Low / Medium / High) rendered
    under both `Final decision counts` (replacing the previous
    table entirely) and the `Packages` header. Each card shows
    count + percent in large font. Palette generalizes to future
    N-band schemes via a green->yellow->red gradient interpolated
    with `grDevices::colorRampPalette()`.
  - **Memory offenders**: spell out **RSS** = Resident Set Size on
    first use.
  - Passes new `log_file_path` param to the summary template so
    the elapsed-runtime calculation can find the log; populated
    automatically from `<input_dir>/val_pipeline.log` when present.

# val.pipeline 0.1.34

- **Parallel workers now reinstate both BiocManager and riskmetric
  shims.** The \`configure_bioc_repositories()\` shim -- installed by
  \`val_pipeline()\` / \`val_prep_pipeline()\` / \`val_build()\` at
  startup when \`VAL_PIPELINE_INTERNAL_BIOC=1\` is set -- rewrites
  \`BiocManager::repositories()\` via \`utils::assignInNamespace()\`
  and toggles \`options(BiocManager.check_repositories = FALSE)\`.
  Companion shim \`configure_riskmetric_offline()\` (also installed
  at the same three entry points, gated on
  \`VAL_PIPELINE_INTERNAL_RISKMETRIC\`) rewrites
  \`riskmetric::assess_reverse_dependencies.default\`,
  \`riskmetric::memoise_bioc_available\`, and
  \`riskmetric::pkg_bioc\`. All of these are session-scoped in-memory
  namespace mutations that do NOT survive the
  \`future::multisession\` boundary. On air-gapped / PPM-only hosts
  every worker booted with stock \`BiocManager\` /
  \`riskmetric\` namespaces, so downstream calls
  (\`riskmetric::assess_reverse_dependencies()\`,
  \`memoise_bioc_available()\`'s hard-coded \`read.dcf\` against
  \`https://bioconductor.org/packages/release/bioc/src/contrib/PACKAGES\`,
  etc.) failed with
  \`cannot open the connection to 'https://bioconductor.org/...'\`.
  Users reported 100s of Bioc-adjacent pkgs failing this way in a
  single parallel run. Env vars cross the boundary (OS inheritance)
  and both helpers are safely re-callable, so plain re-invocations
  inside the worker \`FUN\` body reinstate both shims. Serial mode
  was always fine. Same category of parent-session-state gap as
  #133's \`options(repos)\` fix. (#136)

# val.pipeline 0.1.33

- **Summary report PDF renders again.** The `decision-crosstab-
  downgrade-callout` chunk added in #131 wrote CSS
  `font-weight: 600` on the callout span. Pandoc translates that
  numeric weight into typst `weight: "600"` (a string) which
  typst rejects — it accepts integers 100-900 or the named weights
  `"thin"` / `"regular"` / `"semibold"` / `"bold"` / etc. HTML
  renders unaffected; only the typst PDF backend was broken.
  Switch to `font-weight: bold`, which Pandoc maps cleanly to
  `weight: "bold"` in typst and renders essentially the same in
  HTML. Adds a `format = "pdf"` regression test to
  `test-val_pipeline_report.R` since every existing test in that
  file explicitly requests `format = "html"` and never exercised
  the typst path. (#134)

# val.pipeline 0.1.32

- **Parallel workers now inherit `options(repos = opt_repos)`** (and
  `pkgType = "source"` when `ref == "source"`, plus `scipen` from
  the parent) from the parent session. `future::multisession` boots each worker in a fresh R
  process that does NOT inherit `options()`, so `get_repo_origin()` ->
  `getOption("repos")` inside the worker returned R's factory default
  `c(CRAN = "@CRAN@")`, no substring-match ever hit, and every
  parallel-assessed pkg was stamped `repos = "unknown"` on its
  `_meta.rds` bundle. That literal then leaked into the summary
  report's Run Metadata "Repositories" row and any downstream
  consumer of `qual_metadata$repos`. Re-apply both options inside the
  worker's option-priming block, alongside the existing
  `val.pipeline.verbose` / `val.pipeline.config_path` /
  `val.pipeline.log_file` / `val.pipeline.log_level` re-hydration.
  Serial runs (`workers = 1`) were never affected. (#132)
# val.pipeline 0.1.31

- Summary report **"Run Metadata" → "Repositories"** row now drops the
  literal `"unknown"` value returned by `get_repo_origin()` when a pkg's
  source URL fails to substring-match any entry in `getOption("repos")`
  at assessment time. Matches the existing NA-filter pattern already
  applied to `ref` / `metric_pkg` on the same row set. (#130)

# val.pipeline 0.1.30

- Summary report polish batch (#130):
  - **"Packages by Risk Category"** section heading trimmed to
    **"Packages"** — the subsection headings under it (Low / Medium
    / High) already convey the risk-tier grouping.
  - **New "Ripple effect" table** under "Initial vs. final decision"
    that ranks failing packages by the number of other packages
    they dragged down as a dep failure. Sourced by parsing
    `final_decision_reason_note` on every dep-downgraded row and
    counting occurrences per culprit; joined back against
    `qual_metadata` so each culprit's own decision + reason are
    shown alongside the ripple count.
  - **Coverage buckets** in Metric-Level Summaries now cleave at 65%,
    mirroring the Low-tier `covr_coverage` threshold from
    `inst/config.yml`. Old three-band split (50-79 / 80-94 / 95-100)
    is now four bands (50-64 / 65-79 / 80-94 / 95-100).
  - **"Ten slowest packages"** → **"Slowest packages"**; sources the
    top 50 rows and renders via `itable()` with a page-size dropdown
    (10 / 25 / 50 / 100), default 10.
  - **"Top 25 memory offenders"** → **"Memory offenders"**; same
    treatment — 50 rows sourced, default page 25.
  - **"Initial vs. final decision"**: the downgrade count now
    renders in a large-font callout above the crosstab so it's the
    first thing an operator sees.
  - **Package report "Has source control"**: when riskmetric couldn't
    parse a code-host URL and returned `"unknown"`, the pkg template
    now falls back to reading the sourced `DESCRIPTION`'s `URL:`
    field for a github/gitlab/bitbucket/codeberg/sr.ht/gitea link,
    mirroring the existing "Has bug reports url" fallback.
  - **Top-of-report runtime** is now **cumulative** (sum of
    per-package `assessment_runtime_mins`) so a run resumed across
    multiple sessions shows total work rather than the last
    session's wall clock. Formatted as `Hh MMm` (e.g. `41h 12m`).
  - **Runtime section**: total / mean / median / max are now
    formatted as `Hh MMm` where >= 1h, `Xm` for 1-59 min, `X.XX m`
    for sub-minute values.
  - **`val.pipeline` version(s)**: a new `val_pipeline_ver` field is
    persisted on each `_meta.rds` (from `utils::packageVersion()` at
    `val_pkg()` time). The summary report's meta table surfaces the
    distinct set of versions that produced the run -- resumed runs
    that spanned a package update will now show every version that
    contributed.

# val.pipeline 0.1.29

- Re-fixed the `<<-` scope bug in `val_finalize()`'s
  `reject_iteration()` convergence loop that was originally patched
  in `val_build()` via #103. When #101 extracted the loop from
  `val_build.R` into the new `val_finalize.R`, it carried the
  pre-existing `<<-` forward verbatim; #103 (in flight against the
  old `val_build.R` location) never touched the new copy, so the
  fix was never applied here. `<<-` inside a top-level function
  writes to the enclosing scope, leaving the local `failed` /
  `pkgs_df` bindings pinned at their iter-1 values -- turning the
  fixed-point loop into an infinite loop for any cohort whose
  iter-1 result differs from the seed. Guarded against a third
  re-introduction with a source-level test that scans every `.R`
  file in `R/` for the offending pattern. (#128)

# val.pipeline 0.1.28

- Pre-filtered already-assessed packages out of `val_build()`'s serial
  loop, mirroring the `!replace` skip the parallel branch has had
  since #91. Previously, resuming a mostly-complete run (e.g. 1602 of
  1606 packages already on disk) meant emitting a `val_msg` line per
  cached package before the loop even reached the four unassessed
  ones — clogging the log with thousands of "already assessed" lines.
  The serial branch now reads `_meta.rds` for each cached package
  once up front, replays any cached failure (`decision != decisions[1]`,
  ignoring NAs) into `dont_run` / `failed_pkgs` so subsequent uncached
  packages still get dep-skipped correctly, and iterates only the
  filtered `todo` list. The in-loop dep-skip check for newly-assessed
  failures is unchanged. (#126)

# val.pipeline 0.1.27

- Guarded `val_build()`'s serial-branch dep-propagation check against
  `NA` decisions from `val_pkg()`. Previously, when `val_decision()`'s
  rule ladder produced no category (typically a `remote_only` or Bioc
  package with a shrunken viable-metric set), `pkg_meta$decision` came
  back `NA`, the `!= decisions[1]` comparison evaluated to `NA`, and
  the whole run took an "missing value where TRUE/FALSE needed" halt
  near the tail. `val_pkg()` now stashes an `assessment_gaps` list on
  the meta bundle (viable metrics, per-metric categories, primary /
  secondary risk categories, synthesized note) and flips
  `decision_reason` to `"Incomplete Assessment"`. Decisions themselves
  stay `NA` — no silent coercion to a tier. `val_finalize()` preserves
  the diagnostic as a list-col on `qual_metadata`, and the summary
  report gained a "Packages with incomplete assessment" table plus a
  per-package appendix section with viable metrics, per-metric
  categories, and cross-joined actual scores from `qual_assessments`.
  (#124)

# val.pipeline 0.1.26

- Added per-package memory watchdog observability so `workers` on
  `val_pipeline()` / `val_build()` can be sized against real per-package
  peak RSS instead of eyeballed. When `mem_watchdog = TRUE` (new
  default) `val_build()` samples each package's high-water RSS (via
  `/proc/<pid>/status` `VmHWM` on Linux, with a `ps::ps_memory_info()`
  fallback) and appends one row to `<val_dir>/mem_watchdog.tsv`.
  `val_finalize()` prints p50 / p95 / max, the 10 heaviest packages,
  and — on Linux — a suggested `workers` value for the next run keyed
  on `MemAvailable` / p95. The summary report gained a "Top 25 memory
  offenders" section that renders the same TSV. Cached / dep-skip
  packages are excluded from the sample because no real work was done.
  (#122)
- Reordered the parallel-mode package queue so heavy packages spread
  across workers instead of piling up at the tail. `val_build(workers
  > 1)` now round-robin restripes the `todo` list — sorted by prior
  `mem_watchdog.tsv` peak RSS when available, otherwise by the input
  order — and dispatches one future per package (`future.scheduling =
  1L`) so workers pick up alternating heavy/light packages rather than
  chunks of consecutive tail-heavies. Serial mode (`workers = 1`) is
  unchanged so its dep-skip short-circuit still fires. (#122)

# val.pipeline 0.1.25

- Caught errors thrown by `val_pkg()` inside `val_build()`'s per-package
  assessment loop so a single package blowing up (e.g. a
  `build_decisions_df()` "None of the metrics in 'rule_lst' are viable"
  error when a package's viable-metric set collapses) no longer cancels
  the whole multi-hour run. The erroring package is marked with the
  highest risk tier, `decision_reason = "Error"`, and the error message
  goes into `decision_reason_note`; the failure is also logged at
  `minimal` verbosity so it surfaces regardless of `verbose` setting.
  Downstream `reject_iteration()` propagates the failure to dependents
  as it would for any other failed package. The summary report gained a
  new "Packages that errored during assessment" section listing every
  such package + its captured error text. (#116)

# val.pipeline 0.1.25

- Hardened `val_build()`'s parallel branch (`workers > 1`) against
  silent early exits. `future.apply::future_mapply()` used the
  default `future.scheduling`, which pre-partitions all pending
  packages into ~`workers`-many chunks; a single worker's R
  subprocess dying mid-chunk (OOM-killed, segfault, walltime hit,
  etc.) dropped every remaining package in that chunk on the floor
  and, depending on the future version, either raised a
  `FutureError` that never surfaced to `val_build()` or was absorbed
  silently, leaving the parent to hand a truncated on-disk set to
  `val_finalize()` and produce a partial `qual_metadata.rds` with
  the Workbench job showing success. Two fixes:
  1. Pin `future.scheduling = 1L` so a worker death loses at most
     the single package it was actively assessing; healthy workers
     pick up the rest.
  2. After `future_mapply` returns, recount `_meta.rds` files on
     disk against what was dispatched; if fewer landed than
     expected, stop with a clear error so `val_finalize()` doesn't
     collate a truncated cohort silently. Re-run with `replace =
     FALSE` to resume.
  (#120)

# val.pipeline 0.1.24

- Added optional reverse-dependency expansion to `resolve_pkg_tree()`,
  surfaced via new `rev_deps` / `rev_deps_recursive` args on
  `val_pipeline()`, `val_prep_pipeline()`, and `val_build()`. When
  set, the seed set is first expanded with the reverse deps of the
  supplied packages (via `tools::package_dependencies(reverse = TRUE)`)
  before forward-dep expansion runs, so re-assessing a remediated
  package (e.g. `duckdb` flipped from High to Low) can also re-run
  everything that previously failed *because* it depended on that
  package. Defaults preserve pre-existing behaviour (`rev_deps = NULL`,
  `rev_deps_recursive = FALSE`). (#105)

# val.pipeline 0.1.23

- Narrowed `reject_iteration()`'s Pre-Approved carve-out so a package on
  the config `approved_pkgs` list is protected from dep-driven downgrade
  only when none of its runtime deps actually failed. When a Pre-Approved
  pkg's dep DID fail (PPM can no longer serve it because its install
  closure is broken) the pkg is downgraded to the reject category with
  `final_decision_reason = "Pre-Approved (dep failed)"` so an operator
  can distinguish it from an ordinary dep-driven downgrade and either
  fix the upstream dep or drop the pkg from `approved_pkgs`.
  `val_build()`'s pre-skip branch was updated to emit the same reason
  string when the pre-skipped pkg is on `approved_pkgs`, and
  `val_pipeline_report()`'s summary gained a small section listing every
  Pre-Approved-with-failed-dep pkg + the failing direct dep(s), so the
  actionable list surfaces at the top of the report. (#110)
- Fixed `decision_reason_note` listing the recursive Suggests closure
  intersected with all failed packages (~100 unrelated transitive pkgs)
  instead of the actual DESCRIPTION-level dep(s) that triggered the
  downgrade. `val_pkg()` and `val_build()`'s dep-skip branch now capture
  direct (non-recursive) `depends_direct` / `suggests_direct` alongside
  the existing recursive fields; both the dep-skip branch and
  `reject_iteration()` populate the note from the direct set. Legacy
  meta bundles without the new fields fall back to the recursive fields.
  (#107)
- Widened `rip_cats_by_pkg()`'s `pass_primary` bypass so a package on the
  `pass_primary` allow-list is exempted from the `downloads_1yr` primary
  metric when its downloads sit below the *Low* tier's floor (upper
  `downloads_1yr` bound, e.g. 200k under the default config), not just
  below the *High* tier's ceiling (lower bound, e.g. 80k). Packages in
  the Medium tier would still land in Medium on `downloads_1yr` alone
  and fail `accept_cats: Low`, defeating the point of the bypass. (#112)
- Added the package's source repo URL as a bullet in the Context section
  of the per-package report (`inst/report/package/pkg_template.qmd`).
  The URL's basename is the snapshot repo name in Posit Package Manager,
  so reviewers can trace a rendered report back to a specific PPM
  snapshot without cross-referencing `qual_metadata.rds`. (#114)

# val.pipeline 0.1.21

- Extracted the collation + wrap-up tail of `val_build()` into a new
  exported [`val_finalize()`](R/val_finalize.R), so a run whose
  per-package assessment loop finishes but whose collation step
  hangs / OOMs / gets killed can now be recovered from a fresh R
  session with `val_finalize("<val_dir>")` — or, preferably, with a
  `val_prep` object in hand, `val_finalize(prep = prep)`. Both
  `val_build()` and `val_pipeline()` gained a `finalize = TRUE`
  argument that controls whether the tail runs inline (default;
  matches pre-0.1.21 behaviour) or whether the caller wants to
  invoke `val_finalize()` themselves in a follow-up step. To keep
  the source of truth for the recovery workflow in one place,
  `val_prep_pipeline()`'s return was extended with the three
  caller-facing args (`deps`, `config_path`, `verbose`) that
  weren't already on it, so `val_finalize(prep = prep)` is a
  one-liner and `val_pipeline()` itself returns invisibly `NULL`.
  (#101)
- Fixed a latent scope bug in `val_build()`'s dep-driven decision
  propagation loop that caused it to spin forever on any cohort
  large enough to actually need >1 iteration of `reject_iteration()`.
  The loop's body used `<<-` to update its `failed` and `pkgs_df`
  locals, which — inside a top-level function — skipped the local
  frame and assigned into the enclosing lexical scope, leaving both
  locals stuck at their iter-1 values and the loop condition
  perpetually `TRUE`. Latent because small dev cohorts converge on
  iteration 1, so the bug never fired in tests; a 1422-package run
  surfaced it via `val_finalize()`'s new propagation messaging.
  Changed both to local `<-`; added a regression test that runs a
  3-package dependency chain (X ← Y ← Z) requiring exactly 2
  propagation passes and hard-caps the loop at 10 iterations so a
  re-introduction of the bug fails testthat instead of hanging CI.
  (#103)

# val.pipeline 0.1.20

- `val_build()` and `val_pipeline()` now mirror the parent R session's
  `.libPaths()` into the `R_LIBS_SITE` environment variable for the
  duration of the assessment loop, so subprocesses spawned by
  riskmetric — `rcmdcheck::rcmdcheck()` (for the `r_cmd_check`
  metric), `covr::package_coverage()` (for `covr_coverage`), etc. —
  see the same library search order as the parent. A fresh R
  subprocess does NOT inherit an interactive `.libPaths()` from the
  parent, so before this fix, users who pointed `.libPaths()` at an
  rv-provisioned library (the recommended flow after PR #67 introduced
  `write_pipeline_toml()`) saw ~65% of packages come back with
  `r_cmd_check_errors` and `r_cmd_check_warnings` as `NA` because
  R CMD check couldn't find their deps. The mirror is restored on
  function exit via `withr::local_envvar()`. Controlled by a new
  `propagate_libpaths` argument on both `val_build()` and
  `val_pipeline()` (default `TRUE`, sourced from
  `getOption("val.pipeline.propagate_libpaths", TRUE)` so it can be
  disabled session-wide). (#99)

# val.pipeline 0.1.19

- Add `val_timings_summary()`: exported standalone analysis helper for
  the `timings.csv` file that `val_build()` writes per run (#87).
  Accepts a path to `timings.csv`, a run directory that contains one,
  or a data frame already read into R, and returns a list of three
  tibbles — `per_pkg` (total wall time per package), `per_phase`
  (total / mean / median / p95 / n_pkgs across packages), and `wide`
  (one row per package, one column per phase). Prints a compact
  top-N summary at the console when called interactively. Independent
  of `val_pipeline()` state: point it at any completed run's
  artifacts and go. (#97)

# val.pipeline 0.1.18

- Fix `val_pipeline_report()` failing on air-gapped PPM hosts with
  `Error running quarto CLI from R. ... unable to access index for
  repository https://bioconductor.org/packages/.../PACKAGES`. The
  Quarto CLI spawns a child Rscript for the render, which was
  inheriting the parent session's renv activation (`RENV_PROJECT`
  and/or an activated `renv/activate.R`) and trying to
  `Bootstrapping renv 1.x.x` -> `Download renv` from every configured
  repo. On offline hosts the BioC repo download failed and took the
  whole report render down. `val_pipeline_report()` now wraps the
  `quarto::quarto_render()` call in `withr::with_envvar()` that sets
  `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` and unsets `RENV_PROJECT`
  for the child process. The Quarto child still inherits the
  parent's `.libPaths()`, so val.pipeline + its imports remain
  visible from inside the .qmd; we're just skipping the useless
  bootstrap step. (#95)

# val.pipeline 0.1.17

- Export `write_qualified_pkg_lists()` so operators can regenerate the
  per-source `qualified-<src>.txt` / `blocklist-<src>.txt` files
  ad-hoc from an existing `qual_metadata.rds` (e.g. after tweaking
  `blocklist_sources` in the config), without re-running the pipeline.
  (#93)
- Emit `blocklist-<source>.txt` files instead of `qualified-<source>.txt`
  for repo sources that Posit Package Manager can only curate via a
  blocklist (currently Bioconductor). The blocklist file lists every
  assessed package from that source whose `final_decision` is NOT the
  qualified decision (e.g. anything not `"Low"`), so PPM can mirror the
  full upstream source and exclude the unqualified ones. Which sources
  get blocklist treatment is configurable via the new
  `blocklist_sources` key under `default:` in `inst/config.yml`
  (defaults to `[BioC]`); listing `github` there gives the same
  treatment to the github bucket. `write_qualified_pkg_lists()` gains a
  matching `blocklist_sources` argument. Empty blocklist files are
  still written on purpose so downstream PPM provisioning has a stable
  filename to point at (an empty blocklist means "mirror this source,
  block nothing"). Unknown-source rows always go to
  `qualified-NA.txt` and are never inverted, even if `"NA"` is passed
  in `blocklist_sources`. (#93)

# val.pipeline 0.1.16

- Stop accumulating per-package `meta_list` bundles in memory during
  `val_build()`. Prior behaviour built a named list `pkg_bundles` of
  every assessed pkg's full meta (recursive `depends`/`suggests`, a
  full `R.Version()` dump under `sys_info`, plus `rev_deps` and
  `timings`), and in parallel mode also shipped each bundle back
  through the `future` IPC channel -- easily 1-3 GB on full CRAN +
  BioC cohorts (~6000 pkgs) and a primary driver of the OOM crashes
  that forced users to restart 40-hour runs 2-3 times. `val_build()`
  now discards worker returns (`val_pkg()` already writes
  `_meta.rds` to disk) and streams collation from those files:
  `qual_metadata0.rds` (`pkgs_df0`) and `timings.csv` are built in a
  single one-bundle-at-a-time disk pass, cutting collation-phase
  peak memory from O(all bundles) to O(one bundle + derived rows).
  `assessment_bundle` is also `rm()`+`gc()`'d immediately after
  `qual_assessments.rds` is saved so it no longer overlaps
  `pkgs_df0` in memory. No public API change; downstream artifacts
  (`qual_assessments.rds`, `qual_metadata0.rds`, `qual_metadata.rds`,
  per-pkg `_meta.rds`, `timings.csv`) are byte-equivalent to before.
  Mid-run crash recovery is unchanged and free: rerun `val_pipeline()`
  with the same args and the cached branch of `assess_one()`
  short-circuits every previously-assessed pkg via its `_meta.rds`
  file. When `workers > 1`, the parallel branch now *also*
  pre-filters those already-assessed pkgs out of the dispatch list
  entirely (was: dispatch every pkg and let each worker hit the
  cached branch, paying a full future/IPC round-trip plus cached
  val_msg lines for zero real work). Restarting a crashed 40-hr run
  now only re-dispatches the pkgs that still need real work. Serial
  mode (`workers = 1`) still walks cached pkgs so it can accumulate
  `dont_run` / `failed_pkgs` state for its dep-skip short-circuit.
  (#91)

# val.pipeline 0.1.15

- Decouple `val_date` from the CRAN URL rewrite. New logical argument
  `freeze_opt_repos` on `val_pipeline()` and `val_prep_pipeline()`
  (default `FALSE`, preserves the pre-existing behaviour where
  `update_opt_repos()` rewrites the config's CRAN URL to match
  `val_date`). When `TRUE`, `update_opt_repos()` is skipped and the
  config's `opt_repos` is used verbatim, so `val_date` can drift from
  the frozen PPM snapshot without silently changing which packages
  the pipeline pulls. Useful when the org has pinned CRAN to a
  specific date in `inst/config.yml` but wants each run's output
  folder (`R_<ver>/<YYYYMMDD>/`) to reflect the date the analysis was
  actually executed. `val_date` still governs the output directory
  name and every `val_date` field written to metadata -- only the
  URL rewrite is gated off. (#89)

# val.pipeline 0.1.14

- Per-package timing instrumentation inside `val_pkg()`. A new
  `val_time_block(label, expr)` helper fences the expensive blocks
  (`download`, `untar`, `assess_initial`, `assess_final`, `decision`,
  `report`), emits an `[timing] <label>: N.NNs` line at the `"normal"`
  tier, and accumulates the elapsed seconds in a per-package session
  option that `val_pkg()` attaches to the returned `meta_list` as
  `$timings`. `val_build()` aggregates every package's `$timings`
  post-run into a long-format `timings.csv` under `val_dir`
  (columns: `pkg`, `ver`, `phase`, `seconds`) so we can profile
  where a multi-hour run's time actually goes. Timings are recorded
  even when the fenced expression errors, so failures still surface
  "we spent N seconds here before it blew up". (#87)
- Single-file run log. `val_build()` now teees every `val_msg()` /
  `val_print()` / `val_pkg_summary_line()` call to
  `val_dir/val_pipeline.log`. Console tier and log-file tier are
  decoupled -- set `options(val.pipeline.log_level = "verbose")` to
  keep a rich on-disk record while running `verbose = "minimal"` at
  the console. The parallel branch of `val_build()` propagates
  `val.pipeline.log_file` and `val.pipeline.log_level` into workers
  via the same options-in-worker plumbing added in #80, so every
  worker appends to the same log. POSIX O_APPEND is atomic for
  line-sized writes and NFSv4.2 upholds it, so concurrent worker
  appends interleave cleanly. (#87)

# val.pipeline 0.1.13

- Fix two `val_build(workers > 1)` bugs surfaced by a real 8-worker run:
  - `quarto::quarto_render()` was blowing up with
    `Error running quarto CLI from R` in every worker but one.
    `riskreports::package_report()` copies its template files into
    `options("riskreports_output_dir")`, renames one file to a
    pkg-specific `prefix_output`, and then removes the leftovers.
    `val_pkg()` pointed that option at a single shared `reports/`
    directory, so 8 concurrent workers raced on the mid-flight
    copy/rename/remove sequence and one worker's `quarto_render()`
    tripped over another's cleanup. `val_pkg()` now renders into a
    per-package scratch dir (`reports/.render_<pkg>_v<ver>/`), then
    copies the produced output(s) up into `reports/` and cleans the
    scratch dir on exit. Final layout is unchanged.
  - `verbose = "minimal"` was silently reverting to `"normal"` inside
    workers. `future::multisession` boots each worker in a fresh R
    session that does *not* inherit the parent's `options()`, so the
    `val.pipeline.verbose` option `apply_verbose()` set in the parent
    never reached `val_msg()` inside the worker. The parallel branch
    of `val_build()` now captures the resolved tier
    (`getOption("val.pipeline.verbose")`) plus the user-supplied
    `val.pipeline.config_path` and re-applies both at the top of every
    `future_mapply()` task. (#80)
- Config + lockfile drift picked up while shaking down the parallel
  build: bump the pinned CRAN snapshot from `2026-06-21` to
  `2026-07-21`, tighten the `downloads_1yr` Medium/Low cutoff from
  240k to 200k, and comment out the `bioc_remote_initial_metrics`
  block (the whitelist is currently blocking valid Bioc assessments
  on hosts with outbound access; will revisit under a dedicated
  issue). Adds `future`, `future.apply`, `globals`, `listenv`,
  `parallelly`, and `codetools` to `renv.lock` so the parallel
  branch of `val_build()` boots cleanly under `renv::restore()`.
- `dev/dev_pipeline.R`: swap `devtools::load_all()` for
  `devtools::install(quick = TRUE) + library(val.pipeline)` so
  `workers > 1` doesn't hit `FutureLaunchError: there is no package
  called 'val.pipeline'` — `future::multisession` workers boot with
  the plain `.libPaths()` and can't see a `load_all()`-only shadow
  namespace.

# val.pipeline 0.1.12

- Add a `workers` argument to `val_build()` (and threaded through
  `val_pipeline()`). `workers = 1L` (default) preserves the original
  serial behaviour and dep-skip short-circuiting. Values greater than
  `1` fan the per-package assessment loop out via
  `future.apply::future_mapply()` under a `future::multisession` plan;
  the dep-skip short-circuit is disabled in parallel mode because its
  state cannot cross a worker boundary, but downstream final-decision
  propagation in `val_decision()` still applies the worst-of-any-dep
  rule, so package-report accuracy is unchanged. `{future}` and
  `{future.apply}` moved to `Suggests` and are only required when
  `workers > 1`. (#80)
  
# val.pipeline 0.1.11

- Hotfix: strip a stray `data:image/png;base64,...` blob that had been
  accidentally glued onto the maintainer's `ORCID` field in
  `Authors@R`. R CMD check on Ubuntu was failing every downstream PR
  with `checking DESCRIPTION meta-information ... ERROR / Malformed
  Authors@R field:`. The blob was introduced in `main` via #84 and had
  begun propagating to every branch that merged main. No other
  change. (#85)

# val.pipeline 0.1.10

- Restrict the `pkg_bioc_remote` initial-assessment pass to a
  config-defined whitelist of metrics that actually produce valid
  results after `configure_riskmetric_offline_if_requested()` has
  installed its four shims. The `has_news`, `remote_checks`,
  `news_current`, `has_vignettes`, `has_maintainer`, `bugs_status`,
  `has_source_control`, `has_bug_reports_url`, and `license` metrics
  all scrape `bioconductor.org` via `x$web_html` /
  `x$repo_base_url` and return `<pkg_metric_error: Failed to connect
  to bioconductor.org port 443: Connection refused>` on air-gapped
  PPM hosts (whose BioC mirrors typically do not serve the `/html/`,
  `/news/`, `/checkResults/` trees). Running them anyway just fills
  the initial score frame with NAs.
- Added `default: bioc_remote_initial_metrics:` config knob shipped
  with `c("assess_reverse_dependencies", "assess_dependencies")` —
  the two riskmetric assessments that go through
  `utils::available.packages()` (via Shim 1) and DESCRIPTION fields
  cached in `memoise_bioc_available()` (via Shim 2) respectively, so
  neither depends on scraping the BioC HTML tree. Set to `~` (YAML
  null) or delete the key to run every metric
  `riskmetric::all_assessments()` returns — the pre-0.1.10
  behaviour, only appropriate on hosts with real outbound access to
  `bioconductor.org`.
- CRAN packages, GitHub packages, and BioC packages using
  `bioc_initial_ref: install|source|skip` are unaffected. (#84)


# val.pipeline 0.1.8

- `configure_riskmetric_offline()` now installs four in-session shims
  on `riskmetric` to make BioC assessments work on air-gapped hosts,
  replacing the two shims previously introduced in this branch.
  Upstream fix proposed at `pharmaR/riskmetric#402`.
  - Shim 1 (unchanged): swaps
    `riskmetric:::assess_reverse_dependencies.default()` for a version
    that computes reverse dependencies via `utils::available.packages()`
    + `tools::dependsOnPkgs()` instead of
    `devtools::revdep(bioconductor = TRUE)` (which reads a `VIEWS`
    file the internal PPM does not serve).
  - Shim 2 replaces `riskmetric:::memoise_bioc_available()`'s
    hard-coded
    `read.dcf(url("https://bioconductor.org/packages/release/bioc/src/contrib/PACKAGES"))`
    lookup with a `utils::available.packages()` call. The BioC-repo
    resolver now handles setups where the BioC PPM is exposed via
    `options("repos")` without a `BioC*` name; callers can also
    override detection explicitly via
    `options(val.pipeline.bioc_repos = ...)` or
    `Sys.setenv(VAL_PIPELINE_BIOC_REPOS = "<comma-separated URLs>")`.
  - Shim 3 wraps `riskmetric:::is_available_cran()` so a package that
    is also present in `memoise_bioc_available()` no longer wins the
    CRAN check, restoring `pkg_bioc_remote` classification for BioC
    packages served alongside CRAN in `options("repos")` (fixes
    `pkg_ref("BiocGenerics")` being classified as `pkg_cran_remote`).
  - Shim 4 replaces `riskmetric:::pkg_bioc()` so the resulting
    `pkg_bioc_remote` reference carries the internal PPM BioC URL in
    `x$repo` / `x$repo_base_url` instead of the released code's
    hard-coded `https://bioconductor.org/packages/release/bioc`.
    Fixes a family of `pkg_assess()` metrics (`has_news`,
    `remote_checks`, `news_current`, `has_vignettes`, `has_maintainer`,
    `bugs_status`, `has_source_control`, `has_bug_reports_url`,
    `license`, ...) failing with
    `Failed to connect to bioconductor.org port 443: Connection refused`
    because they scrape `x$web_html` derived from `x$repo_base_url`.
  (#81)

# val.pipeline 0.1.7

- `val_pkg()` now prefers a disk-only reference for the initial
  assessment of Bioconductor packages instead of scraping
  `bioconductor.org` via `pkg_bioc_remote`. On air-gapped Posit
  Package Manager mirrors that broken scrape wiped out roughly half a
  dozen primary metrics (`has_vignettes`, `has_news`, `license`,
  `has_maintainer`, `has_bug_reports_url`, `has_source_control`) and
  effectively forced `covr_coverage` to run on every BioC package,
  defeating the point of the initial pass.
  - The new fallback order for BioC packages is: `pkg_install` (when
    the pkg is in `.libPaths()[1]`) -> `pkg_source` (when the tarball
    has been untarred) -> skip.
  - A new `default: bioc_initial_ref:` config knob controls the order,
    with allowed values `install` (default), `source`, `remote`
    (legacy scrape behaviour) and `skip` (no initial pass; final
    `pkg_source` assessment always includes `covr_coverage`).
  - `workable_assessments(source_ref = ...)` now records the actual
    provenance of the initial pass (`install`, `source`, or `remote`)
    rather than always claiming `"remote"`, so downstream reports no
    longer misrepresent where BioC metrics came from.
  - CRAN / GitHub packages are unaffected: the initial pass continues
    to run as `pkg_cran_remote`. (#82)

# val.pipeline 0.1.6

- Add `configure_bioc_repositories()` and
  `configure_bioc_repositories_if_requested()` — helpers for
  air-gapped Posit Package Manager environments where
  `BiocManager::repositories()` still emits its five hard-coded
  public `bioconductor.org` URLs alongside the internal repos on
  `options("repos")`, causing downstream `riskmetric` metrics
  (`assess_reverse_dependencies()`, remote checks, ...) to fail
  with "Bioconductor version cannot be validated; no internet
  connection?" or `cannot open the connection to
  'https://bioconductor.org/packages/.../VIEWS'`.
- The helper installs an in-session shim on
  `BiocManager::repositories()` so it returns *only* the caller's
  `options("repos")` value, and sets
  `options(BiocManager.check_repositories = FALSE)`. When the
  caller's repo vector uses a single flat `BioC` entry (typical
  PPM layout), any missing `BioC*` aliases (`BioCsoft`, `BioCann`,
  `BioCexp`, `BioCworkflows`, `BioCbooks`) are auto-populated with
  the same URL so downstream lookups like
  `BiocManager::repositories()[["BioCsoft"]]` (used inside
  `riskmetric`) don't blow up with `subscript out of bounds`.
- The entry points (`val_pipeline()`, `val_build()`,
  `val_prep_pipeline()`) call
  `configure_bioc_repositories_if_requested()` at startup so users
  can opt in with the environment variable
  `VAL_PIPELINE_INTERNAL_BIOC=1` — no code change required and no
  effect for public-network users.
- Add `configure_riskmetric_offline()` and its env-var-gated
  wrapper `configure_riskmetric_offline_if_requested()`. Even with
  the BiocManager shim in place,
  `riskmetric::assess_reverse_dependencies.default()` calls
  `devtools::revdep(x$name, bioconductor = TRUE)`, which in turn
  calls `devtools:::bioc_packages()` — a helper that
  unconditionally reads a `VIEWS` file from
  `BiocManager::repositories()[["BioCsoft"]]`. PPM's aggregated
  BioC snapshot is served at `<repo>/src/contrib/PACKAGES`; there
  is no `<repo>/VIEWS` at the mirror root, so the read fails and
  the reverse-dependencies metric becomes a `pkg_metric_error`
  (rendered as `"unknown"` in the package report). The new
  helper installs an in-session override for
  `riskmetric:::assess_reverse_dependencies.default` that computes
  the reverse-dependency list from `utils::available.packages()`
  directly (which reads from `options("repos")` — the internal
  PPM CRAN + BioC snapshots) with no `VIEWS` file required.
- The entry points now also call
  `configure_riskmetric_offline_if_requested()` at startup, so the
  same `VAL_PIPELINE_INTERNAL_BIOC=1` opt-in enables both shims.
# val.pipeline 0.1.5

More polish for the individual package report
(`inst/report/package/pkg_template.qmd`):

- Drop the standalone `## Code coverage` section — `Code coverage`
  is now a row in the metric table (next to `R CMD check`).
- Move `R CMD check`, `Code coverage`, and `Remote checks` up in
  the metric table so the trio sits below `Dependencies` and
  above `Has news`.
- Guard the Dependencies-section prep against packages with an
  empty character-vector dependencies metric (e.g. `ADGofTest`)
  that previously halted the render with
  `data.frame(): arguments imply differing number of rows: 0, 1`.
- Drop the standalone `## Code checks` section — R CMD check and
  Remote checks are now rows in the metric table.
- Fold Downloads 1yr, Reverse dependencies, and License into the
  metric table (retiring the separate summary-card table for
  non-HTML output).
- Move Origin to the Context section as a bullet.
- Reorder the metric table: `Downloads 1yr` and
  `Reverse dependencies` at the top; `R CMD check` and
  `Remote checks` paired inside the existing groupings;
  `License` pinned above `Has maintainer` at the bottom.

# val.pipeline 0.1.4

Polish and bug fixes for the individual package report PDF
(`inst/report/package/pkg_template.qmd`):

- Fix reverse-dependencies field showing a Bioconductor warning
  instead of a count.
- Fix `data.frame(... check.names = FALSE): row names contain
  missing values` (both at `summary_table()` input and for exports
  whose names had leading underscores / stray quote characters).
- Fix `cat(... covr_coverage ...)` list-argument error.
- Populate the Context section with `val_date` and `val_dir`
  (thread `val_dir` through `R/val_pkg.R`).
- Surface R CMD check error / warning text under `## Code checks`,
  with a fallback message when only counts were captured.
- Indent R CMD check and Remote checks lines (and their bullets /
  error / warning blocks) into a Markdown blockquote.
- Reorder the summary metric table: `Dependencies` at the top;
  `Has news` / `News current`, `Exported namespace` / `Export help`,
  and `Has examples` / `Has vignettes` paired; the URL-returning
  fields (`Has website`, `Has source control`, `Has bug reports url`)
  grouped with `Bugs status` next to `Has bug reports url`;
  `Has maintainer` at the bottom.
- Richer summary-card values: real vignette count, `Export help`
  as `N / M (P%)`, `Bugs status` rounded to one decimal,
  `Size codebase` pretty-printed, `Has website` and
  `Has bug reports url` show the actual URL(s), and `Dependencies`
  shows a real count (no longer coerced to `"Yes"` when equal to 1).
- Drop the redundant `## License` section (already in the
  summary card table).
- Add `## Dependencies` and `## Reverse dependencies` sections
  after `## Code checks`. Dependencies is rendered as a table
  with `Package` and `Type` columns.
- Guard against `params$val_date = NULL` (dropped the fragile
  YAML `date` / `date-format` fields that were breaking
  `quarto::quarto_render()`).


# val.pipeline 0.1.3

- **Pruned `pass_primary` in `inst/config.yml`**: the bypass list is only
  meant for packages that would otherwise fail the `downloads_1yr` primary
  metric (< 80,000 annual downloads). Ran a cranlogs audit against every
  entry and removed the 391 packages with `>= 80,000` annual downloads,
  taking the list from 575 to 184 entries. Packages with 0 reported
  downloads (base R, Bioconductor-only, GitHub-only, org-internal) were
  kept — the bypass is exactly what they need. Also updated the block's
  header comment to spell out the `< 80,000/yr` intent.

- **Guard `rip_cats_by_pkg()` bypass on live download count**: the
  bypass branch that drops `downloads_1yr` from the primary metric set
  now additionally requires the package's own `downloads_1yr` to be
  `NA` or `< min_dwnld_bound`, where `min_dwnld_bound` is the lowest
  boundary of the `downloads_1yr` primary rule, parsed out of `dec_df`
  via `to_the_limit()` at call time so it stays in sync with the
  config. This keeps the bypass targeted at packages that actually
  need it — a package on `pass_primary` that has since crossed the
  threshold now takes the normal primary path and passes on its own
  merits.

# val.pipeline 0.1.2

- **Individual package report PDF fixes**: hardened
  `inst/report/package/pkg_template.qmd` against three failure modes
  seen in real reports: (1) the "Reverse dependencies" cell in the
  summary card table sometimes rendered an error/warning string (e.g.
  "Bioconductor version cannot be validated...") instead of a count —
  it is now coerced to a length via `length()` when the metric is a
  character vector of package names, and shown as "unknown" for
  `pkg_metric_error` / `NULL` values; (2) `data.frame(... check.names
  = FALSE): row names contain missing values` right below that table
  — every card column is now forced to a single unnamed character
  scalar with explicit `row.names = NULL`; (3) `cat(... covr_coverage
  ...): argument 2 (type 'list') cannot be handled by 'cat'` in the
  "Code coverage" section — the chunk now extracts a scalar from the
  `list(totalcoverage=, filecoverage=)` structure, formats numeric
  values as a percentage, and skips the section entirely when no
  usable coverage value is available.

# val.pipeline 0.1.1

- **Optional local repo in `pipeline.toml`**: `write_pipeline_toml()` and
  `val_prep_pipeline()` now accept a `local_repo` / `toml_local_repo`
  argument. When supplied, the given URL is prepended at position 1 of
  the emitted toml's `[project].repositories` array (so `rv` reaches it
  first while processing the toml) without leaking into `opt_repos`,
  which continues to drive the assessment itself. Accepts either an
  unnamed `character(1)` (aliased as `"local"`) or a named
  `character(1)` where the name becomes the alias (e.g.
  `c(local = "https://...")`). Defaults to `NULL` — no change to
  existing behavior.

# val.pipeline 0.1.0

- **User-supplied `config.yml`**: `val_pipeline()`, `val_prep_pipeline()`,
  and `val_build()` now accept a `config_path` argument. When supplied,
  every `pull_config()` call made during the run reads from that file
  instead of the `config.yml` bundled with the package, and
  `val_build()` copies the same file into `val_dir` for record keeping.
  Under the hood the entry points set the session option
  `val.pipeline.config_path` (restored on exit) so any internal
  `pull_config()` — no matter how deep in the call graph — picks up the
  override. Passing `config_path = NULL` (the default) preserves the
  previous behavior and uses the bundled config.

- **CI fix (follow-up)**: address the four `R-CMD-check` warnings that
  were failing the workflow *after* `renv::restore()` succeeded on
  `ubuntu-latest` and `windows-latest`:
  - Escaped `\u2014` in the `val_msg()` roxygen block (which R was
    interpreting as an unknown Rd macro `\u`) and replaced non-ASCII
    em-dashes / right-arrow characters in `R/utils.R` with plain
    ASCII equivalents.
  - Declared `withr` under `Suggests` in `DESCRIPTION` (used by the
    `test-write_pipeline_toml` and `test-write_qualified_pkg_lists`
    test files).
  - Marked `macos-latest` as `continue-on-error: true` in the workflow
    matrix: the CRAN/PPM binary of `tomledit` 0.1.1 for
    `aarch64-apple-darwin` fails to load with
    `symbol not found in flat namespace '_R_init_tomledit_extendr'`
    (an upstream `extendr` symbol mismatch). Revisit once tomledit
    ships a fixed binary.

- **CI fix**: refresh `renv.lock` so `R-CMD-check` passes on
`ubuntu-latest` and `windows-latest` again. Since ~Feb 2026 the
workflow failed because 100+ per-package `Repository` fields were
hard-coded to `packagemanager.posit.co/cran/__linux__/rhel9/...`
URLs and the top-level CRAN URL was the drifting `/cran/latest`.
`renv::restore()` followed the RHEL9 URLs on Ubuntu 24.04 runners
and fetched binaries linking against `libicui18n.so.67`, which the
runner does not ship (`stringi.so` load fails). Fix, applied to the
lockfile only:
  - Top-level CRAN URL re-pinned from `/cran/latest` to
    `/cran/2026-06-21` (a specific date snapshot).
  - All 110 per-package `Repository` fields normalized to the alias
    `"CRAN"` so `renv::restore()` resolves via the top-level
    `Repositories` using the runner's OS-appropriate URL (set by
    `r-lib/actions/setup-r`).
  - Added the `tomledit` (v0.1.1) entry introduced by the toml
    emitter in the previous release so `renv::status()` returns to
    a consistent state.

- **New**: `val_pipeline()` has been split into two phases so callers
who want to install a snapshot with `rv` before the (expensive) build
step can do so without duplicating work:

  - `val_prep_pipeline()` — runs everything up through pre-filter,
    `pass_primary` inclusion, and full dependency-tree resolution
    (the block that used to live at the top of `val_build()`),
    then writes a `pipeline.toml` file to `<val_dir>/pipeline.toml`
    listing every package the run intends to assess plus the
    val-date-adjusted CRAN + BioC repositories. Returns a
    `val_prep` object.

  - `val_pipeline(prep = <val_prep>)` and
    `val_build(prep = <val_prep>)` both accept the prep result and
    fast-path straight to the build phase, reusing the resolved
    `pkgs`, `vers`, `avail_pkgs`, `val_dir` and `opt_repos` rather
    than recomputing them.

  Fully backwards-compatible: `val_pipeline()` with no `prep`
  argument still runs both phases end-to-end (it just calls
  `val_prep_pipeline()` internally).

- **New**: `write_pipeline_toml()` helper writes the `[project]`
`pipeline.toml` format `rv` expects (array-of-inline-tables
`repositories` with `alias` + `url`,
one-package-per-line `dependencies` array). Reusable outside the
pipeline. Adds `tomledit` to `Imports`.

- **Refactor**: the "resolve full dependency tree, sort by
dep-frequency" block that was duplicated between `val_prep_pipeline()`
and `val_build()` is now a single exported helper,
`resolve_pkg_tree()`, which both call. No behaviour change.

- **Enhancement (`verbose = "minimal"`)**: the per-package summary line
now leads with an abbreviated `[HH:MM]` timestamp (US/Eastern) and a
right-aligned `(idx/total)` position counter so long `val_build()` runs
show at a glance both when each package landed and how far into the
list we are. Format is now roughly:

  ```
     [09:25] (   1/1195) [Low]     dplyr v1.1.4          (12s)
     [09:25] (   2/1195) [Medium]  ggplot2 v3.5.1        (2m 18s)
     ...
     [10:47] (1195/1195) [Low]     zoo v1.8-12           (3s)
  ```

  Implemented via new optional `pkg_idx` / `pkg_total` / `timestamp`
  args on `val_pkg_summary_line()`; `val_build()` threads the counter
  through to all three summary-line call sites (the cached, dep-skip,
  and normal branches). Standalone `val_pkg()` calls (which don't know
  where they sit in a run) still render the timestamp and omit the
  counter cleanly rather than printing `(NA/NA)`.

- **Bug fix**: `val_pkg()` (and therefore `val_pipeline()` / `val_build()`) no
longer crashes with `Error in dplyr::case_when(): object 'aa_metrics' not
found` when processing a package whose initial assessment doesn't hit any
auto-accept threshold. `dplyr::case_when()` eagerly evaluates every RHS
expression regardless of which LHS matches, so referencing `aa_metrics`
inside a case that was only meant to fire when `decision_aa` is `TRUE`
still tries to evaluate `paste(aa_metrics, collapse = ", ")` on the
non-auto-accept path — where `aa_metrics` was never defined. Regression
introduced in #37 (`decision_reason_note` with driver metrics). Fixed by
initialising `aa_metrics <- character(0)` unconditionally before the
`if(decision_aa)` block.

- All `val_*` entry points (`val_pipeline()`, `val_build()`, `val_pkg()`,
`val_categorize()`, `val_decision()`, `val_pipeline_report()`) gain a
new `verbose` argument that dials console output up or down without
touching source. Four tiers, cumulative:
  - `"quiet"` (or `0`, `FALSE`): silent. Only `warning()` / `stop()` fire.
  - `"minimal"` (or `1`): one line per package as it lands
    (`   [Low]     dplyr v1.1.4                     (12s)`), plus
    pipeline / build banners and top-level summary counts. Intended
    for production runs.
  - `"normal"` (or `2`, `TRUE`, the default): every progress marker
    the pipeline emitted prior to this change (per-package `-->
    downloaded`, `--> untarred`, `--> initial reference complete`,
    `--> Report built`, etc.). Backwards-compatible \u2014 users who
    don't pass `verbose` see no change.
  - `"verbose"` (or `3`): everything at `"normal"` plus deep progress
    crumbs previously emitted unconditionally (per-metric column
    dumps, `#N of M:` counters, dependency-driven decision-update
    lines). Intended for debugging.

  The session option `val.pipeline.verbose` sets the default across a
  session (e.g. `options(val.pipeline.verbose = "minimal")`); the
  function argument wins when both are set. Internally a single
  `val_msg()` helper reads the current tier and no-ops when a message
  is below the threshold, so no `cat()` call in the package is
  unconditional anymore \u2014 warnings and errors are the only
  guaranteed console output.

- `val_pipeline()` now writes one `qualified-<source>.txt` file per
package source (`CRAN`, `BioC`, and `github` — every non-CRAN/BioC
github-hosted source, regardless of its user-defined label in
`opt_repos`, is normalised to a single `github` bucket by
`get_repo_origin()`) into the `val_build()` output directory,
alongside `qual_metadata.rds`. Each file is a plain, newline-delimited,
alphabetised list of qualified package names — no header, no quoting,
no comments — so it can be dropped straight into the source
configuration of the "validated" Posit Package Manager (PPM) repo
provisioned into a GxP environment. Qualified packages whose source
can't be identified (`repo_name` is `NA` or `"unknown"`) are folded
into a single `qualified-NA.txt` bucket so no qualified package
silently drops out of provisioning. As part of this change,
`val_pkg()` now persists a plain-string `repo_name` column (e.g.
`"CRAN"`, `"BioC"`, `"github"`, `"unknown"`) alongside the existing
named-character `repos` URL field into each package's `_meta.rds`, so
downstream consumers of `qual_metadata.rds` don't have to re-derive
the source label by URL-matching against the current session's
`getOption("repos")`. If an older `qual_metadata.rds` predating the
`repo_name` column is passed in, `write_qualified_pkg_lists()`
transparently reverse-engineers the label per-row from the `repos`
URL column via `get_repo_origin()` so historical files can still be
processed without a re-run. Backed by the new internal helper
`write_qualified_pkg_lists()`. Runs before `val_pipeline_report()` and,
like it, is wrapped in `tryCatch()` so a write failure doesn't sink the
whole pipeline.
- **Performance**: `val_categorize()` now runs dramatically faster on
large candidate universes. The internal `rip_cats()` helper previously
wrapped its per-metric `dplyr::mutate(!!! cond_exprs)` call in
`dplyr::rowwise()` / `dplyr::ungroup()`, which forced dplyr to
re-evaluate each fully-vectorised `dplyr::case_when()` on 1-row slices
of `pkgs_df`. Because the expressions built by `get_case_whens()`
compose vectorised primitives (`is.na()`, `<`, `>`, `dplyr::between()`,
`%in%`, `dplyr::case_when()`) the rowwise pass was redundant. Dropping
it lets dplyr evaluate each expression once per column, cutting the
categorisation step from many minutes to seconds on the full CRAN
universe. Results are byte-identical to the previous implementation.
- Found out that Posit provides their own validation documentation for several
co-horts of packages they develop, so we've added them to the config's
`approved_pkgs` config element by default. (#42)
- Fix `qual_metadata.rds` so `final_decision` / `final_decision_reason` are
populated for every package (not just dependency-downgraded ones). The
interim pre-propagation frame is now saved separately as
`qual_metadata0.rds`, and the post-propagation frame is written before the
per-package meta RDS update walk so any error inside that walk cannot leave
a stale `qual_metadata.rds` on disk. (#53)
- Add `decision_reason_note` / `final_decision_reason_note` to the
per-package meta bundle, populated with the specific driver metric(s) or
failing dep pkg name(s) depending on the reason. Also shorten the
`decision_reason` for auto-accepted pkgs from
`"Met auto-accepted metric threshold(s) for: <metrics>"` to just
`"Auto-Accepted"` (the metric names now live in the note). Covers three
cases: `"Auto-Accepted"` (auto-accept metrics), `"Risk Assessment"`
(driver metrics whose per-metric `_cat` matched the final risk), and
`"Dependency"` (failing dep pkg name(s), comma-separated — best effort per
the caveat in issue #37). (#37)
- `val_pipeline_report()`: HTML output now renders the per-risk-category
package lists (and other multi-row tables) as filterable, sortable
`{reactable}` widgets, and a new **Appendix: Metric Thresholds** table
gives a human-readable summary of the `decide` rule block from
`config.yml` with `Low` / `Medium` / `High` / `Auto-Accept` as their own
columns (e.g. `120,000 – 240,000`, `< 35 or NA`). The `promote_min` knob
is intentionally omitted. PDF output falls back to plain kable tables.
Adds `{reactable}` as an `Imports` dependency. (#59)
- Add `val_pipeline_report()`, a new exported function that renders a
high-level HTML + PDF summary of a `val_pipeline()` / `val_build()` run
suitable for GxP / QMS archival. It takes a `qual_metadata.rds` (and
optionally the sibling `qual_assessments.rds` for richer per-metric
distributions) and writes the rendered reports alongside the inputs by
default. `val_pipeline()` now invokes it automatically at the end of every
run. The template is tolerant of older evidence files: missing
`_note` cols (pre-#37), missing `assessment_runtime_mins` (pre-runtime
tracking), `NA` `final_decision` values (pre-#53), and missing / list-
typed `qual_assessments` metric cols are all handled gracefully. The R
function validates a minimum required column set and errors with a clear
message on truly ancient files. Adds `{quarto}` (R pkg + CLI) as an
`Imports` dependency. (#59)
- `val_pipeline_report()`: run-metadata table gains a **Candidate
packages** row (`nrow(pre_filtered_pkg_metrics)`) shown above **Packages
assessed**; **Assessment source** is renamed to **Assessment reference**;
and NA `ref` / `metric_pkg` values (rows categorised via dependency
propagation only) are dropped before flattening so those cells no longer
show a literal `"NA"`. `val_pipeline()` now persists
`pre_filtered_pkg_metrics.rds` alongside `qual_metadata.rds` so re-runs
of the report against the same directory pick the candidate count up
automatically. (#59)
- `val_pipeline_report()`: add a new **Pre-Filter Summary** section
(before Decision Summary) with the pre-filter risk distribution and a
pass / drop count table, plus a new **Appendix: Packages Dropped by
Pre-Filter** with a filterable table of every candidate that didn't
make it into `val_build()` (including download counts, reverse-dep
counts, and other raw metric values for context). The `Candidate
packages` row on the Run Metadata table is renamed to `Candidate
packages evaluated`. `val_pipeline()` now persists
`pre_filtered_pkg_metrics.rds` **eagerly** (right after the pre-filter
data frame is created, before `val_build()` runs) so an interrupted
run still leaves the candidate universe on disk. Adds a
`pre_filtered_path` argument to `val_pipeline_report()` for callers
who want to pass it explicitly; `NULL` (default) auto-detects a
sibling, `NA` skips the pre-filter sections gracefully. (#59)
- `val_pipeline_report()`: rework the Pre-Filter Summary section.
  * When `pre_filtered_pkg_metrics.rds` is unavailable, only the
    parent heading renders (all H2 sub-sections are suppressed
    instead of showing empty placeholders).
  * Pass / drop breakdown now appears before Pre-filter risk
    distribution, and its rows are sorted by descending count.
  * New **Per-metric risk distribution** sub-section that
    consolidates the Low/Medium/High counts + percents that
    `val_pipeline()` prints to the console for each `_cat` metric
    into a single filterable table.
  * The dropped-packages table (previously in its own appendix) is
    now the last sub-section of Pre-Filter Summary.
  Adds a `pipeline_runtime` argument to `val_pipeline_report()`
  (accepts `difftime`, numeric seconds, or a pre-formatted string).
  When supplied, a `val_pipeline() runtime` row is added to the
  Run Metadata table. `val_pipeline()` now passes
  `Sys.time() - val_start` in automatically. Not persisted in the
  evidence RDS files (pipeline-level fact, not per-package). (#59)
- `val_pipeline_report()`: the Per-metric risk distribution table
now includes a **Downloads (1yr)** row (backed by the primary
metric's per-package risk category), and all rows use human-friendly
labels (**Reverse dependencies**, **Dependencies**, **Vignettes**,
**NEWS currency**, **Source control**, **Website**, ...) instead of
raw column names like `rev_deps_cat` / `n_vig_cat`. Enabling this
required a one-line change to `val_categorize()` to stop dropping
`primary_risk_category` / `exception_risk_category` at the end of
its pipeline (no code outside `R/val_decision.R` referenced those
columns, so the change is additive). Older
`pre_filtered_pkg_metrics.rds` files missing `primary_risk_category`
still render \u2014 the Downloads row is simply skipped. (#59)
- `val_pipeline_report()`: the per-package **Packages dropped by
pre-filter** table now only renders in HTML output. The PDF version
keeps the sub-section heading and dropped-count summary sentence but
omits the (potentially very large) filterable listing, keeping the
archival PDF short enough to navigate. Readers are pointed to the
HTML rendering for the full listing. (#59)
- Speed up the post-assessment collation step in `val_build()` by
replacing the O(n^2) `purrr::reduce(dplyr::bind_rows)` pattern used
to stitch per-package `_assess_record.rds` files and per-package
meta bundles into a single frame with a single O(n)
`dplyr::bind_rows(list_of_frames)` call, cutting many minutes off
runs with ~1000+ packages. Also emit `--> Saved ...` lines at
`minimal` verbosity for `qual_assessments.rds` and
`qual_metadata0.rds` so minimal-tier runs surface those write
milestones the same way `qual_metadata.rds` already does. (#69)

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
