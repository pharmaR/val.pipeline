# val.pipeline — Improvement Ideas

Compiled 2026-07-29. Ordered by rough impact; the last section is a
suggested pick-3 if you only want to tackle a few.

---

## High-impact

1. **Config validation + versioning.**
   `pull_config()` returns whatever YAML you feed it; a typo in a key
   (e.g. `weight` vs `weights`) silently falls back to defaults. Ship a
   JSON Schema (or `config::validate()` shim) that runs at
   `val_pipeline()` start and errors early with the offending key.
   Add a `config_version:` field so old configs are rejected/migrated
   instead of half-working.

2. **Fully resumable runs.**
   `val_build()` is the expensive step. You already cache per-package
   assessments to `<val_dir>/assessments/<pkg>.rds`. Add a `resume=TRUE`
   flag that skips packages whose rds already exists (guarded by a
   checksum of inputs). Combined with the BioC shim, this lets air-gapped
   users iterate without paying full cost.

3. **Structured logging + a single `run.log`.**
   Verbosity tiers control console output; also write every phase
   (`prep_start`, `prep_end`, `build_start`, per-pkg start/end, timings,
   warnings) as JSON lines into `<val_dir>/run.log`. Makes post-mortems
   and CI dashboards straightforward. Bonus: surface total wall-clock
   per phase in the final report.

4. **Parallelism for `val_build()`.**
   Package assessments are embarrassingly parallel. Wrap the main loop
   in `future.apply::future_lapply()` with a `workers=` param (default 1
   for reproducibility). On a 32-pkg run this is a real difference.

5. **Deterministic snapshot manifest.**
   Emit `manifest.json` alongside `pipeline.toml` capturing: R version,
   riskmetric/val.pipeline versions, `opt_repos` with resolved snapshot
   dates, config hash, package SHAs. That single file makes a report
   reproducible on demand.

---

## Medium-impact

6. **Retry + backoff for network ops.**
   `available.packages()` / `install.packages()` failures during PPM
   hiccups tank a whole run. Wrap network entry points in a small
   `.with_retry()` helper (3 tries, exponential backoff, respect
   `Retry-After`).

7. **Per-package timeout.**
   A hanging `R CMD check` or `covr` on one package can wedge the
   pipeline. `R.utils::withTimeout()` with per-metric budgets from
   config, and record `"timed_out"` as the metric value.

8. **`dry_run = TRUE`.**
   Have `val_pipeline()` accept a flag that runs prep + prints the
   resolved package list, `opt_repos`, config, and estimated run size
   (deps count) without building. Great for reviewers before a big run.

9. **Bundle the shim opt-in into config.**
   Instead of `Sys.setenv(VAL_PIPELINE_INTERNAL_BIOC=1)`, add a
   top-level `air_gapped: true` (or `bioc: {mode: internal}`) knob in
   `config.yml`. Env var stays as an override. Feels more discoverable.

10. **Config discovery cascade docs + tests.**
    `config_path` was recently added — document the resolution order
    (arg > `val.pipeline.config_path` option > env `VAL_PIPELINE_CONFIG`
    > pkg default) in `?pull_config` and add a `test-pull_config.R`
    case for each layer if it isn't already there.

---

## Lower-impact / hygiene

11. **`val_prep` object should be self-contained.**
    So `val_pipeline(prep = readRDS("prep.rds"))` works across sessions
    and machines. Requires storing config, opt_repos, val_date, package
    list; avoid function references or lazy env captures.

12. **CLI wrapper.**
    `Rscript -e 'val.pipeline::val_pipeline_cli(...)'` or an
    `inst/scripts/val-pipeline` file that parses `--config`, `--out`,
    `--resume`, `--workers`. Air-gapped ops teams tend to prefer CLI
    over interactive R sessions.

13. **`val_decision.R` (690 lines) refactor.**
    Biggest single file. Split by concern (rule parsing / evaluation /
    rendering) and unit-test each piece. Comparable for `utils.R`
    (1942 lines) — split at least `utils-riskmetric.R`,
    `utils-config.R`, `utils-format.R`.

14. **Snapshot tests for report rendering.**
    Small fixture package + expected report — currently a lot of manual
    round-trips. `_snaps/` already exists; add a quarto-render snap for
    at least one pkg.

15. **`NEWS.md` linkification.**
    Autolink `#77`-style refs to GH PRs in `pkgdown` output.

16. **Deprecation policy.**
    `val.pipeline.config_path` option, `VAL_PIPELINE_INTERNAL_BIOC` env
    — as knobs multiply, document lifecycle and use
    `lifecycle::deprecate_*()` so users get warnings, not silent breaks.

---

## New capability ideas

17. **`val_diff(prev_val_dir, this_val_dir)`.**
    Rerun-friendly diff summary: added/removed packages, metric deltas,
    score changes. Enormously valuable during quarterly re-validation
    cycles.

18. **Package allowlist / denylist.**
    Config-level `deny: [pkg1, pkg2]` for orgs that must exclude
    specific packages regardless of criteria.

19. **`val_pipeline_report()` — an executive summary across all pkgs.**
    You already have per-pkg reports; a single top-level PDF/HTML
    aggregating pass/fail counts, top 10 risky packages, missing
    metrics, run stats.

---

## Top-3 recommendation if picking a few

- **#2 Fully resumable runs** — biggest quality-of-life win, especially
  on the air-gapped server.
- **#1 Config validation + versioning** — biggest robustness improvement.
- **#4 Parallelism** — biggest wall-clock improvement.

## Natural PR pairings

- **#1 + #9** — config validation and the air-gapped opt-in are both
  config-schema work.
- **#2 + #8** — resume + dry_run touch the same entry-point plumbing.
- **#3 + #5** — structured logging + manifest.json share a "write
  metadata alongside the run" theme.
- **#6 + #7** — retries + timeouts are both fault-tolerance around
  external calls.
