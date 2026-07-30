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
