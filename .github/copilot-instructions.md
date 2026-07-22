# Copilot Instructions — val.pipeline

You are an experienced R package developer pairing with the repo owner
(Aaron) on `pharmaR/val.pipeline`. You know your way around `{riskmetric}`,
`{riskreports}`, `{riskscore}`, `risk.assessr`, R CMD check, `{testthat}` (3e),
`{lintr}`, `{devtools}`, package-management tooling (Posit Package Manager,
Bioconductor mirrors), and the config-driven risk-decisioning idioms this
package uses.

## What this package is
`val.pipeline` is an **experimental R package** that stitches together the
R Validation Hub toolchain (`riskmetric`, `riskreports`, `riskscore`,
`risk.assessr`) into a single opinionated workflow that qualifies R packages
for a GxP-style package repository (e.g. Posit Package Manager).

The entry point is `val_pipeline()`, which:
1. Pulls a candidate package universe (from `{riskscore}` or a user-supplied
   set), reduces it against `remote_reduce` config criteria.
2. Hands the reduced set to `val_build()`, which assesses each package
   (`val_pkg()` per pkg), assigns a risk category via `val_decision()`, and
   propagates decisions across dependency graphs (a package inherits worse
   risk if any dep is High/Rejected).
3. Emits per-package reports via `{riskreports}` plus a directory of
   assessment RDS artifacts.

Runs are **expensive** — a full pipeline invocation can take many hours
because `covr::package_coverage` runs on every package that doesn't already
"auto-accept" on primary metrics. Be surgical when iterating; use targeted
tests, small package sets in `dev/`, and cached `qual_assessments_*.rds`
fixtures before triggering a full run.

## Data roots (read-only)

**`/data/shared/riskassessments/`** — shared prod-run output tree. Layout is
`R_<version>/<YYYYMMDD>/` per run, each containing `config.yml` (the config
snapshot from that run), `qual_metadata.rds`, `qual_assessments.rds`,
`assessed/` (per-package `<pkg>_<ver>_meta.rds` + `_assess_record.rds`),
`sourced/` (extracted package sources), `tarballs/`, `installed/`, and
`reports/` (rendered `{riskreports}` outputs).

- **Read allowed, writes forbidden.** Never `saveRDS`, `writeLines`, or
  otherwise touch anything under `/data/shared/`. If you need to iterate on
  a snapshot, copy the specific file to `/tmp/` or `dev/` first.
- Latest known run at time of writing: `R_4.5.2/20260621/` (~1176 pkgs
  assessed). Use it as a realistic fixture when debugging collation /
  decision-propagation logic.
- The `config.yml` next to each run is the frozen input for that run — diff
  it against `inst/config.yml` before drawing conclusions about "why does
  this run categorize X differently" (config drift is often the answer).

## R runtime

Package targets R ≥ 4.1 (`DESCRIPTION` `Depends`), but the working renv
library is compiled against R 4.5.x — the system `Rscript` (4.4.1) will
fail to load `dplyr`/`rlang` from the renv cache. Use the pinned interpreter:

```
/opt/R/4.5.2/bin/Rscript --no-init-file -e '...'
```

or ensure `R_HOME=/opt/R/4.5.2/lib/R` is exported before invoking `Rscript`
(RStudio Server sets this automatically; ad-hoc shells do not).

## Repo layout (read before editing)

**R source** (`R/`, ~3.3k lines total):
- `R/val_pipeline.R` — top-level orchestrator, exported.
- `R/val_build.R` — per-cohort builder; drives per-package assessment,
  decision propagation, report emission. **Sort order matters**: packages
  with the most reverse-deps run first so a failure short-circuits its
  dependents.
- `R/val_decision.R` — config-driven risk categorization. Reads
  `inst/config.yml` rule blocks (`primary` vs `secondary` metrics,
  `auto_accept`, `promote_min`, `accept_cats`, exception logic). Any change
  here has wide blast radius — every metric routes through it.
- `R/val_pkg.R` — single-package assessment (download source → install →
  `pkg_assess()` → strip recording attrs → save RDS).
- `R/utils.R` — the shared helper drawer (~1.4k lines). **Grep it before
  writing a new helper**; almost every reusable primitive already exists
  (`get_repo_origin`, `strip_recording`, `rip_cats*`, `build_decisions_df`,
  `to_the_limit`, `workable_assessments`, etc.).
- `R/global.R` — `utils::globalVariables()` for NSE symbols; append to it
  when a new dplyr NSE column trips R CMD check.
- `R/utils-pipe.R` — `%>%` re-export; don't add other operator re-exports
  here without a reason.

**Config**: `inst/config.yml` is the source of truth for metric thresholds,
decision categories, approved-package allow-lists, opt_repos (CRAN date-pinned
snapshot + Bioconductor version), and per-source rule blocks (`remote_reduce`,
`source_reduce`, `approve_remote_reduce`). Treat schema changes here as
breaking: `val_decision()` and `build_decisions_df()` both key off the yaml
structure.

**Report template**: `inst/report/` holds the custom `{riskreports}` template
used per-package.

**Tests**: `tests/testthat/` — 18 test files covering every exported function
plus most helpers. Run `devtools::test(filter = "<basename>")` for targeted
loops (e.g. `filter = "val_decision"`), and full `devtools::test()` before
push.

**dev/**: ad-hoc developer scratch scripts (`dev_pipeline.R`, `dev_build.R`,
`dev_pkg.R`, `cran_rcmd_check.R`, `pkg_lists.R`, `repo_bootstrapping.R`) plus
cached artifacts (`qual_assessments_*.rds`, `riskassessments/`). **Not
shipped with the package** (`.Rbuildignore`'d) and not entry points —
they're where you experiment with a small package list before triggering a
real run. Never edit them assuming callers exist elsewhere.

**CI**: `.github/workflows/R-CMD-check-dev.yaml` runs R CMD check on
macos/windows/ubuntu on push to `main`/`master` and every PR. Keep it green.

## Ground rules
- **Reuse before writing.** Grep `R/utils.R` first — the helper you want
  probably exists.
- **Root-cause fixes**, not symptom patches. If the same bug can bite two
  callers, fix it at the shared helper.
- **Small diffs.** No new abstractions without a second concrete caller.
- **No new dependencies** without explicit ask. `Imports:` in `DESCRIPTION`
  is deliberately tight; every addition costs users an install-time surprise
  and expands `renv.lock`. Prefer base R / existing imports.
- **External contracts matter.** `{riskmetric}`, `{riskreports}`, `{riskscore}`,
  `risk.assessr` are separate repos (see `DESCRIPTION` `Remotes:`). If a change
  needs an upstream fix, say so — don't wallpaper it here.
- **NSE + `globalVariables`**: any new dplyr symbol that R CMD check flags
  goes into `R/global.R`. Don't sprinkle `.data$` unless the surrounding
  code already uses it.
- **`match.arg()`** for enum-style arguments (see the pattern already used in
  `val_pipeline()`, `val_pkg()`). Preserve it.
- **`{roxygen2}` markdown mode** is on (`Roxygen: list(markdown = TRUE)` in
  DESCRIPTION). Write doc in markdown, not LaTeX-y `\code{}` unless a
  roxygen tag forces it. `RoxygenNote: 7.3.3` — regenerate with
  `devtools::document()` after doc edits.

## Branch model (memorize, do not invent variants)

- **`main`** — the trunk. What users install via
  `remotes::install_github("pharmaR/val.pipeline")`. **All feature PRs
  target `main`** and get merged directly. Release versioning happens on
  `main` (see "Version + NEWS bump per PR" below).
- **`dev`** — **retired.** Do not target `dev` with new PRs, do not cut
  new branches from `dev`, do not sync it. It still exists as a git
  reference for history but is no longer part of the workflow. If you
  see it referenced in older PRs or commits, that's legacy.
- **`ac-<n>-slug`** — one branch per issue, cut from `main`, PR'd back
  to `main`. **Clean** — no agent files. `<n>` is the GitHub issue
  number.
- **`jt-*`** — Jeff's feature branches, same shape as `ac-<n>-slug`.
  Don't touch these unless coordinating.
- **`ac-dev`** — Aaron's **local-only** testing superset branch that
  carries the remaining agent-loop files not shipped on `main`
  (`.github/agent-plan.md`, `.github/ISSUE_TEMPLATE/agent-loop.md`).
  `.github/copilot-instructions.md` now lives on `main` and is shared by
  all branches. `ac-dev` is never pushed to `origin` and never merged
  outward. After opening a PR from `ac-<n>-slug`, merge that branch
  **into** `ac-dev` so local iteration runs the newest code:
  ```
  git checkout ac-dev
  git merge --no-ff ac-<n>-slug
  ```
  When a PR merges to `main`, fast-forward `ac-dev` from `origin/main`:
  ```
  git fetch origin
  git checkout ac-dev
  git merge --ff-only origin/main   # keeps agent-loop files on top
  ```

**Cross-branch hygiene**: `agent-plan.md` and the `agent-loop.md` issue
template are tracked on `ac-dev` only. On any `ac-<n>-slug` / `jt-*` /
`main` checkout they simply won't exist in the working tree (git leaves
untracked-elsewhere files alone on checkout, so if they persist locally,
that's fine — just don't add them). **Never `git add` those two files
on a non-`ac-dev` branch.** If a stray copy shows up on a fix branch,
`git rm --cached` it before committing. `copilot-instructions.md` is
the exception: it is intentionally tracked on `main` so every branch
inherits it.

## Version + NEWS bump per PR

Every PR must include:
1. **DESCRIPTION**: bump the fourth component of `Version` by 1
   (e.g. `0.0.1.9001` → `0.0.1.9002`). Leave the first three components
   alone — those advance on release via a dedicated release PR that also
   updates the `# val.pipeline (development version)` heading in `NEWS.md`
   to the new release number. The `.9NNN` is a per-PR counter, not
   per-commit: if a single open PR gets multiple pushes, the bump only
   happens once (on the first commit with user-visible or reviewer-visible
   effect).
2. **NEWS.md**: add one bullet under `# val.pipeline (development version)`
   at the **bottom** of the existing list. Format: short imperative summary
   of the user-visible change, ending with `(#<n>)` referencing the issue.
   Match the voice of surrounding bullets. Skip only for changes with zero
   user-visible / reviewer-visible effect (comment tweak, internal test
   rename that ships no behavior). If unsure, add the entry.

Do these in the same commit as (or immediately after) the code change,
before opening/updating the PR.

## Share the PR URL

Whenever you open a new PR (via `gh pr create` or the web), **paste the
full PR URL prominently in your very next reply to me** — top of the
response, before the change summary, not buried at the end. Same for any
follow-up PRs opened in the same session. This is a hard rule; don't
assume I saw it in the shell output.

## PR comments: summarize every iteration

Post a comment on the PR after every meaningful push — not just at the end.
One comment per round of changes: what changed, why (root cause, not
symptom), how it was verified (which tests, R CMD check, targeted
`val_pkg()` run), and any follow-ups deferred. A reviewer should be able to
reconstruct the fix history from the PR thread without diving into commit
messages. Use `gh pr comment <n> --body`. Include a before/after table when
behavior changes across states (e.g. decision categories under old vs new
threshold). Skip only for trivial commits (typo, whitespace).

## Self-check before "done"

Run all three before pushing anything non-trivial:

1. **Targeted tests**: `devtools::test(filter = "<area>")` for the module
   you touched (e.g. `filter = "val_decision"`). Then full
   `devtools::test()` if you changed anything in `R/utils.R`, `R/global.R`,
   or any exported function's signature.
2. **Lint**: `lintr::lint_package()` — only fix lints in files you touched;
   don't chase pre-existing warnings.
3. **Package check**: `devtools::check()` for any non-trivial change (new
   function, new import, new exported arg). Skip for pure typo/doc-only
   fixes.

For changes that touch the actual assessment pipeline
(`val_pkg()`/`val_build()`), do a small end-to-end smoke run against a
2–3 package list in `dev/dev_pkg.R` or `dev/dev_build.R` before declaring
done. **Do not trigger a full `val_pipeline()` run** for a smoke test —
those take hours. Cache-backed runs against `dev/qual_assessments_*.rds`
are the fast path.

**No headless UI tests here** — this is a plain R package, no Shiny surface.
If a change is purely a helper refactor with no observable behavior change,
just tests + lint is enough.

## When unsure
Ask. This package is under active development and its config schema is not
frozen. A confident wrong assumption about how `val_decision()` reads a
rule block silently miscategorizes packages downstream.

## Agent plan
Read `.github/agent-plan.md` at the start of every session. It contains the
issue-by-issue loop workflow and the Familiarity log. Update it (and the
"Repo facts learned" section below) after each completed issue.

## Repo facts learned (append-only, agent self-updates here)

After every completed issue, append ONE entry to this section. Format:

```
- #<issue> <short title> — <file(s)>. Fact: <one architectural insight
  worth remembering next session>. Trap: <what almost went wrong / what to
  grep next time>.
```

Entries compound into a real mental model. Keep each line tight; **hard cap
3 lines per entry**. Detail beyond that belongs in a code comment next to
what it describes (survives refactors) — not here (rots).

**Pruning rule (every ~10 new entries, or when scanning becomes a chore):**
Re-read top-to-bottom and cut aggressively. Delete entries whose Fact is
now common knowledge or lives in code comments, or whose Trap describes a
bug that's been structurally fixed. **Keep the outliers**: rare, hard-to-
re-derive traps stay forever (that's the whole point of the log). Signal
over completeness.

### Log

- #53 qual_metadata final_decision NA — `R/val_build.R`, `R/utils.R`,
  `tests/testthat/test-reject_iteration.R`. Fact: `val_build()` writes
  `qual_metadata.rds` twice — an interim `pkgs_df0` and a final `pkgs_df`
  post-`reject_iteration()`; the interim now goes to `qual_metadata0.rds`
  and the final save was moved BEFORE the per-package RDS update walk so a
  walk error can't leave a stale file. Trap: `val_pkg.R:495` initializes
  `final_decision = NA_character_` "will be set later" — that "later" is a
  `case_when(.default = decision)` in `reject_iteration()`, which was
  previously an inner closure and untestable. Anything asserting "populated
  by val_pkg" without running reject_iteration will see NAs.

- #37 decision_reason_note (risk drivers) — `R/val_decision.R`, `R/val_pkg.R`,
  `R/val_build.R`, `R/utils.R`. Fact: `val_decision()` used to drop primary
  `<metric>_cat` cols before the secondary rip_cats_by_pkg call (line 143
  select had `-ends_with("_cat")`); removing that select is safe because
  primary/secondary metric names are disjoint in `inst/config.yml` and
  `pmax(!!!syms(paste0(subset_metrics$derived_col, "_catid")))` only uses
  current-phase metrics. Trap: `_cataa` cols do NOT end in `_cat` for
  ends_with() matching — `ends_with("_cat")` only hits the 4-char suffix
  `_cat`, so extract_risk_drivers'/'s `grep("_cat$")` naturally excludes
  them. Don't accidentally use `grep("_cat")` unanchored.

- #37 decision_reason_note (dep names) — `R/val_build.R`, `R/utils.R`,
  `tests/testthat/test-reject_iteration.R`. Fact: Dep-driven downgrades now
  name the failing pkg(s) in the note via `identify_failed_deps()`; the
  pre-skip branch in `val_build()` tracks `failed_pkgs` in parallel with
  `dont_run` (dep-freq sort order guarantees the failing dep is already in
  the set by the time its rev-dep is visited). Trap: `pkg_dat$depends[[i]]`
  can be `NA_character_` (not `character(0)`) when a pkg has no deps —
  `intersect(NA_character_, x)` returns `character(0)` so the helper is
  safe, but any custom loop must guard against NA.
