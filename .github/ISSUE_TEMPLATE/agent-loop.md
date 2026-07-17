---
name: Agent loop issue
about: Format an issue so the val.pipeline agent loop can pick it up directly.
title: ""
labels: []
assignees: []
---

### Symptom (1 sentence)
When <do X>, <Y happens> but <Z expected>.

### Where
File: `R/val_<name>.R` (or `R/utils.R`) | Config block: `<name>` in `inst/config.yml` | Upstream pkg (if suspected): `{riskmetric|riskreports|riskscore|risk.assessr}`

### Repro
1. `devtools::load_all()`
2. Minimal invocation (single-pkg via `val_pkg()`, cached fixture via `dev/dev_build.R`, or in-memory config → `val_decision()` — pick the smallest that shows the bug)
3. Observe: <bug>

<!-- Do NOT require a full `val_pipeline()` run. Narrow to one package or a cached `dev/qual_assessments_*.rds` fixture. If you truly can't, say so and we'll pair on scoping the repro. -->

### Expected vs Actual
- Expected: <one line>
- Actual:   <one line>

### Acceptance
- [ ] <done condition 1>
- [ ] <done condition 2>
- [ ] `devtools::test()` passes; `lintr::lint_package()` clean on touched files
- [ ] `devtools::check()` clean (skip only for typo/doc-only fixes)
- [ ] `NEWS.md` bullet added; `DESCRIPTION` `Version` fourth component bumped

### Scope
In:  <files/functions to touch>
Out: <files/functions to NOT touch>

### Breadcrumbs (optional)
- Related PR: #
- Related fn: `pkg::fn()`
- Config key: `<block>.<rule>` in `inst/config.yml`
- Prior context: <one line>
