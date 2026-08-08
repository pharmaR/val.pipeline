decisions <- c("Low", "Medium", "High")

make_pkg_dat <- function() {
  # 4 packages, none pre-approved:
  # - A: assessed Low, no failing deps      -> final = Low
  # - B: assessed High (Risk Assessment)    -> final = High
  # - C: assessed Low BUT depends on B      -> final = High/Dependency
  # - D: assessed Low BUT suggests B        -> depends on `deps` arg
  tibble::tibble(
    pkg              = c("A", "B", "C", "D"),
    decision         = c("Low", "High", "Low", "Low"),
    decision_reason  = c(
      "Auto-Accepted",
      "Risk Assessment",
      "Risk Assessment",
      "Risk Assessment"
    ),
    final_decision         = NA_character_, # as val_pkg.R initializes
    final_decision_reason  = NA_character_,
    decision_reason_note   = NA_character_,
    final_decision_reason_note = NA_character_,
    depends  = list(character(0), character(0), "B",            character(0)),
    suggests = list(character(0), character(0), character(0),   "B")
  )
}

test_that("reject_iteration() leaves final_decision populated for every row", {
  pkg_dat <- make_pkg_dat()
  failed  <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = failed)
  expect_equal(sum(is.na(out$final_decision)), 0L)
  expect_equal(sum(is.na(out$final_decision_reason)), 0L)
})

test_that("reject_iteration() carries decision through un-downgraded pkgs", {
  pkg_dat <- make_pkg_dat()
  failed  <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = failed)
  # A has no failing deps: original decision carries through
  expect_equal(out$final_decision[out$pkg == "A"], "Low")
  expect_equal(
    out$final_decision_reason[out$pkg == "A"],
    "Auto-Accepted"
  )
  # B was assessed High: stays High, reason preserved
  expect_equal(out$final_decision[out$pkg == "B"], "High")
  expect_equal(out$final_decision_reason[out$pkg == "B"], "Risk Assessment")
})

test_that("reject_iteration() downgrades packages whose Depends fail", {
  pkg_dat <- make_pkg_dat()
  failed  <- pkg_dat$pkg[pkg_dat$decision != decisions[1]] # -> "B"
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = failed)
  # C depends on B (which failed): downgraded to High/Dependency
  expect_equal(out$final_decision[out$pkg == "C"], "High")
  expect_equal(out$final_decision_reason[out$pkg == "C"], "Dependency")
  # note names the failing dep (issue #37)
  expect_equal(out$final_decision_reason_note[out$pkg == "C"], "B")
})

test_that("reject_iteration() only downgrades on Suggests when deps has it", {
  pkg_dat <- make_pkg_dat()
  failed  <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]

  # deps = "depends": D suggests B but should NOT be downgraded
  out_dep <- reject_iteration(pkg_dat, dec_reject = "High",
                              deps = "depends", decisions = decisions,
                              failed_pkgs = failed)
  expect_equal(out_dep$final_decision[out_dep$pkg == "D"], "Low")
  expect_equal(
    out_dep$final_decision_reason[out_dep$pkg == "D"],
    "Risk Assessment"
  )
  # No dep failure -> note stays NA (D wasn't downgraded)
  expect_true(is.na(out_dep$final_decision_reason_note[out_dep$pkg == "D"]))

  # deps includes "Suggests": D suggests B, so it SHOULD be downgraded
  out_sug <- reject_iteration(pkg_dat, dec_reject = "High",
                              deps = c("depends", "Suggests"),
                              decisions = decisions, failed_pkgs = failed)
  expect_equal(out_sug$final_decision[out_sug$pkg == "D"], "High")
  expect_equal(out_sug$final_decision_reason[out_sug$pkg == "D"], "Dependency")
  # note names the failing suggest (issue #37)
  expect_equal(out_sug$final_decision_reason_note[out_sug$pkg == "D"], "B")
})

test_that("reject_iteration() protects Pre-Approved packages with no failing dep (#110)", {
  # Chain: A (Low, seed), B (High, seed), C (Pre-Approved, no failing dep),
  #        D (Pre-Approved, depends on failed B) -- see next test.
  # C's only dep is A (Low). No dep failed => Pre-Approved carve-out fires
  # and C stays Low / "Pre-Approved package".
  pkg_dat <- make_pkg_dat()
  pkg_dat$decision_reason[pkg_dat$pkg == "C"] <- "Pre-Approved package"
  # Rewrite C's deps so it does NOT depend on failing B (only on A).
  pkg_dat$depends[[which(pkg_dat$pkg == "C")]] <- "A"
  failed <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = failed)
  expect_equal(out$final_decision[out$pkg == "C"], "Low")
  expect_equal(
    out$final_decision_reason[out$pkg == "C"],
    "Pre-Approved package"
  )
})

test_that("reject_iteration() downgrades Pre-Approved pkgs with failed deps (#110)", {
  # Pre-Approved pkg C depends on failing B. Under #110's narrowed
  # carve-out, C should be downgraded to High with reason
  # "Pre-Approved (dep failed)" so an operator can distinguish it from
  # an ordinary dep-driven downgrade. Note names the failing dep.
  pkg_dat <- make_pkg_dat()
  pkg_dat$decision_reason[pkg_dat$pkg == "C"] <- "Pre-Approved package"
  failed <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]  # "B"
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = failed)
  expect_equal(out$final_decision[out$pkg == "C"], "High")
  expect_equal(
    out$final_decision_reason[out$pkg == "C"],
    "Pre-Approved (dep failed)"
  )
  expect_equal(out$final_decision_reason_note[out$pkg == "C"], "B")
})

test_that("reject_iteration() downgrades Pre-Approved pkgs with failing Suggests (when in scope) (#110)", {
  # Pre-Approved pkg D suggests failing B. When deps includes
  # "Suggests", D should be downgraded to "Pre-Approved (dep failed)".
  # When deps = "depends" only, D is protected.
  pkg_dat <- make_pkg_dat()
  pkg_dat$decision_reason[pkg_dat$pkg == "D"] <- "Pre-Approved package"
  failed <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]  # "B"

  out_dep <- reject_iteration(pkg_dat, dec_reject = "High",
                              deps = "depends", decisions = decisions,
                              failed_pkgs = failed)
  expect_equal(out_dep$final_decision[out_dep$pkg == "D"], "Low")
  expect_equal(
    out_dep$final_decision_reason[out_dep$pkg == "D"],
    "Pre-Approved package"
  )

  out_sug <- reject_iteration(pkg_dat, dec_reject = "High",
                              deps = c("depends", "Suggests"),
                              decisions = decisions, failed_pkgs = failed)
  expect_equal(out_sug$final_decision[out_sug$pkg == "D"], "High")
  expect_equal(
    out_sug$final_decision_reason[out_sug$pkg == "D"],
    "Pre-Approved (dep failed)"
  )
  expect_equal(out_sug$final_decision_reason_note[out_sug$pkg == "D"], "B")
})

test_that("reject_iteration() never downgrades Pre-Approved packages", {
  # #110 narrowed this: Pre-Approved is protected only when NO dep
  # failed. Since C's dep B is failing in `make_pkg_dat()`, we point C
  # at a non-failing dep for this test so the carve-out fires.
  pkg_dat <- make_pkg_dat()
  # Mark C as Pre-Approved and rewire its dep to A (Low, not failing).
  pkg_dat$decision_reason[pkg_dat$pkg == "C"] <- "Pre-Approved package"
  pkg_dat$depends[[which(pkg_dat$pkg == "C")]] <- "A"
  failed <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = failed)
  # No dep of C failed => pre-approval protects it
  expect_equal(out$final_decision[out$pkg == "C"], "Low")
  expect_equal(
    out$final_decision_reason[out$pkg == "C"],
    "Pre-Approved package"
  )
})

test_that("reject_iteration() derives failed_pkgs from final_decision if NA", {
  # Simulate a mid-iteration frame where final_decision is already populated
  pkg_dat <- make_pkg_dat()
  pkg_dat$final_decision <- c("Low", "High", "Low", "Low")
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = NULL)
  # Should still recognize B as failing and downgrade C
  expect_equal(out$final_decision[out$pkg == "C"], "High")
  expect_equal(out$final_decision_reason[out$pkg == "C"], "Dependency")
})

test_that("reject_iteration() note names DIRECT deps, not recursive closure (#107)", {
  # Chain: A (High, seed) <- B (depends A directly) <- C (depends B directly).
  # Recursive-depends of C includes both A and B; direct-depends of C is
  # only B. Pre-#107, C's note would have listed "A, B" (recursive closure
  # intersected with failed). Post-#107, C's note names only "B" -- its
  # actual DESCRIPTION-level dep.
  pkg_dat <- tibble::tibble(
    pkg              = c("A", "B", "C"),
    decision         = c("High", "Low", "Low"),
    decision_reason  = rep("Risk Assessment", 3),
    final_decision         = NA_character_,
    final_decision_reason  = NA_character_,
    decision_reason_note   = NA_character_,
    final_decision_reason_note = NA_character_,
    depends         = list(character(0), "A",           c("A", "B")),
    suggests        = list(character(0), character(0),  character(0)),
    depends_direct  = list(character(0), "A",           "B"),
    suggests_direct = list(character(0), character(0),  character(0))
  )
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = "A")
  expect_equal(out$final_decision, c("High", "High", "High"))
  expect_equal(out$final_decision_reason_note[out$pkg == "B"], "A")
  # C's note must be "B" (its direct dep), not "A, B" (recursive closure).
  expect_equal(out$final_decision_reason_note[out$pkg == "C"], "B")
})

test_that("reject_iteration() falls back to recursive deps when direct cols absent", {
  # Legacy pkg_dat (pre-#107) has no depends_direct / suggests_direct
  # list-cols. reject_iteration() must still work and populate the note
  # by falling back to the recursive `depends` / `suggests` list-cols.
  pkg_dat <- make_pkg_dat()
  expect_false("depends_direct" %in% names(pkg_dat))
  failed  <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = failed)
  expect_equal(out$final_decision[out$pkg == "C"], "High")
  expect_equal(out$final_decision_reason_note[out$pkg == "C"], "B")
})

test_that("reject_iteration() note does NOT include recursive-Suggests noise (#107)", {
  # Regression: pak/lintr were pre-approved and dep-skipped; their
  # decision_reason_note was populated with intersect(recursive Suggests
  # closure, failed_pkgs) yielding ~100 unrelated transitive pkg names.
  # In the reject_iteration path the same bug leaked into propagated
  # notes. Guard against reintroduction: given a pkg whose recursive
  # Suggests closure includes many failed pkgs but whose direct
  # deps/suggests do NOT, the note stays NA (pkg isn't downgraded).
  pkg_dat <- tibble::tibble(
    pkg              = c("X", "Y", "Z"),
    decision         = c("High", "High", "Low"),
    decision_reason  = rep("Risk Assessment", 3),
    final_decision         = NA_character_,
    final_decision_reason  = NA_character_,
    decision_reason_note   = NA_character_,
    final_decision_reason_note = NA_character_,
    # Z's RECURSIVE suggests includes both failed pkgs (X, Y), but its
    # DIRECT deps / suggests are empty -- so Z should NOT be downgraded
    # under deps = "depends" and its note should remain NA.
    depends         = list(character(0), character(0), character(0)),
    suggests        = list(character(0), character(0), c("X", "Y")),
    depends_direct  = list(character(0), character(0), character(0)),
    suggests_direct = list(character(0), character(0), character(0))
  )
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = c("X", "Y"))
  expect_equal(out$final_decision[out$pkg == "Z"], "Low")
  expect_true(is.na(out$final_decision_reason_note[out$pkg == "Z"]))
})



# Guards val_build()'s (and val_finalize()'s in #101) reject_iteration()
# convergence loop against an infinite-loop bug introduced by a stray
# `<<-`. Prior to #103, the loop assigned `failed <<- ...` /
# `pkgs_df <<- ...`, which skipped the enclosing function's frame and
# left both locals stuck at their iter-1 values -- so any cohort large
# enough that dep propagation needed >1 pass (i.e. anything real-world)
# spun forever. Simulate that convergence loop in-test and require it
# to terminate.
test_that("reject_iteration() reaches a fixed point in <= n iterations", {
  # Chain: X (High) <- Y (Low; depends X) <- Z (Low; depends Y).
  # Iter 1: only X is in `failed`; Y gets downgraded via depends.
  # Iter 2: with Y now failed, Z gets downgraded via depends.
  # Iter 3: fixed point.
  # If the outer loop mis-scopes its assignments, this test loops forever
  # and testthat kills it -- an unmissable regression signal.
  pkg_dat <- tibble::tibble(
    pkg              = c("X", "Y", "Z"),
    decision         = c("High", "Low", "Low"),
    decision_reason  = c("Risk Assessment", "Risk Assessment",
                         "Risk Assessment"),
    final_decision         = NA_character_,
    final_decision_reason  = NA_character_,
    decision_reason_note   = NA_character_,
    final_decision_reason_note = NA_character_,
    depends  = list(character(0), "X", "Y"),
    suggests = list(character(0), character(0), character(0))
  )

  # Simulate the exact convergence loop shape used in val_finalize() /
  # val_build(). Local `<-` (not `<<-`) is the intended semantics.
  failed  <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]
  pkgs_df <- reject_iteration(pkg_dat, dec_reject = "High",
                              deps = "depends", decisions = decisions,
                              failed_pkgs = failed)
  n_iter <- 1L
  max_iter <- 10L  # hard cap; test fails if loop doesn't converge.
  while (!identical(pkgs_df$pkg[pkgs_df$final_decision != decisions[1]],
                    failed)) {
    if (n_iter >= max_iter) {
      fail("reject_iteration() convergence loop did not terminate after ",
           max_iter, " iterations")
    }
    failed  <- pkgs_df$pkg[pkgs_df$final_decision != decisions[1]]
    pkgs_df <- reject_iteration(pkgs_df, dec_reject = "High",
                                deps = "depends", decisions = decisions,
                                failed_pkgs = failed)
    n_iter <- n_iter + 1L
  }

  # Chain requires 2 in-loop passes on top of the initial call, so the
  # total counter lands at 3 (1 initial + 2 in-loop).
  expect_equal(n_iter, 3L)
  # Every pkg in the chain ends up High-flagged.
  expect_setequal(pkgs_df$pkg[pkgs_df$final_decision == "High"],
                  c("X", "Y", "Z"))
  # Y and Z were downgraded via a Dependency.
  expect_equal(pkgs_df$final_decision_reason[pkgs_df$pkg == "Y"],
               "Dependency")
  expect_equal(pkgs_df$final_decision_reason[pkgs_df$pkg == "Z"],
               "Dependency")
})

test_that("the buggy `<<-` variant would have spun forever (regression proof, #103)", {
  # Positive proof that the `<<-` variant introduces the bug: run the
  # exact same convergence loop shape with `<<-` instead of `<-` and
  # assert it hits our 10-iteration safety cap without converging.
  # This locks in the diagnosis so a future refactor can't quietly
  # reintroduce `<<-` "because it's shorter".
  pkg_dat <- tibble::tibble(
    pkg              = c("X", "Y", "Z"),
    decision         = c("High", "Low", "Low"),
    decision_reason  = rep("Risk Assessment", 3),
    final_decision         = NA_character_,
    final_decision_reason  = NA_character_,
    decision_reason_note   = NA_character_,
    final_decision_reason_note = NA_character_,
    depends  = list(character(0), "X", "Y"),
    suggests = list(character(0), character(0), character(0))
  )
  # Wrap in a function so `<<-` inside behaves the way it does when
  # embedded inside val_finalize() / val_build() -- i.e. skips this
  # function's own frame.
  run_buggy <- function() {
    failed  <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]
    pkgs_df <- reject_iteration(pkg_dat, dec_reject = "High",
                                deps = "depends", decisions = decisions,
                                failed_pkgs = failed)
    n_iter <- 1L
    while (!identical(pkgs_df$pkg[pkgs_df$final_decision != decisions[1]],
                      failed)) {
      if (n_iter >= 10L) return(n_iter)  # safety cap
      failed  <<- pkgs_df$pkg[pkgs_df$final_decision != decisions[1]]
      pkgs_df <<- reject_iteration(pkgs_df, dec_reject = "High",
                                   deps = "depends", decisions = decisions,
                                   failed_pkgs = failed)
      n_iter <- n_iter + 1L
    }
    n_iter
  }
  expect_equal(run_buggy(), 10L)
})
