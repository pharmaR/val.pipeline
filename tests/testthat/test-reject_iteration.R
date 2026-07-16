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
      "Met auto-accepted metric threshold(s) for: downloads_1yr",
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
    "Met auto-accepted metric threshold(s) for: downloads_1yr"
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

test_that("reject_iteration() never downgrades Pre-Approved packages", {
  pkg_dat <- make_pkg_dat()
  # Mark C (which depends on failed B) as Pre-Approved
  pkg_dat$decision_reason[pkg_dat$pkg == "C"] <- "Pre-Approved package"
  failed <- pkg_dat$pkg[pkg_dat$decision != decisions[1]]
  out <- reject_iteration(pkg_dat, dec_reject = "High",
                          deps = "depends", decisions = decisions,
                          failed_pkgs = failed)
  # Even though C's dep failed, pre-approval protects it
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
