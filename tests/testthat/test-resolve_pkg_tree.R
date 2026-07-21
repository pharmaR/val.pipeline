
# resolve_pkg_tree() is normally driven by a live available.packages()
# call. We inject a small synthetic avail_pkgs frame so the tests stay
# offline and deterministic.
local_avail <- function() {
  data.frame(
    Package = c("A", "B", "C", "D", "E"),
    Version = c("1.0", "2.0", "3.0", "4.0", "5.0"),
    stringsAsFactors = FALSE
  )
}

test_that("resolve_pkg_tree(): NULL pkg_names returns everything in avail_pkgs", {
  ap <- local_avail()
  res <- resolve_pkg_tree(
    pkg_names  = NULL,
    deps       = NULL,
    avail_pkgs = ap
  )
  expect_setequal(res$pkgs, ap$Package)
  expect_length(res$vers, nrow(ap))
  expect_identical(res$avail_pkgs, ap)
})


test_that("resolve_pkg_tree(): NULL deps preserves the seed set (no expansion)", {
  ap <- local_avail()
  res <- resolve_pkg_tree(
    pkg_names  = c("A", "C"),
    deps       = NULL,
    avail_pkgs = ap
  )
  # pkgs are pulled from avail_pkgs in avail_pkgs' row order, filtered
  # to the seed set — so order matches ap's ordering, not the seed's.
  expect_setequal(res$pkgs, c("A", "C"))
  expect_length(res$pkgs, 2L)
  expect_identical(res$vers, c("1.0", "3.0"))
  # avail_pkgs is passed through untouched when no expansion happens.
  expect_false("dep_freq" %in% names(res$avail_pkgs))
})


test_that("resolve_pkg_tree(): expansion sorts avail_pkgs by dep_freq desc, ties alpha", {
  ap <- local_avail()
  # Fake dep tree: D→B, E→B, C→{A,B}. So the dep-frequency table is
  # B=3 (mentioned in D, E, C) and A=1.
  fake_deps <- list(D = c("B"), E = c("B"), C = c("A", "B"))

  res <- with_mocked_bindings(
    package_dependencies = function(packages, which, recursive) fake_deps,
    .package = "tools",
    {
      resolve_pkg_tree(
        pkg_names      = c("C", "D", "E"),
        deps           = "depends",
        deps_recursive = TRUE,
        avail_pkgs     = ap
      )
    }
  )

  # avail_pkgs should now be sorted by dep_freq desc, then alpha.
  # Expected freq column: A=1, B=2, C=0, D=0, E=0.
  expect_true("dep_freq" %in% names(res$avail_pkgs))
  freqs <- setNames(res$avail_pkgs$dep_freq, res$avail_pkgs$Package)
  expect_equal(as.integer(freqs["B"]), 3L)
  expect_equal(as.integer(freqs["A"]), 1L)
  expect_equal(as.integer(freqs["C"]), 0L)

  # Row order: B (freq 2), A (freq 1), then C/D/E alphabetical (freq 0).
  expect_identical(res$avail_pkgs$Package, c("B", "A", "C", "D", "E"))

  # pkgs = union(seed, unlisted deps) filtered against avail_pkgs, in
  # the new (freq-sorted) row order.
  expect_identical(res$pkgs, c("B", "A", "C", "D", "E"))
  expect_identical(res$vers, c("2.0", "1.0", "3.0", "4.0", "5.0"))
})


test_that("resolve_pkg_tree(): invalid `deps` value is caught with the stopcheck", {
  expect_error(
    resolve_pkg_tree(
      pkg_names  = "A",
      deps       = "linkingto",   # not one of depends/suggests/most
      avail_pkgs = local_avail()
    ),
    "which_deps"
  )
})
