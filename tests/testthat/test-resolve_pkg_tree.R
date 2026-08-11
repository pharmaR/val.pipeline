
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


test_that("resolve_pkg_tree(): rev_deps=NULL (default) skips reverse expansion", {
  ap <- local_avail()
  called <- FALSE
  res <- with_mocked_bindings(
    package_dependencies = function(packages, which, recursive, reverse = FALSE) {
      if (isTRUE(reverse)) called <<- TRUE
      setNames(rep(list(character()), length(packages)), packages)
    },
    .package = "tools",
    {
      resolve_pkg_tree(
        pkg_names      = c("A"),
        deps           = "depends",
        deps_recursive = TRUE,
        avail_pkgs     = ap
      )
    }
  )
  expect_false(called)
  expect_true("A" %in% res$pkgs)
})


test_that("resolve_pkg_tree(): rev_deps folds reverse-dep pkgs into the seed set", {
  ap <- local_avail()
  # Reverse-dep tree: rev(A) = {C, D}. Forward-dep tree of {A,C,D}: no deps.
  fake_rev  <- list(A = c("C", "D"))
  fake_fwd  <- list(A = character(), C = character(), D = character())

  res <- with_mocked_bindings(
    package_dependencies = function(packages, which, recursive, reverse = FALSE) {
      if (isTRUE(reverse)) fake_rev else fake_fwd
    },
    .package = "tools",
    {
      resolve_pkg_tree(
        pkg_names          = "A",
        deps               = "depends",
        deps_recursive     = TRUE,
        rev_deps           = "depends",
        rev_deps_recursive = FALSE,
        avail_pkgs         = ap
      )
    }
  )
  expect_setequal(res$pkgs, c("A", "C", "D"))
})


test_that("resolve_pkg_tree(): rev_deps_recursive is forwarded to package_dependencies", {
  ap <- local_avail()
  seen_recursive <- NA
  with_mocked_bindings(
    package_dependencies = function(packages, which, recursive, reverse = FALSE) {
      if (isTRUE(reverse)) seen_recursive <<- recursive
      setNames(rep(list(character()), length(packages)), packages)
    },
    .package = "tools",
    {
      resolve_pkg_tree(
        pkg_names          = "A",
        deps               = "depends",
        rev_deps           = "depends",
        rev_deps_recursive = TRUE,
        avail_pkgs         = ap
      )
    }
  )
  expect_true(seen_recursive)
})


test_that("resolve_pkg_tree(): rev_deps ignored when pkg_names is NULL", {
  ap <- local_avail()
  rev_called <- FALSE
  res <- with_mocked_bindings(
    package_dependencies = function(packages, which, recursive, reverse = FALSE) {
      if (isTRUE(reverse)) rev_called <<- TRUE
      setNames(rep(list(character()), length(packages)), packages)
    },
    .package = "tools",
    {
      resolve_pkg_tree(
        pkg_names  = NULL,
        deps       = NULL,
        rev_deps   = "depends",
        avail_pkgs = ap
      )
    }
  )
  expect_false(rev_called)
  expect_setequal(res$pkgs, ap$Package)
})
