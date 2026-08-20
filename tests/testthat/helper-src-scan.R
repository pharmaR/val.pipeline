# Locate an R source file for source-level invariant scans.
#
# Under devtools::test(), the working dir is `tests/testthat/` and
# `../../R/<file>.R` resolves. Under R CMD check the package is installed
# first and `R/` in the install tree contains lazy-load databases, not
# source. Try the source-tree location first, then fall through to
# `system.file("R", ...)` (which usually won't have .R files either),
# and callers must `skip_if_not(nzchar(path))` when nothing is found.
#
# Returns "" (empty string) when no accessible source file exists.
src_path_for <- function(basename_r) {
  p <- testthat::test_path("..", "..", "R", basename_r)
  if (file.exists(p)) return(p)
  p2 <- system.file("R", basename_r, package = "val.pipeline",
                    mustWork = FALSE)
  if (nzchar(p2) && file.exists(p2)) return(p2)
  ""
}
