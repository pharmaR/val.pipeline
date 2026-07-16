test_that("identify_failed_deps() returns NA when nothing intersects", {
  expect_true(is.na(identify_failed_deps(c("A", "B"), c("X", "Y"))))
})

test_that("identify_failed_deps() returns single matching pkg name", {
  expect_equal(identify_failed_deps(c("A", "B", "X"), c("X", "Y")), "X")
})

test_that("identify_failed_deps() returns sorted unique comma list", {
  expect_equal(
    identify_failed_deps(c("B", "A", "C", "A"), c("A", "B", "Z")),
    "A, B"
  )
})

test_that("identify_failed_deps() handles NULL / character(0) / NA inputs", {
  expect_true(is.na(identify_failed_deps(NULL, c("X"))))
  expect_true(is.na(identify_failed_deps(character(0), c("X"))))
  expect_true(is.na(identify_failed_deps(c("A", "B"), character(0))))
  expect_true(is.na(identify_failed_deps(NA_character_, c("A"))))
  expect_true(is.na(identify_failed_deps(c("A", NA_character_), c("X"))))
})

test_that("identify_failed_deps() ignores NA entries in dep list", {
  expect_equal(
    identify_failed_deps(c("A", NA_character_, "B"), c("A", "B")),
    "A, B"
  )
})
