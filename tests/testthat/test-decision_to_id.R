



test_that("decision_to_id() converts decision to id", {
  decision_id_df <- data.frame(
    decision = c("Low", "Medium", "High"),
    decision_id = c(1, 2, 3)
  )
  
  result <- decision_to_id(decision_id_df, rev = FALSE, "High")
  expect_equal(result, 3)
  
  result <- decision_to_id(decision_id_df, rev = FALSE, "Low")
  expect_equal(result, 1)
})

test_that("decision_to_id() converts id to decision", {
  decision_id_df <- data.frame(
    decision = c("Low", "Medium", "High"),
    decision_id = c(1, 2, 3)
  )
  
  result <- decision_to_id(decision_id_df, rev = TRUE, 3)
  expect_equal(result, "High")
  
  result <- decision_to_id(decision_id_df, rev = TRUE, 1)
  expect_equal(result, "Low")
})

test_that("decision_to_id_v() works with vectors", {
  decision_id_df <- data.frame(
    decision = c("Low", "Medium", "High"),
    decision_id = c(1, 2, 3)
  )
  sel_dec <- c("High", "Low")
  result <- decision_to_id_v(decision_id_df, rev = FALSE, sel_dec)
  expect_equal(result, c(3, 1) |> setNames(sel_dec))
  
  result <- decision_to_id_v(decision_id_df, rev = TRUE, c(3, 1))
  expect_equal(result, c("High", "Low"))
})
