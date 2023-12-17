test_that("missing_gone works", {
  new_data <- missing_gone(conflict_data, threshold = 0.1)
  expect_true("Maternal Mortality rate" %in% new_data$removed_vars)
})
