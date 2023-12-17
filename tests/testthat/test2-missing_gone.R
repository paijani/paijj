test_that("missing_gone works", {
  new_data <- missing_gone(conflict_data, threshold = 0.1, exclude = "Maternal Mortality rate")
  cleaned_data <- new_data$clean_data
  expect_equal(cleaned_data, conflict_data)
})
