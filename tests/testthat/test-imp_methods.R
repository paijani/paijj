test_that("imp_methods works", {
  methods <- imp_methods(data_type = "binary")

  expected_methods <- c(
    "Predictive mean matching (pmm)",
    "Weighted predictive mean matching (midastouch)",
    "Random sample (sample)",
    "Classification and regression trees (cart)",
    "Random forest (rf)",
    "Level-2 class predictive mean matching (2lonly.pmm)",
    "Logistic regression (logreg)",
    "Logistic regression with bootstrap (logreg.boot)",
    "Lasso logistic regression (lasso.logreg)",
    "Lasso select + logistic regression (lasso.select.logreg)",
    "Level-1 logistic, generalized linear mixed-effects model (2l.bin)"
  )

  # Use trimws to remove leading and trailing whitespaces
  expected_methods <- trimws(expected_methods)
  actual_methods <- trimws(methods)

  expect_true(all(expected_methods %in% actual_methods), "All expected methods are present")
})
