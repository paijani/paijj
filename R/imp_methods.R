#' @title Get available imputation methods based on data type
#'
#' @param data_type The type of data in the outcome column (supported data types: "binary", "numeric", "ordinal", "nominal")
#' @param level The level of imputation method ("1" or "2")
#'
#'
#' @return A character vector of available imputation methods from the mice package, including their full name and mice code
#'
#'
#'@export
#'@examples methods <- imp_methods("binary")
imp_methods <- function(data_type = "binary") {
  valid_data_types <- c("binary", "numeric", "ordinal", "nominal")

  if(!is.null(data_type) && !data_type %in% valid_data_types) {
    stop("Invalid data type. Supported types: 'binary', 'numeric, 'ordinal', 'nominal'")
  }

  if(data_type == "numeric") {
    return(c("Predictive mean matching (pmm)", "Weighted predictive mean matching (midastouch)", "Random sample (sample)", "Classification and regression trees (cart)", "Random forest (rf)", "Level-2 class predictive mean matching (2lonly.pmm)", "Level-1 normal heteroscedastic (2l.norm)", "Level-1 normal homoscedastic, linear mixed-effects model (2l.lmer)", "Level-1 normal homoscedastic, pan (2l.pan)", "Level-2 class mean (2lonly.mean)", "Level-2 class normal (2lonly.norm)", "Unconditional mean imputation (mean)", "Bayesian linear regression (norm)", "Linear regression ignoring model error (norm.nob)", "Linear regression using bootstrap (norm.boot)", "Linear regression, predicted values (norm.predict)", "	Lasso linear regression (lasso.norm)", "Lasso select + linear regression (lasso.select.norm)", "	Imputation of quadratic terms (quadratic)", "	Random indicator for nonignorable data (ri)"))
  } else if(data_type == "binary") {
    return(c("Predictive mean matching (pmm)", "Weighted predictive mean matching (midastouch)", "Random sample (sample)", "Classification and regression trees (cart)", "Random forest (rf)", "Level-2 class predictive mean matching (2lonly.pmm)", "Logistic regression (logreg)", "Logistic regression with bootstrap (logreg.boot)", "Lasso logistic regression (lasso.logreg)", "Lasso select + logistic regression (lasso.select.logreg)", "Level-1 logistic, generalized linear mixed-effects model (2l.bin)"))
  } else if(data_type == "ordinal") {
    return(c("Predictive mean matching (pmm)", "Weighted predictive mean matching (midastouch)", "Random sample (sample)", "Classification and regression trees (cart)", "Random forest (rf)", "Level-2 class predictive mean matching (2lonly.pmm)", "	Proportional odds model (polr)"))
  } else if(data_type == "nominal") {
    return(c("Predictive mean matching (pmm)", "Weighted predictive mean matching (midastouch)", "Random sample (sample)", "Classification and regression trees (cart)", "Random forest (rf)", "Level-2 class predictive mean matching (2lonly.pmm)", "Polytomous logistic regression (polyreg)", "Linear discriminant analysis (lda)"))
  }


}
