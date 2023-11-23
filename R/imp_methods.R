#' Get available imputation methods based on data type
#'
#' @param data_type The type of data in the outcome column (eg. "binary", "numeric", "ordered", "unordered")
#' @param methods Built-in univariate imputation methods from the mice package
#'
#' @return A character vector of available imputation methods
#' @export
#'
#' @examples methods <- get_imputation_methods("binary")
#' methods
imp_methods <- function(data_type) {
  valid_data_types <- c("binary", "numeric", "ordered", "unordered")

  if(!data_type %in% valid_data_types) {
    stop("Invalid data type. Supported types: 'binary', 'numeric, 'ordered', 'unordered")
  }

  if(data_type == "numeric") {
    return(c("pmm", "midastouch", "sample", "cart", "rf", "2lonly.pmm", "2l.norm", "2l.lmer", "2lonly.mean", "2lonly.norm", "mean", "norm", "norm.nob", "norm.boot", "norm.predict", "lasso.norm", "lasso.select.norm", "quadratic", "ri"))
  } else if(data_type == "binary") {
    return(c("pmm", "midastouch", "sample", "cart", "rf", "2lonly.pmm", "logreg", "logreg.boot","lasso.logreg","lasso.select.logreg", "2l.bin"))
  } else if(data_type == "ordered") {
    return(c("pmm", "midastouch", "sample", "cart", "rf", "2lonly.pmm", "polr"))
  } else if(data_type == "unordered") {
    return(c("pmm", "midastouch", "sample", "cart", "rf", "2lonly.pmm", "polyreg", "lda"))
  }
}
