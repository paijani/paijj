#' @title remove missing data based on percent thresholds defined by user
#'
#' @param data A data frame that has the missing data
#' @param threshold The threshold that represents the percentage of missing data in the variable.
#' Variables with missing data percent frequency above this threshold will be removed.
#' @param exclude A variable is deemed important in data set and will not want removed, even if percent missing is above threshold
#' @return A cleaner data frame with variables removed based on the threshold and a character vector of variables that were removed.
#' @export
#' @examples Using the "here" package to locate the example file:
#' library(here)
#' library(dplyr)
#' data <- read.csv(here("data-raw", "conflict_data.csv"))
#' Write in the function:
#' data <- missing_gone(data, threshold = 0.1, exclude = "Maternal Mortality Rate")

missing_gone <- function(data, threshold = 0.1, exclude = NULL) {

  #load dplyr
  library(dplyr)

  #identify the variables
  variables <- colnames(data)

  # Identify variables with at least one missing value
  variables_with_missing <- names(which(colSums(is.na(data)) > 0))

  #Initialize vector to store removed variables
  removed_vars <- character(0)

  # Iterate through variables with missing values
  for (variables in variables_with_missing) {

    # Skip the variable if it's specified in 'exclude'
    if (variables %in% exclude) {
      next
    }

    missing_ratio <- sum(is.na(data[[variables]])) / length(data[[variables]])

    #if missingness is above threshold, remove from data set
    if (missing_ratio > threshold) {
      data <- data %>% select(-{{variables}})
      removed_vars <- c(removed_vars, variables)
    }

  }

  # Return the updated dataset
  return(list(clean_data = data, removed_vars = removed_vars))
}

