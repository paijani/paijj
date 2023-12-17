#' @title determine if something is likely missing not at random (MNAR)
#'
#' @param data A data frame that has the missing data, outcome variable, p value
#' @param outcome The outcome variable in the data set that dummy variables will be tested for association against
#' @param pvalue The value we're testing at for spearman rank test
#'
#'
#' @return A true or false statement on whether the spearman rank test between the outcome and dummy variable was significant.
#' @export
#'
#' @examples
#' raw_data <- data.frame(
#'   A = c(1, 3, 5, NA, 1, 2, 2,3 4, 10),
#'   B = c(NA, 2, 3, NA, 5, 9, 9, 6, 7, 1),
#'   C = c(1, 2, 3, 4, 5, 1, 1, 1, 3, 7),
#'   D = c(1, 2, NA, 4, 5, 2, 3, 2, 8, 5),
#'   E = c(1, NA, 3, 4, 5, NA, NA, 7, 8, 3)
#'   #include some binary variables
#' )
#' test <- MNAR(data, outcome = "C", p-value = 0.05) #limitation is that outcome can't have missing values
#'
library(dplyr)
library(ggplot2)
library(Hmisc)

#' Test for Missing at Random
#'
#' This function tests whether missing data is at random using logistic regression.
#'
#' @param data A data frame with missing data.
#' @param p_value The significance level for the logistic regression tests.
#'
#' @return A data frame summarizing the results of logistic regression tests.
#'
#' @export
#'

library(dplyr)

analyze_missing_data <- function(data) {

  # Step 1: Identify variables with missing data
  variables_with_missing <- colnames(data)[colSums(is.na(data)) > 0]

  # Step 2: Create a data frame with those variables
  missing_data_frame <- data[, variables_with_missing, drop = FALSE]

  # Step 3: Convert missing data into binary dummy variables
  dummy_matrix <- matrix(NA, nrow = nrow(missing_data_frame), ncol = length(variables_with_missing))
  colnames(dummy_matrix) <- paste0(variables_with_missing, '_dummy')

  for (i in seq_along(variables_with_missing)) {
    dummy_matrix[, i] <- as.integer(is.na(missing_data_frame[[variables_with_missing[i]]]))
  }

  #Merge dummy_matrix and missing_df
  var_and_dummies <- cbind(missing_data_frame, dummy_matrix)

  # Step 4: Perform logistic regression for each dummy variable with covariates from missing_data_frame
  result_matrix <- matrix(NA, nrow = length(variables_with_missing), ncol = ncol(data))
  rownames(result_matrix) <- colnames(dummy_matrix)
  colnames(result_matrix) <- colnames(data)

  for (i in seq_along(colnames(dummy_matrix))) {
    for (j in seq_along(colnames(data))) {
      formula <- as.formula(paste0(colnames(dummy_matrix)[i], ' ~ ', j))
      logistic_model <- glm(formula, data = missing_data_frame, family = "binomial")
      result_matrix[i, j] <- summary(logistic_model)$coefficients[2, 4] < 0.05  # Check if p-value is less than 0.05

    }
  }

  # Step 5: Return the significance levels matrix
  return(result_matrix)
}

# Example usage:
# Assuming your dataframe is named 'your_data_frame'
result <- analyze_missing_data(your_data_frame)
print(result)

#create an example data and add it to the package. can use one year of the armed conflict data



#1: identify variables that have missing data
#2: make a data frame that carries those variables (eg. A, B, C, D, E)
#3: convert the missing data in those variables into binary dummy variables (eg. A, B, C, D, E, A_dummy, B_dummy, C_dummy, etc.)
#4: for the dummy variable, conduct a logistic regression with each covariate (not dummy) (eg. A_dummy vs B, A_dummy vs C, A_dummy vs D, etc.)
#5: repeat #4 with each dummy variable, making seperate analyses.
#6: output the significance levels of each test in a matrix; true if significant, false if not.

