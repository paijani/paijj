---
editor_options: 
  markdown: 
    wrap: 72
---

# Paijj

This is an R package developed and maintained by Paijani Sheth. This
package aims to increase efficiency of handling missing data during data
cleaning. Paijj provides 2 functions: missing_gone and imp_methods.

## Installment

```{r}
library(devtools)
devtools::install_github("paijani/paijj")
library(paijj)
```

# Functions

## missing_gone

Removes columns from a data set that have a certain percentage of
missing values. The percentage is customized by the user from 0-1 using
the threshold argument. The function also has the argument to exclude
columns that will remain in the new data set even if they are above
threshold. The function will return a data set with only the columns
that had percent missing below the threshold , as well as a character
vector of the column names that were removed.

### Code Example

```{r}
new_data <- missing_gone(data, threshold = 0.1)
new_data$clean_data #will return a new data frame
new_data$removed_vars #will return list of column names removed 
```

## imp_methods

Provides user with options of imputation methods available in the Mice
package, based on the data type. Available data types include numeric,
binary, nominal and ordinal. This aims to facilitate the user's
selection of an imputation model based on their data type.

```{r}
methods <- imp_methods(data_type = "binary")
methods #return list of methods
```
