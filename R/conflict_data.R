#' Example data to demonstrate the missing_gone function
#'
#' Data for 181 countries from the Uppsala Conflict Data Program and World Bank for the years 2000 to 2019.
#' The dataset includes various socio-economic indicators and various mortality rates.
#'
#' @docType data
#' @format A data frame with 3720 rows and 19 columns:
#' \describe{
#'    \item{country_name}{Country Name}
#'    \item{ISO}{International Organization of Standardization codes for countries and their subdivisions}
#'    \item{region}{the broad area in the world}
#'    \item{year}{year data in that row was taken}
#'    \item{GDP}{Gross Domestic Product per capital (USD)}
#'    \item{OECD}{Whether the country was a member of the Organization for Economic Co-operation and Development}
#'    \item{popdens}{percentage of population living in a density of >1000 people/km^2}
#'    \item{urban}{percentage of population living in urban areas}
#'    \item{male_edu}{years per capita of male education, standardizes by age}
#'    \item{temp}{mean population-weighted annual temperature in Celsius}
#'    \item{Maternal Mortality rate}{Maternal mortality ratio (modeled estimate, per 100,000 live births)}
#'    \item{Infant mortality rate}{Number of infant deaths (per 1000 live births)}
#'    \item{Neonatal mortality rate}{Number of neonatal deaths (per 1000 live births)}
#'    \item{Under 5 mortality rate}{Number of under 5 years of age deaths (per 1000 live births)}
#'    \item{Drought}{Whether drought occured}
#'    \item{Earthquake}{Whether earthquake occured}
#'    \item{totdeath}{Total number of deaths}
#'    \item{armconfl}{Whether there was armed conflict (greater than or equal to 25 battle-related deaths) of not (less than 25 battle related deaths)}
#'    }
#' @source https://pubmed.ncbi.nlm.nih.gov/34582455/
#' @references Jawad, M., Hone, T., Vamos, E.P., Cetorelli, V., Millett, C. (2021).Implications of armed conflict for
#'  maternal and child health: A regression analysis of data from 181 countries for 2000-2019. PLoS Medicine, 18(9),
#'  doi: 10.1371/journal.pmed.1003810.
#'
#' Test the missing_gone function to remove variables with missing values over a certain defined threshold
#' This will remove columns that have more than 10% missing data in the conflict_data dataframe, with the exception of "Maternal Mortality Rate" if it's above threshold.
#'
#'
#' Test missing_gone by loading the data frame. Then within the function, define the data frame, threshold of percent missing, and any variables that we want to exclude from this function.
#' @examples
#' install.packages("here")
#' library(here)
#' data <- here("data-raw", "conflict_data.csv")
#' conflict_data_new <- missing_gone(data, threshold = 0.1, exclude = "Maternal Mortality Rate")
#' End of Data Documentation
"conflict_data"

