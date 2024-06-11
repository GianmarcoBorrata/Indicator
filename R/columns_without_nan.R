#' Function to get the names of the columns with NAN values
#'
#' This function identifies and returns the names of the columns in a DataFrame
#' that contain missing values (NaN). It is particularly useful for missing
#' data imputation and preliminary analysis, allowing for quick identification
#' of columns that need to be handled due to the presence of missing values
#'
#' @param data The DataFrame's rows represent observations and the columns
#' represent variables
#' @return It returns a vector of columns with NAN
#' @importFrom missMethods impute_EM
#' @importFrom missMethods impute_median
#' @importFrom missMethods impute_mean
#' @importFrom missMethods impute_hot_deck_in_classes
#' @keywords data manipulation missing values
#' @examples
#'
#' data("airquality")
#' columns_with_nan(airquality)
#'
#' @export columns_with_nan
columns_with_nan = function(data){
  # Function to get the names of the columns with NAN values.
  # Input:
  # 1: dataframe : rows = observations and columns = variables.
  # It returns a vector of columns with NAN
  columns = names(data)
  with_nan = apply(apply(data,2,is.na),2,any)
  output=apply(apply(data[,columns[with_nan]],2,is.na),2,sum)
  return(output)
}


