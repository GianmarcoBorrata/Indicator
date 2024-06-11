#' Function to apply nan inputation with linear regression
#'
#' The lm_imputation function aims to replace missing values (NA) in a dataset
#' with values estimated using a linear regression model. This technique allows
#' the existing relationships between variables in the dataset to be used to
#' accurately estimate missing values
#'
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' variables
#' @param to_impute string , name of the variables whre there are NANs to
#' impute
#' @param regressors vector of string with names of the variables to use to
#' apply linear regression imputation
#' @return It returns a dataframe with imputed values
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @keywords LM imputation
#' @importFrom stats lm
#' @examples
#'
#' data("airquality")
#' regressors<-colnames(airquality[,c(3,4)])
#' lm_imputation(data =airquality,"Ozone",regressors = regressors)
#'
#' @export lm_imputation
lm_imputation = function(data, to_impute, regressors){
  # Function to apply nan inputation with linear regression.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantitative variables;
  # 2) to_inpute: string , name of the variables whre there are NANs to impute;
  # 3) regressors: vector of string with names of the variables to use to apply linear regression imputation.
  # It returns:
  # 1) dataframe with imputed values.
  x = !is.na(data[,to_impute])
  coefficients = as.vector(lm(formula = as.vector(data[x,to_impute]) ~ as.matrix(data[x,regressors]))$coefficients)
  for(i in 1:nrow(data))
  {
    if(!x[i])
    {
      data[i,to_impute] = coefficients%*%unlist((as.vector(cbind(1,data[i,regressors]))))
    }
  }
  return (round(data,2))
}
