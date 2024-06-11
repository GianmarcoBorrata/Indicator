#' Function to evaluate different nan imputation methods
#'
#' The get_all_performance_boot function is designed to evaluate different
#' methods of imputing missing values into a dataset
#'
#' The function calculates performance metrics, such as:
#'
#' - \eqn{ R^2 = [1/N * [({\sum_{i=1}^N(P_i - (\bar{P})(O_i -
#' (\bar{O})]/\sigma_{P}*\sigma_{O}]^2}},
#'
#' - \eqn{RMSE= (1/N * ({\sum_{i=1}^N(P_i - O_i)^2)^{1/2}}}
#'
#' and
#'
#' - \eqn{MAE = 1/N * {\sum_{i=1}^N|{P_i - O_i}|}}
#'
#' for each imputation method
#'
#' Supported Imputation Methods:
#'
#' 1. Linear Regression Imputation (lm_imputation): it uses a linear regression
#' model to predict and impute missing values
#'
#' 2. Median Imputation (median_imputation): it replaces missing values with the
#' median of observed values
#'
#' 3. Mean Imputation (mean_imputation): it replaces missing values with the mean
#' of observed values
#'
#' 4. Hot Deck Imputation (hot_deck_imputation): it replaces missing values with
#' similar observed values
#'
#' 5. Expectation-Maximization Imputation (EM_imputation): it uses the
#' Expectation-Maximization algorithm to estimate and impute missing values
#'
#' It evaluate different methods of imputing missing values and calculate
#' performance metrics for each method
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' variables
#' @param to_impute string , name of the variables where there are NANs to
#' impute
#' @param regressors vector of string with names of the variables to use to
#' apply 1st,4th imputation method
#' @return It returns a performance measures dataframe with rows = methods
#' and columns = methods' performances
#' @note For the methods Median Imputation and Mean Imputation, it is not
#' possible to calculate the R^2 value.  This is because the standard deviation
#' is zero based on the following R^2 formula:
#'
#' \eqn{ R^2 = [1/N * [({\sum_{i=1}^N(P_i - (\bar{P})(O_i -
#' (\bar{O})]/\sigma_{P}*\sigma_{O}]^2}}
#'
#' where:
#'
#' - \eqn{N} is the number of imputations,
#'
#' - \eqn{O_i} are the observed data point,
#'
#' - \eqn{P_i} are the imputed data point,
#'
#' - \eqn{\bar{O}} are the average of the observed data,
#'
#' - \eqn{\bar{P}} are the average of the imputed data,
#'
#' - \eqn{\sigma_{P}} are the standard deviation of the imputed data,
#'
#' - \eqn{\sigma_{O}} are the standard deviation of the observed data
#'
#' @references  OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#'
#' @keywords imputation
#' @keywords missing value
#' @importFrom norm imp.norm
#' @examples
#'
#' data("airquality")
#' regressors<-colnames(airquality[,c(3,4)])
#' suppressWarnings(get_all_performance(data =airquality,"Ozone",regressors = regressors))
#'
#' @export get_all_performance
get_all_performance = function(data,to_impute, regressors){
  # Function to evaluate different nan imputation methods.
  # Input:
  # 1) data : dataframe with rows = observations and columns = quantitative variables;
  # 2) to_impute: string , name of the variables where there are NANs to impute;
  # 3) regressors: vector of string with names of the variables to use to apply 1st,4th imputation method.
  # It returns:
  # 1) performance: dataframe with rows = methods and columns = methods' perfomances.
  performance = data.frame()
  for (method in 1:5){
    performance = rbind(performance,performance_nan_imputation(data,to_impute, regressors, method = method))

  }
  rownames(performance) = c("lm_imputation",
                            "median imputation",
                            "mean imputation",
                            "hot deck imputation",
                            "EM imputation")
  return(performance)
}
