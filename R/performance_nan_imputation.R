#' Function to evaluate nan imputation method's performance
#'
#' This function evaluates the performance of various missing value imputation
#' methods in a quantitative dataframe. It is designed to examine and compare
#' five different imputation methods using standard performance measures
#'
#' This function is useful for comparing the effectiveness of different methods
#' of imputing missing values, allowing the most appropriate method to be
#' chosen based on measured performance
#'
#'
#' @param data A dataframe containing the observations (rows) and quantitative
#' variables (columns) to be analyzed. This dataframe includes variables with
#' missing values to be imputed
#' @param to_impute A string specifying the name of the variable in the
#' dataframe that contains the missing values to be imputed
#' @param regressors A vector of strings indicating the names of the variables
#' to be used as regressors for imputation in the case of methods 1
#' (lm_imputation) and 4 (hot deck imputation)
#' @param method An integer between 1 and 5 that specifies the imputation
#' method to be used. The supported methods are:
#'
#' 1: lm_imputation (Imputation by linear model)
#'
#' 2: median imputation (imputation by median)
#'
#' 3: mean imputation (imputation by mean)
#'
#' 4: hot deck imputation (imputation via hot deck)
#'
#' 5: EM imputation (imputation via Expectation-Maximization)
#'
#' @return The function returns a dataframe that contains a row for each
#' imputation method and columns with performance measures. The performance
#' measures included are:
#'
#' R^2: Coefficient of Determination, which measures how well the imputed values
#' fit the observed values
#'
#' RMSE: Root Mean Squared Error, which provides a measure of the mean square
#' deviation between imputed and observed values
#'
#' MAE: Mean Absolute Error, which represents the mean absolute deviation
#' between the imputed and observed values
#'
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @keywords performanca imputation
#' @keywords nan
#' @importFrom stats cor
#' @importFrom stats na.omit
#' @examples
#'
#' data("airquality")
#' regressors<-colnames(airquality[,c(3,4)])
#'
#' #---Methods 1 = Imputation by linear model
#' performance_nan_imputation(data =airquality,"Ozone",regressors = regressors,method = 1)
#'
#' #---Methods 2 = Imputation by Median
#' suppressWarnings(performance_nan_imputation(data =airquality,"Ozone",method = 2))
#'
#' #---Methods 3 = Imputation by Mean
#' suppressWarnings(performance_nan_imputation(data =airquality,"Ozone",method = 3))
#'
#' #---Methods 4 = Hot Deck imputation
#' performance_nan_imputation(data =airquality,"Ozone",regressors = regressors,method = 4)
#'
#' #---Methods 5 = Expectation-Maximization imputation
#' performance_nan_imputation(data =airquality,"Ozone",regressors = regressors,method = 5)
#'
#' @export performance_nan_imputation
performance_nan_imputation = function(data,to_impute, regressors, method = 1){
  # Function to evaluate nan imputation method's performance.
  # Input:
  # 1) data : dataframe with rows = observations and columns = quantitative variables;
  # 2) to_impute: string , name of the variables where there are NANs to impute;
  # 3) regressors: vector of string with names of the variables to use to apply 1st,4th imputation method;
  # 4) method : integer between 1 and 5 with the following meanings:
  # 1: lm_imputation;
  # 2: median imputation;
  # 3: mean imputation;
  # 4: hot deck imputation;
  # 5: EM imputation.
  # It returns:
  # 1) perf: dataframe with row = method and columns = performance's measures.
  nan_perc = round(sum(is.na(data[,to_impute]))/nrow(data),2)
  data_comp = na.omit(data)
  #ntest = round(nan_perc*nrow(data_comp),0) + 2
  ntest = max(c(round(nan_perc*nrow(data_comp),0),4))
  if (ntest<1){
    message( "added 1 to the percentage of nan values")  # when  nan %  to impute too low
    ntest = 1
  }
  test = sample(1:nrow(data_comp),ntest)
  data_comp1 = data_comp
  data_comp1[test,to_impute] = NaN
  if(method == 1){
    data_imp = lm_imputation(data_comp1,to_impute, regressors)
  }
  else if (method ==2){
    data_imp = impute_median(data_comp1)

  }
  else if (method ==3){
    data_imp = impute_mean(data_comp1)
  }
  else if (method == 4){
    data_imp = impute_hot_deck_in_classes(data_comp1,regressors)
  }
  else if (method ==5){
    data_comp2=data_comp1[,c(to_impute,regressors)]
    data_imp = impute_EM(data_comp2)
  }

  else{
    print("Not valid method")
  }
  r2 = (cor(data_imp[test,to_impute],data_comp[test,to_impute]))^2
  rmse = (1/length(test)*sum((data_imp[test,to_impute]-data_comp[test,to_impute])^2))^(0.5)
  mae = 1/length(test)*sum(abs(data_imp[test,to_impute]-data_comp[test,to_impute]))
  perf = data.frame(t(c(r2,rmse,mae)), row.names = paste0("method",method))
  colnames(perf) = c("R2","rmse","mae")
  return(perf)
}
