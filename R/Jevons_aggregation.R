#' Jevons static aggregation
#'
#' The Jevons_aggregation function computes an index using the Jevons method of
#' static aggregation. This method calculates the geometric mean, multiplied
#' for 100.
#'
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' variables
#' @return It returns a dataframe with rows = observations and column =
#' composite indicator
#' @references Massoli, P., Mazziotta, M., Pareto, A., Rinaldelli, C. (2013).
#' Synthesis Methodologies and Spatial Analysis. Composite indices for BES,DAYS
#' OF RESEARCH IN ISTAT, NOVEMBER 10-11, 2014
#' @keywords Jevons
#' @examples
#'
#' data("Education")
#' Indicator=Jevons_aggregation(Education)
#' print(Indicator)
#'
#' @export Jevons_aggregation
Jevons_aggregation = function(data){
  # Function to apply Jevons static aggregation.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantitative variables;
  # It returns:
  # 1) dataframe with rows = observations and column = composite indicator.
  mean_diag=diag(apply(data,2,mean))
  data1=(100*as.matrix(data)%*%mean_diag)^(1/ncol(data))
  return(apply(data1,1, prod))
}
