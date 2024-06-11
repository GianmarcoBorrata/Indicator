#' Linear Aggregation
#'
#' This is a function to apply linear aggregation index
#'
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' variables
#' @param weights vector of weights (default all weights = 1/ ncol(dataframe))
#' @return It returns a dataframe with rows = observations and column =
#' composite indicator
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @keywords Linear Aggregation
#' @examples
#'
#' data("Education")
#' Indicator=linear_aggregation(Education)
#' print(Indicator)
#'
#' @export linear_aggregation
linear_aggregation = function(data, weights = rep(1/ncol(data),ncol(data))){
  # Function to apply linear aggregation.
  # Input:
  # 1) data : dataframe with rows = observations and columns = quantitative variables;
  # 2) weights : vector of weights (default all weights = 1/ ncol(dataframe)).
  # It returns:
  # 1) agg: dataframe with rows = observations and column = composite indicator.
  agg = as.data.frame(as.matrix(data) %*% as.matrix(weights))
  colnames(agg) = "Indicator Aggregated"
  return(agg)
}
