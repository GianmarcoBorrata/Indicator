#' Geometric Aggregation
#'
#' The purpose of the Geometric Aggregation function is to calculate a
#' synthetic index based on the geometric mean for a set of variables
#'
#' This is simply the product of each indicator to the power of its weight, all
#' raised the the power of the inverse of the sum of the weights
#'
#' The geometric mean is less compensatory than the arithmetic mean – low
#' values in one indicator only partially substitute high values in others. For
#' this reason, the geometric mean may sometimes be preferred when indicators
#' represent “essentials”. An example might be quality of life: a longer life
#' expectancy perhaps should not compensate severe restrictions on personal
#' freedoms
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' @param weights vector of weights (default all weights = 1/ ncol(dataframe))
#' @param geo_wo (geometric mean workaround to deal with negative values),
#' positive number to make all variables positive
#' @return It returns a dataframe with rows = observations and column =
#' composite indicator
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @keywords Geometric Aggregation
#' @examples
#'
#' data("Education")
#' Indicator=geometric_aggregation(Education)
#' print(Indicator)
#'
#' #-----When there are negative values
#' set.seed(123)
#' Data=matrix(rnorm(100),nrow = 10,ncol = 10)
#' Indicator=geometric_aggregation(Data,geo_wo = 100)
#' print(Indicator)
#'
#' @export geometric_aggregation
geometric_aggregation = function(data, weights = rep(1/ncol(data), ncol(data)), geo_wo = 0){
  # Function to apply geometric aggregation.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantitative variables;
  # 2) weights : vector of weights (default all weights = 1/ ncol(dataframe)).
  # 3) geo_wo : (geometric mean workaround to deal with negative values),  positive number to make all
  # variables positive.
  # It returns:
  # 1) dataframe with rows = observations and column = composite indicator.
  data = apply(data,c(1,2), function(x){return(x+geo_wo)})
  v = c()
  for (i in 1:ncol(data)){
    v = cbind(v, data[,i]^weights[i])
  }
  return(apply(v,1, prod)-geo_wo)
}
