#' Mazziotta-Pareto index
#'
#' The Mazziotta–Pareto index (MPI) is a composite index for summarizing a set
#' of individual indicators that are assumed to be not fully substitutable. It
#' is based on a non-linear function which, starting from the arithmetic mean
#' of the normalized indicators, introduces a penalty for the units with
#' unbalanced values of the indicators
#'
#' The ‘polarity’ of an indicator is the sign of the relation between the
#' indicator and the phenomenon to be measured ( + if the indicator represents
#' a dimension considered positive and - otherwise)
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' @param pol polarity if not selected is positive, otherwise write neg (see
#' details)
#' @return It returns a dataframe with rows = observations and column =
#' composite indicator
#' @references De Muro P., Mazziotta M., Pareto A. (2011), "Composite Indices
#' of Development and Poverty: An Application to MDGs", Social Indicators
#' Research, Volume 104, Number 1, pp. 1-18
#' @keywords MPI
#' @keywords Mazziotta-Pareto Index
#' @examples
#'
#' data("Education")
#' Indicator=linear_aggregation_MPI(Education)
#' print(Indicator)
#'
#' #----With negative polarity
#' Indicator_neg=linear_aggregation_MPI(Education,"neg")
#' print(Indicator_neg)
#'
#'
#' @export linear_aggregation_MPI
linear_aggregation_MPI = function(data,pol="pos"){
  # Function to apply linear aggregation to Mazziotta-Pareto index.
  # Input:
  # 1) data : dataframe with rows = observations and columns = quantitative variables;
  # 2) pol if not select is positive, otherwise write neg
  # It returns:
  # 1) agg: dataframe with rows = observations and column = composite indicator.

  data = standardization_MPI(as.data.frame(data))
  if(pol == "pos") {
    mean=apply(data,1,mean)
    sd = apply(data,1,sd)
    cv=sd/mean
    agg=as.data.frame(mean-(sd*cv))

  } else{
    mean=apply(data,1,mean)
    sd = apply(data,1,sd)
    cv=sd/mean
    agg=as.data.frame(mean+(sd*cv))

  }
  colnames(agg) = "MPI Indicator"
  return(agg)
}
