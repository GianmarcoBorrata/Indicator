#' Adjusted Mazziotta-Pareto index
#'
#' The Adjusted Mazziotta-Pareto Index (AMPI) is a composite index for
#' summarizing a set of indicators that are assumed to be non-substitutable,
#' i.e., all components must be balanced. It is based on a non-linear function
#' which, starting from the arithmetic mean, introduces a penalty for the units
#' with unbalanced values of the indicators
#'
#' The ‘polarity’ of an indicator is the sign of the relation between the
#' indicator and the phenomenon to be measured ( + if the indicator represents
#' a dimension considered positive and - otherwise)
#'
#' @param data dataframe with rows = observations and columns = quantitative variables
#' @param pol pol if not selected is "positive"", otherwise write "neg" (see details)
#' @return It returns a dataframe with rows = observations and column = composite indicator
#' @references Matteo Mazziotta & Adriano Pareto, 2018. "Measuring Well-Being
#' Over Time: The Adjusted Mazziotta–Pareto Index Versus Other Non-compensatory
#' Indices," Social Indicators Research: An International and Interdisciplinary
#' Journal for Quality-of-Life Measurement, Springer, vol. 136(3), pages
#' 967-976, April
#' @keywords Adjusted Mazziotta-Pareto Index
#' @keywords AMPI
#' @examples
#'
#' data("Education")
#' Indicator=linear_aggregation_AMPI(Education)
#' print(Indicator)
#'
#' #----With negative polarity
#' Indicator_neg=linear_aggregation_AMPI(Education,"neg")
#' print(Indicator_neg)
#'
#' @export linear_aggregation_AMPI
linear_aggregation_AMPI = function(data,pol="pos"){
  # Function to apply linear aggregation to Adjusted Mazziotta-Pareto index.
  # Input:
  # 1) data : dataframe with rows = observations and columns = quantitative variables;
  # 2) pol if not select is positive, otherwise write neg
  # It returns:
  # 1) agg: dataframe with rows = observations and column = composite indicator.

  data = (as.data.frame(data))
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
  colnames(agg) = "AMPI Indicator"
  return(agg)
}
