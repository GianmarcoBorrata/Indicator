#' Normalization for the Geometric Mean
#'
#' This is a data normalization function for the geometric mean, where we
#' multiplied the normalized data by 198 and add 1, with positive or negative
#' polarity
#'
#' The ‘polarity’ of an indicator is the sign of the relation between the
#' indicator and the phenomenon to be measured ( + if the indicator represents
#' a dimension considered positive and - otherwise)
#'
#' @param data dataframe with rows = observations and columns = quantiative
#' @param pol polarity if not selected is "positive"", otherwise write "neg"
#' @return It returns a datafame of normalized data
#' @references Massoli, P., Mazziotta, M., Pareto, A., Rinaldelli, C. (2013).
#' Synthesis Methodologies and Spatial Analysis. Composite indices for BES,DAYS
#' OF RESEARCH IN ISTAT, NOVEMBER 10-11, 2014
#' @keywords min-max geometric mean
#' @examples
#'
#' data("Education")
#' Normalization=min_max_GM(Education)
#' print(Normalization)
#'
#' #----With negative polarity
#' Normalization_neg=linear_aggregation_AMPI(Education,"neg")
#' print(Normalization_neg)
#'
#' @export min_max_GM
min_max_GM = function(data, pol="pos"){
  # This is a data normalization function for the geometric mean, where we multiplied the normalized data by 198 and add 1.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantiative variables.
  # 2) pol if not select is positive, otherwise write neg
  # It returns:
  # 1) norm_data : datafame of normalized data.
  if(pol == "pos"){
    return(min_max_GM_pos(data))
  }
  else{
    return(min_max_GM_neg(data))
  }
}
