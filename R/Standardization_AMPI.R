#' Standardization of data with Adjusted Maziotta-Pareto index
#'
#' This is a function that standardized the data with Adjusted Maziotta-Pareto
#' index with positive or negative polarity
#'
#' The ‘polarity’ of an indicator is the sign of the relation between the
#' indicator and the phenomenon to be measured ( + if the indicator represents
#' a dimension considered positive and - otherwise)
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' variables
#' @param pol polarity if not selected is "positive"", otherwise write "neg"
#' (see details)
#' @return It returns a dataframe of normalized data
#' @references Matteo Mazziotta & Adriano Pareto, 2018. "Measuring Well-Being
#' Over Time: The Adjusted Mazziotta–Pareto Index Versus Other Non-compensatory
#' Indices," Social Indicators Research: An International and Interdisciplinary
#' Journal for Quality-of-Life Measurement, Springer, vol. 136(3), pages
#' 967-976, April
#' @keywords Standardization AMPI
#' @examples
#'
#' data("Education")
#' Standardization_AMPI=Standardization_AMPI(Education)
#' print(Standardization_AMPI)
#'
#' #----With negative polarity
#' Standardization_AMPI_neg=Standardization_AMPI(Education,"neg")
#' print(Standardization_AMPI_neg)
#'
#' @export Standardization_AMPI
Standardization_AMPI = function(data,pol="pos"){
  # Function to normalization of data with Adjusted Maziotta-Pareto index with positive or negative polarity .
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantitative variables.
  # 2) pol if not select is positive, otherwise write neg
  # It returns:
  # 1) normalized_data : a dataframe of normalized data.
  if(pol == "pos"){
    return(standardization_AMPI_Adjusted_pos(data))
  }
  else{
    return(standardization_AMPI_Adjusted_neg(data))
  }
}
