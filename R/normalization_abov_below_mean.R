#' Normalization above or below the mean
#'
#' This transformation considers the indicators which are above and below an
#' arbitrarily defined threshold, p, around the mean. The threshold p builds a
#' neutral region around the mean, where the transformed indicator is zero.
#' This reduces the sharp discontinuity, from -1 to +1, which exists across the
#' mean value to two minor discontinuities, from -1 to 0 and from 0 to +1,
#' across the thresholds
#'
#' This function to normalize in -1, 0, and 1
#'
#' @param data dataframe with rows = observations and columns = quantiative
#' variables
#' @param p threshold for the window
#' @return It returns a datafame of normalized data
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @keywords normalization
#' @keywords above and below mean
#' @examples
#'
#' data("Education")
#' Indicator=normalization_abov_below_mean(Education)
#' print(Indicator)
#'
#' #---With different threshold
#' Indicator=normalization_abov_below_mean(Education,p=0.1)
#' print(Indicator)
#'
#'
#' @export normalization_abov_below_mean
normalization_abov_below_mean = function(data,p=0.01){
  # Function to normalize in -1, 0, and 1
  # Input:
  # 1) data : dataframe with rows = observations and columns = quantiative variables.
  # It returns:
  # 1) datafame of normalized data.
  normalization_abov_below_mean_var = function(variable){
    # Function to normalize a variable to  -1 or 0 or 1.
    # Input :
    # 1) variable: quantitative column of a dataframe;
    # 2) p : threshold for the window.
    # It returns:
    # 1) variable : normalized variable
    w = 1/mean(variable)*variable
    for (i in 1:length(w)){
      if(w[i]> 1 + p){
        variable[i] = 1
      }
      else if (w[i]< 1-p){
        variable[i] =  -1
      }
      else{
        variable[i] = 0
      }
    }
    return(variable)
  }

return( apply(data,2,normalization_abov_below_mean_var))
}
