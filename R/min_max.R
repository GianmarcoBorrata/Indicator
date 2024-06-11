#' Min-max normalization
#'
#' Min-max normalization transforms each value by subtracting its minimum and
#' dividing by its range (maximum-minimum). The result is a new variable with a
#' minimum of zero and a maximum of one
#'
#' Change the value of variable to negative if it has negative polarity
#'
#' @param data dataframe with rows = observations and columns = quantiative
#' variables
#' @return It returns a datafame of normalized data
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @keywords min-max
#' @keywords normalization
#' @examples
#'
#' data("Education")
#' Normalization=min_max(Education)
#' print(Normalization)
#'
#' @export min_max
min_max = function(data){
  # Function to normalize data within 0 and 1.
  # Input :
  #1) data : dataframe with rows = observations and columns = quantiative variables.
  # It returns:
  # 1) norm_data : datafame of normalized data.
  norm_data = as.data.frame(as.matrix(data - outer(rep(1,nrow(data)),apply(data,2,min))) %*% as.matrix(solve(diag(apply(data,2,max) - apply(data,2,min)))))
  colnames(norm_data) = colnames(data)
  return(norm_data)
}
