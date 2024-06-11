#' Rank normalization
#'
#' It's a function that normalize by ranking method. Create the matrix of
#' ranking for different columns, the rank is the high value is the first
#'
#'
#' @param data dataframe with rows = observations and columns = quantiative
#' variables
#' @return It returns a datafame of normalized data
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @keywords Rank normalization
#' @examples
#'
#' data("Education")
#' Normalized_rank=rank_normalisation(Education)
#' print(Normalized_rank)
#'
#' @export rank_normalisation
rank_normalisation = function(data){
  # Function to normalize by ranking method.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantiative variables.
  # It returns:
  # datafame of normalized data.
  return(apply(-data,2,rank))
}
