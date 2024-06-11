#' Ranking Aggregation
#'
#' It's a function to apply ranking aggregation. The highest value is the first
#' value of rank
#'
#'
#' @param data dataframe with rows = observations and columns = quantiative
#' variables
#' @return It returns a dataframe with rows = observations and column =
#' composite indicator
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @keywords Rank aggregation
#' @examples
#'
#' data("Education")
#' Indicator_rank=rank_aggregation(Education)
#' print(Indicator_rank)
#'
#' @export rank_aggregation
rank_aggregation = function(data){
  # Function to apply ranking aggregation.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantiative variables.
  # It returns:
  # 1) dataframe : dataframe with rows = observations and column = composite indicator.
  dataframe = as.data.frame(apply(rank_normalisation(data), 1, sum),
                            row.names = rownames(data))
  colnames(dataframe) = "Composite indicator"
  return(dataframe)
}
