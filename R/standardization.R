#' Standardization
#'
#' It performs a standardization of data, i.e., centering and scaling, so that the
#' data is expressed in terms of standard deviation (i.e., mean = 0, SD = 1).
#' When applied to a statistical model, this function extracts the dataset,
#' standardizes it, and refits the model with this standardized version of the
#' dataset
#'
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' variables
#' @return It returns a dataframe of scaled data
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @importFrom stats sd
#' @keywords standardization
#' @examples
#'
#' data("Education")
#' Standardization=standardization(Education)
#' print(Standardization)
#'
#' @export standardization
standardization = function(data){
  # Function to standardize data.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantitative variables.
  # It returns:
  # 1) scaled_data : a dataframe of scaled data.
  sd_matrix = (sqrt(nrow(data)))/(sqrt(nrow(data)-1))*solve(diag(apply(data, 2,sd)))
  scaled_data =as.data.frame(as.matrix((data- (outer(rep(1,nrow(data)),apply(data,2, mean))))) %*% sd_matrix)
  colnames(scaled_data) = colnames(data)
  return(scaled_data)
}
