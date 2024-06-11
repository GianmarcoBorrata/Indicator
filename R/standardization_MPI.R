#' Standardization of data with Maziotta-Pareto index
#'
#' This is a function that standardized the data with Maziotta-Pareto index
#'
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' variables
#' @return It returns a dataframe of normalized data
#' @references De Muro P., Mazziotta M., Pareto A. (2011), "Composite Indices
#' of Development and Poverty: An Application to MDGs", Social Indicators
#' Research, Volume 104, Number 1, pp. 1-18
#' @importFrom stats sd
#' @keywords Standardization MPI
#' @examples
#' data("Education")
#' Standardization_MPI=standardization_MPI(Education)
#' print(Standardization_MPI)
#'
#' @export standardization_MPI
standardization_MPI = function(data){
  # Function to normalization of data with Maziotta-Pareto index.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantitative variables.
  # It returns:
  # 1) normalized_data : a dataframe of normalized data.
  sd_matrix = 10*(sqrt(nrow(data)))/(sqrt(nrow(data)-1))*solve(diag(apply(data, 2,sd)))
  normalized_data =as.data.frame(as.matrix((data- (outer(rep(1,nrow(data)),apply(data,2, mean))))) %*% sd_matrix)
  normalized_data=matrix(100,nrow=nrow(data),ncol=ncol(data))+normalized_data
  colnames(normalized_data) = colnames(data)
  return(normalized_data)
}
