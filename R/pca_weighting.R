#' Function that weight the quantitative variable by PCA method
#'
#' The pca_weighting function is designed to perform a principal component
#' analysis (PCA) on the input data to calculate weights that correct for
#' overlapping information between related indicators. This process makes it
#' possible to create a composite indicator that captures as much information
#' as possible from individual indicators while reducing the dimensionality of
#' the data
#'
#'
#' @param data dataframe with rows = observations and columns = quantitative
#' variables
#' @return It returns a dataframe with rows = observations and column =
#' composite indicator
#' @references OECD/European Union/EC-JRC (2008), Handbook on Constructing
#' Composite Indicators: Methodology and User Guide, OECD Publishing, Paris,
#' <https://doi.org/10.1787/9789264043466-en>
#' @keywords PCA indicator
#' @keywords PCA weighting
#' @importFrom FactoMineR PCA
#' @examples
#'
#' data("Education")
#' Indicator_pca=pca_weighting(Education)
#' print(Indicator_pca)
#' @export pca_weighting
pca_weighting = function(data){
  # Function that weight the quantitative variable by PCA method.
  # Input :
  # 1)  data : dataframe with rows = observations and columns = quantitative variables,
  # it returns:
  # 1) datafrmae : dataframe with rows = observations and column = composite indicator.
  pca = PCA(data, graph = FALSE)
  dataframe =  data.frame(pca$ind$coord[,1], row.names = rownames(data))
  colnames(dataframe) = c("Composite Indicator")
  return(dataframe)
}


