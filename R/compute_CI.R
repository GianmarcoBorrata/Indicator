#' Calculation of Condition Indices
#'
#' Diagnosis of collinearity in matrix \code{X}
#'
#' Collinearities can inflate the variance of the estimated regression
#' coefficients and numerical stability. The condition indices are calculated
#' by the eigenvalues of the crossproduct matrix of the scaled but uncentered
#' explanatory variables. Indices > 30 may indicate collinearity
#'
#' @param matrix a matrix of data where rows = observations and columns =
#' variables
#' @return It returns the condition index of the matrix
#' @references Belsley, D. , Kuh, E. and Welsch, R. E. (1979), Regression
#' Diagnostics: Identifying Influential Data and Sources of Collinearity, John
#' Wiley (New York)
#' @importFrom stats cor
#' @keywords collinearity ConditionIndex
#' @examples
#'
#' data("Education")
#' compute_CI(Education)
#'
#' @export compute_CI
compute_CI = function(matrix){
  #Function that takes as input:
  #1) matrix: a matrix of data where rows = observations and columns = variables.
  #It returns the condition index of the matrix.
  corr = cor(matrix)
  eigenCorX<-eigen(corr)
  CI<-sqrt(eigenCorX$values[1]/abs(eigenCorX$values[dim(matrix)[2]]))
  return(CI)
}
