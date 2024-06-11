#' Mean absolute difference of rank
#'
#' Function to calculate the mean absolute difference of rank for different
#' methods
#'
#' Function to calculate the mean absolute difference of rank for different
#' methods. Create the matrix of ranking for different columns, the rank is the
#' high value is the first. Calculate the different in absolute values for
#' different columns and calculate the mean for different methods
#'
#' @param matrix_data data matrix of indicator
#' @return It returns a data frame of mean absolute difference of rank for
#' different methods
#' @references Matteo Mazziotta & Adriano Pareto, 2018. "Measuring Well-Being
#' Over Time: The Adjusted Mazziotta–Pareto Index Versus Other Non-compensatory
#' Indices," Social Indicators Research: An International and Interdisciplinary
#' Journal for Quality-of-Life Measurement, Springer, vol. 136(3), pages
#' 967-976, April
#' @keywords MAD
#' @keywords Mean Absolute Difference
#' @examples
#'
#' data("Education")
#' Indicator_MPI=linear_aggregation_MPI(Education)
#' Indicator_AMPI=linear_aggregation_AMPI(Education)
#' Indicator_GA=geometric_aggregation(Education)
#' All_Indicator=cbind(Indicator_MPI,Indicator_AMPI,Indicator_GA)
#' MAD=MAD(All_Indicator)
#' print(MAD)
#'
#' @export MAD
MAD <- function(matrix_data) {
  # Function to calculate the mean absolute difference of rank for different methods.
  # Create the matrix of ranking for different columns, the rank is the high value is the first
  # Calculate the different in absolute values for different columns
  # Calculate the mean for different methods
  # Input :
  # 1) data : matrix with rows = observations and columns = quantitative variables.
  # It returns:
  # 1) Mean abs different matrix : a data frame of mean absolute difference of rank for different methods.
  matrix_data=apply(-matrix_data,2,rank)
  n_cols <- ncol(matrix_data)
  mean_abs_diff_matrix <- matrix(NA, nrow = n_cols, ncol = n_cols)
  calculate_mean_abs_difference <- function(x, y) {
    mean_abs_diff <- mean(abs(x - y))
    return(mean_abs_diff)
  }
  for (i in 1:n_cols) {
    for (j in 1:n_cols) {
      mean_abs_diff_matrix[i, j] <- calculate_mean_abs_difference(matrix_data[, i], matrix_data[, j])
    }
  }
  rownames(mean_abs_diff_matrix) <- colnames(matrix_data)
  colnames(mean_abs_diff_matrix) <- colnames(matrix_data)
  mean_abs_diff_matrix=round(mean_abs_diff_matrix,2)
  mean_abs_diff_matrix=as.data.frame(mean_abs_diff_matrix)
  return(mean_abs_diff_matrix)
}
