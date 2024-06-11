standardization_AMPI_Adjusted_neg = function(data){
  # Function to normalization of data with Adjusted Maziotta-Pareto index with negative sign.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantitative variables.
  # It returns:
  # 1) normalized_data : a dataframe of normalized data.
  min_max_AMPI_neg_1col = function(col){
    minimo = min(col)
    massimo = max(col)
    return(60* (1/(massimo-minimo)) *(-col + massimo) + 70)
  }
  return(apply(data,2,min_max_AMPI_neg_1col))
}
