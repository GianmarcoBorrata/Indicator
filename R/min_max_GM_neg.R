min_max_GM_neg = function(data){
  # This is a data normalization function for the geometric mean, where we multiplied the normalized data by 198 and add 1.
  # Input :
  # 1) data : dataframe with rows = observations and columns = quantiative variables.
  # It returns:
  # 1) norm_data : datafame of normalized data.
  min_max_GM_neg_1col = function(col){
    minimo = min(col)
    massimo = max(col)
    return(198* (1/(massimo-minimo)) *(-col + massimo) + 1)
  }
  return(apply(data,2,min_max_GM_neg_1col))
}
