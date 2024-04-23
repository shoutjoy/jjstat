#' Observation Expectation Table with calculate_chi_sig
#'
#' @param obs_data matrix
#'
#' @return matrix
#' @export
#'
p_value_cal = function(obs_data){
  obs_data = as.matrix(obs_data)
  res = calculate_chi_sig(obs_data, type="cell_p_values")
  res
}
