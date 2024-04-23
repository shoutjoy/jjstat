#
#' Observation Expectation Table with calculate_chi_sig
#'
#' @param obs_data matrix
#'
#' @return matrix
#' @export
#'
obs_exp_table = function(obs_data){
  obs_data = as.matrix(obs_data)
  res = calculate_chi_sig(obs_data, type="observed_over_expected_ratios")
  res
}

