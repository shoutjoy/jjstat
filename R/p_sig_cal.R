
#' observed expect table with calculate_chi_sig
#'
#' @param obs_data matrix
#'
#' @return matrix
#' @export
#'
p_sig_cal = function(obs_data){

  res = calculate_chi_sig(obs_data, type="p_sig")
  res
}
