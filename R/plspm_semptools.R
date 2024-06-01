#' plspm_indicator_order
#'
#' @param plsres_boot  plsres_boot
#'
#' @return plsres_boot
#' @export
#'
#' @examples
#' \dontrun{
#' plspm_indicator_order(nfl_pls_boot)
#' }
#'
plspm_indicator_order = function(plsres_boot){
  res = plsres_boot$model$gens$mvs_names%>%unique()
  res
}




#' plspm_indicator_factor
#'
#' @param plsres_boot  plsres_boot
#'
#' @return plsres_boot
#' @export
#'
#' @examples
#' \dontrun{
#' plspm_indicator_factor(nfl_pls_boot)
#' }
plspm_indicator_factor  = function(plsres_boot){
  res =plsres_boot$model$gens$lvs_names
  res
}




##
#' layout_mat_name
#'
#' @param plsres_boot  plsres_boot
#'
#' @return plsres_boot
#' @export
#'
#' @examples
#' \dontrun{
#' layout_mat_name(nfl_pls_boot)
#' }
layout_mat_name= function(plsres_boot){
  mres = unique(plsres_boot$model$gens$mvs_names)
  lres = plsres_boot$model$gens$lvs_names
  Res = c(mres, lres)
  Res
}
