#' nice_Var alpha분석한 경우 첫열의 a1->a
#'
#' @param data alpha data
#'
#' @return data.frame
#' @export
#'
#'
nice_Var = function(data){
  data %>% dplyr::mutate(Var= substring(Var, 1, 1))
}
