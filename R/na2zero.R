#' NA replace to value default 0
#'
#' @param data data.frame or matrix
#' @param imp imputation value default 0
#'
#' @return dat
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data %>% na2zero()
#' data %>% na2zero(0)
#' data %>% na2zero(1)
#' }
#'
na2zero= function(data, imp=0){
  data[is.na(data)] <- imp
  as.data.frame(data)
  # data
}
