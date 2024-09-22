#' lca_gamma 사전확률
#'
#' @param lcadata  lcadata
#'
#' @return vedtor
#' @export
#'
#' @examples
#' \dontrun{
#' jutLca3%>% lca_gamma()
#' }
lca_gamma = function(lcadata){

  # freq = lca_class_freq(lcadata)
  gamma =  lcadata$param$gamma

  return(gamma)

}
