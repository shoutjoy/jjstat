#' lca_class_freq cal
#'
#' @param lacdata lca result
#' @param show df, ""
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  jutLca3 %>%lca_class_freq()%>% data.frame()
#' }
lca_class_freq = function(lacdata, show=""){
  res = apply(lacdata$post$ALL, 1, which.max)%>% table()

  if(show =="df"){
    res = data.frame(res)
  }else{
    res = res
  }
  return(res)
}
