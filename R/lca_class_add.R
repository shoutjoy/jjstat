#' lca_class_add,기존데이터에 잠재클래스 추가
#'
#' @param data df
#' @param lcadata lcadata
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' jut6 %>% lca_class_add(jutLca3) %>% head()
#' }
lca_class_add = function(data, lcadata){

  Newdata = dplyr::bind_cols( data,    #데이터
                              round(lcadata$post$ALL, 3)*100  #잠재클래스 3개
  )

  Newdata = Newdata %>% #rowid_to_column(var="응답자") %>%
    dplyr::mutate( 잠재계층 = apply(jutLca3$post$ALL, 1, which.max)
    ) %>%
    dplyr::mutate( LCA_Class = paste0("Class", 잠재계층))

  return(Newdata)

}
