
#' lcaGraphData, lcadata to graph data
#'
#' @param lcaResult lcadata
#' @param ... iteam names, nothng auto
#' @param yes_col yes col , default 3
#' @param yes yes name default `Y = 2`
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' #함수를 이용하여 그래프 데이터 생성
#' jutLca3 %>%lcaGraphData()
#'
#' jutLca3 %>%lcaGraphData( "학업성적"="L05",
#'                          "부모지지"="L04",
#'                          "진로결정"="L03",
#'                          "준비미흡"="L02",
#'                          "목표확실"="L01")
#'
#' }
#'
lcaGraphData =function(lcaResult, ..., yes_col=3, yes=`Y = 2`){

  nclass = lcaResult$model$C
  Colnames = jutLca3$var.names$y.names

  summary_IRP = lcaResult$param$rho$ALL %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    rownames_to_column() %>% dplyr::select(1, all_of(yes_col)) %>%
    mutate(Class = rep( str_c("Class",1:nclass),
                        lcaResult$var.names$y.names %>%length() ),
           item = rep(Colnames, each= nclass)
    ) %>%
    dplyr::select(2, 4, 3) %>%
    tibble() #%>%
  #  rename(YES = yes)
  colnames(summary_IRP) = c("YES", "item",  "Class",  "item_cluster")
  check = c(...)

  if(is.null(check)){
    summary_IRP = summary_IRP%>%
      mutate( item_cluster = item)
  }else{
    summary_IRP = summary_IRP %>%
      mutate(
        item_cluster = fct_recode(item, ...  ),
        item = fct_reorder(item, as.numeric(factor(item))))

  }
  return(summary_IRP)
}



#' lcaGraphData, lcadata to graph data
#'
#' @param lcaResult lcadata
#' @param ... iteam names, nothng auto
#' @param yes_col yes col , default 3
#' @param yes yes name default `Y = 2`
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' #함수를 이용하여 그래프 데이터 생성
#' jutLca3 %>%lcaGraphData()
#'
#' jutLca3 %>%lcaGraphData( "학업성적"="L05",
#'                          "부모지지"="L04",
#'                          "진로결정"="L03",
#'                          "준비미흡"="L02",
#'                          "목표확실"="L01")
#'
#' }
#'
lca_GraphData =function(lcaResult, ..., yes_col=3, yes=`Y = 2`){

  nclass = lcaResult$model$C
  Colnames = jutLca3$var.names$y.names

  summary_IRP = lcaResult$param$rho$ALL %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    rownames_to_column() %>% dplyr::select(1, all_of(yes_col)) %>%
    mutate(Class = rep( str_c("Class",1:nclass),
                        lcaResult$var.names$y.names %>%length() ),
           item = rep(Colnames, each= nclass)
    ) %>%
    dplyr::select(2, 4, 3) %>%
    tibble() #%>%
  #  rename(YES = yes)
  colnames(summary_IRP) = c("YES", "item",  "Class",  "item_cluster")
  check = c(...)

  if(is.null(check)){
    summary_IRP = summary_IRP%>%
      mutate( item_cluster = item)
  }else{
    summary_IRP = summary_IRP %>%
      mutate(
        item_cluster = fct_recode(item, ...  ),
        item = fct_reorder(item, as.numeric(factor(item))))

  }
  return(summary_IRP)
}
