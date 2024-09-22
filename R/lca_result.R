#' lca_result lca분석후 결과 정리
#'
#' @param lcadata lcadata
#' @param class_name input name
#'
#' @return data
#' @export
#'
#' @examples
#' \fontrun{
#' #'
#' jutLca3 %>%lca_result() %>%
#'   replace_df_rep( "L01", "진로목표(L01)",
#'                   "L02", "준비여부(L02)",
#'                   "L03", "직업결정(L03)",
#'                   "L04", "부모추천(L04)",
#'                   "L05", "학업성적(L05)" ) #변수명을 바꾸기
#'
#' jutLca3 %>%lca_result(class_name=c(1,2,3))
#'
#' jutLca3 %>%lca_result(class_name=c("조건부_진로지향형\n(Conditional-Oriented)",
#'                                    "불명확_진로인식형\n(Unclear-Awareness)",
#'                                    "적극적_진로준비형\n(Active-Preparedness)")
#'
#'
#'                       jutLca3 %>%lca_result(class_name=c("조건부_진로지향형",
#'                                                          "불명확_진로인식형",
#'                                                          "적극적_진로준비형"))
#'
#' }
lca_result =function(lcadata, class_name=NULL){

  Nclass = lcadata$model$C

  if(is.null(class_name)){
    class_naming = paste0("ClassName",1:Nclass)
  }else{
    class_naming = class_name
  }

  summarydata= lcadata %>%summary()%>%t()

  res =  summarydata %>%
    data.frame()%>%
    row2col("측정변수") %>%
    add_rows(c("Class Name",class_naming),
             c("샘플수", lca_class_freq(lcadata)),
             c("Gamma(𝛾)",round(lcadata$param$gamma,3)),
             row=1)

  res
}
