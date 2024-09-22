#' lca_gof
#'
#' @param ... models
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 함수 사용 예시
#' lca_gof(jutLca2, jutLca3, jutLca4)
#'
#' lca_gof(jutLca3)%>%row2col()
#' }
lca_gof <- function(...){
  # 여러 모델 데이터를 리스트로 받음
  lcadata_list <- list(...)

  # 각 모델에 대해 적합도(gof) 데이터를 추출하고, 결합
  res <- lapply(lcadata_list, function(lcadata) lcadata$gof %>% do.call(cbind, .))

  # 결과를 하나의 데이터프레임으로 결합
  res_df <- do.call(rbind, res)

  # 모델 이름을 "Model01", "Model02", "Model03" 등의 형태로 만듦
  model_names <- paste0("Model.", sprintf("%02d", seq_along(lcadata_list)))

  # rownames를 설정
  rownames(res_df) <- model_names

  colnames(res_df) = c("loglik","AIC","CAIC","BIC","entropy","df","Gsq")

  return(data.frame(res_df))
}
