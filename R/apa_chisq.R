#' 카이제곱 apa보고
#'
#' @param data chisq.test result data
#' @param chi_type "적합성 검정", "독립성 검정", "동질성 검정"
#' @param show TRUE cat output
#'
#' @return data result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' dfa= matrix(c(22, 26, 55,13, 31, 33,13, 16, 38), ncol=3, byrow=TRUE)
#' colnames(dfa)=c("가","나","다")
#' rownames(dfa)=c("A","B","C")
#' dfa
#' dfat= t(dfa)
#' dfat%>%chisq.test()%>%chisq_apa()
#' }
chisq_apa <- function(data, chi_type = NULL, show= TRUE) {

  # "적합성 검정", "독립성 검정", "동질성 검정"
  # chisq.test()를 실행하고 tidy()를 이용하여 결과를 추출
  #   if(is.list(data)){
  # result <- data %>% tidy()
  #   }else if(is.matrix(data)|is.numeric(data)){}
  #   result <- chisq.test(data) %>% tidy()

  result <-data%>% tidy()
  # 통계적 유의성 여부 계산
  significance <- ifelse(result$p.value < 0.05, "통계적으로 유의하였다", "통계적으로 유의하지 않았다")

  p_vlaue_type = ifelse(result$p.value < 0.001, "p <.001", paste0("p = ",round(result$p.value, 3) ))

  if(is.null(chi_type)){
    # 결과에 따른 메시지 생성
    message <- paste("카이제곱 분석", " 결과(", result$method,"), ", significance,
                     paste0("(chisq(", result$parameter, ") = ", round(result$statistic, 3),
                            ", ", p_vlaue_type, ".", sep = " "), sep = "")
  }else{
    # 결과에 따른 메시지 생성
    message <- paste("chisq test:  ", chi_type, "으로 분석한 결과(", result$method,"), ", significance,
                     paste0("(chisq(", result$parameter, ") = ", round(result$statistic, 3),
                            ", ",  p_vlaue_type, ".", sep = " "), sep = "")

  }

  if(show){

    cat("\n")
    cat(message)
    cat("\n")

  }else{
    return(message)
  }

}
# chisq_apa <- function(data, chi_type = NULL, show= TRUE) {
#
#   # "적합성 검정", "독립성 검정", "동질성 검정"
#   # chisq.test()를 실행하고 tidy()를 이용하여 결과를 추출
#   #   if(is.list(data)){
#   # result <- data %>% tidy()
#   #   }else if(is.matrix(data)|is.numeric(data)){}
#   #   result <- chisq.test(data) %>% tidy()
#
#   result <-data%>% tidy()
#   # 통계적 유의성 여부 계산
#   significance <- ifelse(result$p.value < 0.05, "통계적으로 유의하였다", "통계적으로 유의하지 않았다")
#
#   if(is.null(chi_type)){
#     # 결과에 따른 메시지 생성
#     message <- paste("카이제곱 분석", " 결과(", result$method,"), ", significance,
#                      paste0("(chisq(", result$parameter, ") = ", round(result$statistic, 3),
#                             ", p = ", round(result$p.value, 3), ".", sep = " "), sep = "")
#   }else{
#     # 결과에 따른 메시지 생성
#     message <- paste("chisq test:  ", chi_type, "으로 분석한 결과(", result$method,"), ", significance,
#                      paste0("(chisq(", result$parameter, ") = ", round(result$statistic, 3),
#                             ", p = ", round(result$p.value, 3), ".", sep = " "), sep = "")
#
#   }
#
#   if(show){
#
#     cat("\n")
#     cat(message)
#     cat("\n")
#
#   }else{
#    return(message)
#   }
#
#
# }
