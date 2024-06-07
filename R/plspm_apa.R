#' plspm_apa_paths
#'
#' @param data plspm_paths_sig
#'
#' @return interpretation
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 데이터 프레임
#' jutpls_boot1 <- data.frame(
#'   endogenous = c("진로동기", "진로태도", "진로준비", "진로준비", "진로준비"),
#'   exogenous = c("자기효능감", "자기효능감", "자기효능감", "진로동기", "진로태도"),
#'   Est = c(0.880, -0.674, 0.429, 0.446, 0.007),
#'   SE = c(0.050, 0.078, 0.129, 0.121, 0.078),
#'   t = c(17.597, -8.656, 3.330, 3.694, 0.087),
#'   p.value = c("<.001", "<.001", 0.001, "<.001", 0.931),
#'   sig = c("***", "***", "**", "***", "ns")
#' )
#'
#' # 함수 실행
#' plspm_apa_paths(jutpls_boot)
#'
#' jutpls_boot %>% plspm_paths_sig()%>% plspm_apa_paths()
#' #'
#' jutpls_boot %>% plspm_apa_paths()
#' jutpls_boot %>% plspm_paths_sig()%>% plspm_apa_paths()
#' jutpls_boot %>% plspm_paths_sig(unite=T)%>% plspm_apa_paths()
#'
#'
#' }
#'
plspm_apa_paths <- function(data) {

  if(length(data) ==13){
    df = data %>% plspm_paths_sig()

  }else if(length(data)==7){
    df = data
  }else if(length(data)==5){
    df <- data %>%
      mutate(exogenous = str_extract(Path, "^[^ ]+"),
             endogenous = str_extract(Path, "(?<=-> )[가-힣]+"))%>%
      p_mark_sig()
  }


  results <- df %>%
    mutate(hypothesis = paste0("가설[", row_number(), "]: ",
                               exogenous, "에서 ", endogenous, "에 미치는 효과는 ",
                               ifelse(sig == "ns", "통계적으로 유의하지 않았다", "통계적으로 유의하였다"),
                               "(Est = ", Est, ", t = ", t, ", p = ", p.value, ").")) %>%
    pull(hypothesis)

  cat("PLS구조방정식의 경로분석 결과, 다음과 같다.\n")
  for (result in results) {
    cat(result, "\n")
  }
}
