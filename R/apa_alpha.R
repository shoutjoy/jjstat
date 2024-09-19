#' alpha_apa report
#'
#' @param data 분석결과
#'
#' @return apa보고서
#' @export
#'
#' @examples
#' \dontrun{
#'
#' #'
#' # 진로동기 신뢰도 분석
#' jjstat::bind_alpha_table(jutr2%>% select(A01:A15),
#'                          c(1:5), c(6:10), c(11:15)) %>%nice_Var()%>% nice_table()%>%
#'   replace_df("a","진로정체감") %>%
#'   replace_df("b","진로통찰력") %>%
#'   replace_df("c","진로탄력성") %>%
#'   alpha_apa()
#'
#' }
alpha_apa <- function(data) {
  # 필요한 패키지 로드 (없으면 설치)
  if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
  library(dplyr)

  # 'Var' 컬럼이 공백이 아닌 경우에만 선택하여 신뢰도 분석 결과를 APA 형식으로 출력
  results <- data %>%
    dplyr::filter(Var != "") %>%
    dplyr::distinct(Var, .keep_all = TRUE) %>%
    rowwise() %>%
    mutate(
      # Var 컬럼에서 alpha 값과 95%CI를 추출
      alpha_value = sub(".*alpha = ([0-9.]+),.*", "\\1", `alpha_95%CI`),
      ci_lower = sub(".*\\[([0-9.]+),.*", "\\1", `alpha_95%CI`),
      ci_upper = sub(".*, ([0-9.]+)\\].*", "\\1", `alpha_95%CI`),
      report = paste0(Var, " 신뢰도는 ", alpha_value, "이었다, ",
                      "alpha = ", alpha_value, ", ",
                      "95%CI[", ci_lower, ", ", ci_upper, "].")
    ) %>%
    pull(report)

  # 결과 문장들을 하나의 텍스트로 합침
  final_report <- paste(results, collapse = " ")


  print(data)
  # 최종 보고서 반환
  cat("\n")
  return(cat(final_report, "\n\n"))

}
