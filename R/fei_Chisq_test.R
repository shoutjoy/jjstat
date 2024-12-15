
#' fei분석을위한 카이제곱함수
#'
#' @param tabledf 빈도분석 Freq_table 데이터
#' @param col "빈도"
#' @param name 구분
#' @param ns FALSE 생성한 것사용 ,TRUE RVAideMemoire로 행렬로 나타남
#' @param sep sep="/"
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' eduteck_s$Q31%>% na_remover() %>% Freq_table(prop=TRUE) %>%
#'   fei_Chisq_test()
#' }
fei_Chisq_test <- function(tabledf, col = "빈도", name = "구분", ns = FALSE,sep=" / ") {
  # RVAideMemoire 패키지 로드
  if (!requireNamespace("RVAideMemoire", quietly = TRUE)) {
    stop("RVAideMemoire 패키지를 설치해야 합니다. install.packages('RVAideMemoire')")
  }

  if (ns) {
    # ns = TRUE: RVAideMemoire의 chisq.multcomp() 실행
    chisq_posthoc <- tabledf %>%
      arrange(Freq %>% desc()) %>%
      Freq_table_colnames() %>%
      Pull(col = col, name = name) %>%
      RVAideMemoire::chisq.multcomp()
    # chisq_md <- chisq_posthoc %>%as.data.frame()%>%  md() # chisq.multcomp() 결과 요약
    # ns = TRUE: RVAideMemoire의 chisq.multcomp() 실행
    chisq_md0 <- data.frame(
      Comparison = rownames(chisq_posthoc$p.value),
      P.Value = chisq_posthoc$p.value
    ) %>% tibble()
    chisq_md =  chisq_md0%>% md()

    print(chisq_md0)
  } else {
    # ns = FALSE: 기존 chisq_multcomp2() 실행
    chisq_posthoc <- tabledf %>%
      arrange(Freq %>% desc()) %>%
      Freq_table_colnames() %>%
      Pull(col = col, name = name) %>%
      chisq_multcomp2(sep=sep)

    chisq_md <- chisq_posthoc %>%
      md()  # chisq_multcomp2() 결과 요약
  }

  print(chisq_posthoc %>% dall())
  return(chisq_md)
}
