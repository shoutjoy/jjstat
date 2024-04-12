#' repot anova
#'
#' @param data aov data
#' @param digits 3
#' @param print table show
#'
#' @return table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' aov(mpg ~ cyl* gear +carb, data=mtcars) %>%report_aov_ko()
#'
#' aov(mpg ~ cyl* gear +carb, data=mtcars) %>%report_aov_ko(print=T)
#' aov(mpg ~ cyl* gear +carb, data=mtcars) %>%report_aov_ko(print=F)
#'
#' }
#'
report_aov_ko <- function(data, digits = 2, print = TRUE) {
  data0 <- data
  dv <- as.character(data$call[[2]][2])
  iv <- as.character(data$call[[2]][3])
  Data <- broom::tidy(data0)
  summary_data <- summary(data0)

  # 데이터로부터 관련 정보 추출
  term <- Data$term
  df <- Data$df
  statistic <- format(Data$statistic, 3, trim = TRUE)
  p_value <- Data$p.value

  # 결과 문장 생성
  result_sentence <- vector("list", length = nrow(Data) - 1)
  for (i in 1:(nrow(Data) - 1)) {
    # p 값에 기반하여 유의성 결정
    significance <- ifelse(p_value[i] < 0.05,
                           " 통계적으로 유의한 차이를 나타내었다",
                           " 통계적으로 유의한 차이는 나타나지 않았다")

    # 상호 작용 효과 또는 주 효과 여부 결정
    effect <- ifelse(grepl(":", term[i]),
                     paste0("가설[",i,"]: 상호 작용 효과"),
                     paste0("가설[",i,"]: 주 효과"))

    result_sentence[i] <- paste0(effect, "에 대한 일원 분산분석(One way-ANOVA) 결과, 독립변수[",
                                 iv, "]와 종속변수[", dv, "] 간의 관계는 독립변수 간",
                                 significance, "(F(",
                                 df[i], ", ", df[nrow(Data)], ") = ",
                                 statistic[i],
                                 ", p = ",
                                 format(p_value[i], 3, trim = TRUE),
                                 "). \n")
  }
  # 결과 출력
  res <- do.call(rbind, result_sentence)

  if(print){
    print(data%>% broom::tidy()%>%p_mark_sig())
  }
  cat("\n\n",res,"\n")
}
