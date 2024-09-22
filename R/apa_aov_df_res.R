
#' apa_aov_df_res, aov_df의 분석결과
#'
#' @param data data
#' @param dv_col dv
#' @param iv_col iv
#' @param show TRUE, data result
#' @param digits 3
#'
#' @return 결과해석 text
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 테스트 데이터로 실행
#' data <- data.frame(
#'   독립변수 = c("LCA_class", "", "", "LCA_class",
#'   "", "", "LCA_class", "", "", "LCA_class", "", ""),
#'   종속변수 = c("진로동기", "진로동기", "진로동기",
#'   "진로태도성숙", "진로태도성숙", "진로태도성숙",
#'   "진로결정자기효능감", "진로결정자기효능감", "진로결정자기효능감",
#'   "진로준비행동", "진로준비행동", "진로준비행동"),
#'   df1 = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
#'   df2 = c(511, 511, 511, 511, 511, 511,
#'    511, 511, 511, 511, 511, 511),
#'   F = c(132.64, NA, NA, 107.45, NA, NA,
#'    138.02, NA, NA, 106.1, NA, NA),
#'   p = c("4.00665784818538e-47", NA, NA,
#'    "1.11197564656087e-39", NA, NA,
#'    "1.18550277068589e-48", NA, NA,
#'    "2.8829281865653e-39", NA, NA)
#' )
#'
#' # show=TRUE로 결과 출력
#' apa_results <- apa_aov_df_res(data, dv_col = 2, iv_col = 1, show = TRUE)

#'
#' }
apa_aov_df_res <- function(data, dv_col = 2, iv_col = 1, show = TRUE, digits = 3) {
  results <- c()

  if (show) {
    print(data)  # show 옵션이 TRUE일 때 데이터 출력
  }

  for (i in 1:nrow(data)) {
    # 종속변수가 비어있지 않을 때만 처리
    if (!is.na(data[i, dv_col]) && data[i, dv_col] != "") {

      # 독립변수가 비어있지 않은 경우만 추출
      if (!is.na(data[i, iv_col]) && data[i, iv_col] != "") {
        ind_var <- data[i, iv_col]
      } else {
        next  # 독립변수가 없는 경우 스킵
      }

      # 데이터에서 필요한 값을 추출
      factor_var <- data[i, dv_col]
      df1 <- data$df1[i]
      df2 <- data$df2[i]
      F_value <- data$F[i] # F값을그대로 처리

      # p_value가 문자형일 수 있으므로 숫자형으로 변환
      p_value <- as.numeric(data$p[i])

      # p-value 형식 처리: p < .001 여부 확인 및 형식 처리
      if (is.na(p_value)) {
        p_text <- ""
      } else if (p_value < 0.001) {
        p_text <- "p < .001"
      } else {
        p_text <- paste("p =", round(p_value, digits))
      }

      # 유의성 여부 확인
      if (!is.na(p_value)) {
        sig_text <- ifelse(p_value < 0.05, "통계적으로 유의하였다", "통계적으로 유의하지 않았다")
      } else {
        sig_text <- ""
      }

      # 결과 문장 작성
      result <- paste0(ind_var, "의 ", factor_var, "에 관한 ANOVA 분석 결과, ",
                       sig_text, "(F(", df1, ", ", df2, ") = ", F_value, ", ", p_text, ").")

      # 결과 리스트에 추가
      results <- c(results, result)
    }
  }

  cat("\n")
  cat("APA결과보고 \n")

  return(

    cat(results,"\n")
  )
}
