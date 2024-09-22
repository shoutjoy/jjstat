
#' total anova report
#'
#' @param data aoa_df(grp_mean=F)
#' @param show show data
#'
#' @return report
#' @export
#'
#' @examples
#' \dontrun{
#' aov_table(data = mtcars, dv_var = "mpg",
#' iv_vars = c("cyl", "gear", "carb"),
#' grp_mean = FALSE)%>% apa_aov_df()
#'
#' apa_aov_df(data = mtcars, dv_var = "mpg",
#'  iv_vars = c("cyl", "gear", "carb"))
#' }
#'
#'
apa_aov_df <- function(data,   show = TRUE, dv_var = NULL, iv_vars = NULL
) {

  if(is.null(dv_var) | is.null(iv.vars)){
    data <- data
  }
  # else{
  #  data = aov_table(data = data, dv_var = dv_var, iv_vars = iv_vars, grp_mean = FALSE)
  #
  # }


  if(show){
    cat("\n ANOVA result\n")
    print(data)
  }
  cat("\n\n")

  for (i in 1:nrow(data)) {
    iv <- data$iv[i]
    dv <- data$dv[i]
    df1 <- data$df1[i]
    df2 <- data$df2[i]
    F_value <- data$F_value[i]
    p_value <- data$p_value[i]

    # F 값 형식 조정
    F_value <- sprintf("%.2f", F_value)

    # p 값이 0.001보다 작으면 "< .001"로 출력
    # p_value <-  sprintf("%.5f", p_value)
    # p_value_sig <- ifelse(p_value < 0.001, "< .001", sprintf("%.5f", p_value))
    p_value_sig <- ifelse(p_value < 0.001, "< .001", p_value)

    # F 값이 유의수준 0.05보다 작으면 '**'로 표기
    significance <- ifelse(p_value < 0.001, '***',
                           ifelse(p_value < 0.01, '**',
                                  ifelse(p_value < 0.05,  '*', ""
                                  )))


    cat(paste0(iv, "에 따른 ", dv, "의 일원배치 분산분석(oneway-ANOVA) 분석결과, "))
    cat(paste0(ifelse(p_value < 0.05, "통계적으로 유의하게 나타났다",
                      "통계적으로 유의하지 않았다"),
               "(F(", df1, ", ", df2, ") = ",
               F_value, significance,
               ", p = ", p_value_sig,")."),"\n")
  }
}
