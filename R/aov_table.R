
#' Generate an AOV results table for multiple independent variables
#'
#' @param data data.frame
#' @param dv_var dv
#' @param iv_vars c(iv1, iv2....)
#' @param unite pvalue unite
#'
#' @return table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"))
#'
#' }
#'
#'
aov_table <- function(data,
                      dv_var,
                      iv_vars,
                      unite=FALSE) {
  # data: 데이터 프레임
  # dv_var: 종속변수 컬럼명 (문자열)
  # iv_vars: 독립변수 컬럼명 리스트 (문자열 벡터)
  if(!is.data.frame(data)){
    stop("you need input data.frame")
  }
  # 결과를 저장할 데이터 프레임 초기화
  result_df <- data.frame(Indv_Variable = character(0),
                          F_Value = numeric(0),
                          P_Value = numeric(0))

  # 각 독립변수별로 ANOVA 분석 수행
  for (iv in iv_vars) {
    anova_result <- aov(formula(paste(dv_var, "~", iv)),
                        data = data)
    tidy_result <- broom::tidy(anova_result)
    levels <- paste0(unique(data[[iv]]), collapse =", " )

    # 결과를 데이터 프레임에 추가
    result_df <- rbind(result_df,
                       data.frame(
                         iv=iv,
                         dv=dv_var,
                         level = levels,
                         df1= tidy_result$df[1],
                         df2= tidy_result$df[2],
                         F_Value = tidy_result$statistic[1],
                         P_Value = tidy_result$p.value[1]))
  }

  result_df = result_df %>% p_mark_sig("P_Value", unite=unite)
  return(result_df)

}
