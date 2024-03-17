
#' Generate an AOV results table for multiple independent variables
#'
#' @param data data.frame
#' @param dv_var dv
#' @param iv_vars c(iv1, iv2....)
#' @param unite pvalue unite
#' @param mean Option to average by group and add
#'
#' @return table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' aov_table(data = mtcars, dv_var = "mpg",
#'           iv_vars = c("cyl", "gear", "carb"))
#'
#' aov_table(data = mtcars, dv_var = "mpg",
#'           iv_vars = c("cyl", "gear", "carb"),
#'            mean = TRUE)
#'
#' }
#'
#'

aov_table <- function(data,
                      dv_var,
                      iv_vars,
                      mean = FALSE,
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
    meandata =  jjstat::mysummaryBy(formula(paste(dv_var, "~", iv)),
                                    data = data)[2]

    tidy_result <- broom::tidy(anova_result)
    levels_paste <- paste0(unique(data[[iv]]), collapse =", " )
    levels <- unique(data[[iv]])

    result_df <- rbind(result_df,
                       data.frame(
                         iv=iv,
                         dv=dv_var,
                         levels = levels_paste,   # unite
                         level = levels,
                         Mean = meandata,
                         df1= tidy_result$df[1],
                         df2= tidy_result$df[2],
                         F_Value = tidy_result$statistic[1],
                         P_Value = tidy_result$p.value[1]))
  } #for

  # 결과를 데이터 프레임에 추가
  if(mean){
    result_df  = result_df %>%dplyr::select(-levels)

  }else{
    result_df  = result_df %>%dplyr::select(-Mean, -level)
    result_df = dplyr::distinct(result_df,
                                iv, dv, levels, df1, df2, F_Value, P_Value)
  }

  result_df = result_df %>% jjstat::p_mark_sig("P_Value", unite=unite)
  return(result_df)
}

