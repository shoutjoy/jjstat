
#' Generate an AOV results table for multiple independent variables
#'
#' @param data data.frame
#' @param dv_var dv
#' @param iv_vars c(iv1, iv2....)
#' @param unite_F Fvalue unite
#' @param unite_p pvalue unite
#' @param mean Option to average by group and add
#' @param sig 'sig = T' is add star
#'
#' @return table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"))
#'
#' aov_table(data = mtcars, dv_var = "mpg",iv_vars = c("cyl", "gear", "carb"),
#'            mean = TRUE)
#'
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE)
#'
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = TRUE)
#'
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = TRUE, unite_F = TRUE)
#'
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = TRUE, unite_F = TRUE) %>% markdown_table()
#' }
#'
#'

aov_table <- function(data,
                      dv_var = NULL,
                      iv_vars = NULL,
                      grp_mean = FALSE,
                      unite_p = FALSE,
                      unite_F = FALSE,
                      digits = 2,
                      sig = FALSE) {
  # data: 데이터 프레임
  # dv_var: 종속변수 컬럼명 (문자열)
  # iv_vars: 독립변수 컬럼명 리스트 (문자열 벡터)
  if(!is.data.frame(data)){
    stop("you need input data.frame")
  }

  if(is.null(dv_var) | is.null(iv_vars)){
  }

  cat("\naov_df & aov_table Result
 grp_mean is TRUE -> add grp_mean / grp_mean is FLASE -> only levels \n\n")

  # 결과를 저장할 데이터 프레임 초기화
  result_df <- data.frame(Indv_Variable = character(0),
                          F_value = numeric(0),
                          p_value = numeric(0))

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
                         F_value = tidy_result$statistic[1],
                         p_value = tidy_result$p.value[1]))
  } #for

  # 결과를 데이터 프레임에 추가
  if(grp_mean){
    result_df  = result_df %>% dplyr::select(-levels)

    result_df2 = result_df %>%
                     mutate(p_value = format_number(p_value, n3 = 3)) %>%
                  Round(digits, exclude = "p_value")%>%
                    tibble::tibble()
    result_df2$p_value = as.numeric(result_df2$p_value)

     if(sig){
       result_df2 = result_df2 %>% mutate(sig = add_sig(p_value))
     }else{
       result_df2 = result_df2
     }


  }else{
    result_df  = result_df %>% dplyr::select(-Mean, -level)
    result_df = dplyr::distinct(result_df,
                                iv, dv, levels, df1, df2, F_value, p_value)
    result_df2 = result_df %>%
                mutate(p_value = format_number(as.vector(p_value), n3 = 3)) %>%
                Round(digits, exclude = "p_value")%>%
                  tibble::tibble()
    result_df2$p_value = as.numeric(result_df2$p_value)

    if(sig){
      result_df2 = result_df2 %>% mutate(sig = add_sig(p_value))
    }else{
      result_df2 = result_df2
    }

  }



 # UNite
  if(unite_F){
    result_df1 = result_df  %>%
                 mutate(sig = ifelse(p_value < 0.001, "***",
                                 ifelse(p_value < 0.01, "**",
                                    ifelse(p_value < 0.05, "*", "")))

                        )

    result_df1$F_value = round(result_df1$F_value , digits)

    result_df2 = result_df1 %>%
                tidyr::unite(F_value, F_value, sig, sep="") %>%
      Round(digits, exclude = "p_value") %>% tibble::tibble()


  }else if(unite_p){

     result_df1 = result_df %>%
                        mutate(sig = ifelse(p_value < 0.001, "***",
                                         ifelse(p_value < 0.01, "**",
                                             ifelse(p_value < 0.05, "*", ""))))


     # result_df1$F_value = round(result_df1$F_value , digits)

     result_df2 = result_df1 %>%
       Round(digits, exclude = "p_value") %>%
       mutate(p_value = format_number(p_value, n3=2)) %>%
       tidyr::unite(p_value, p_value, sig, sep="") %>%
       tibble::tibble()

  }  # result_df2
  result_df2
  }




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
#' aov_df(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"))
#'
#' aov_df(data = mtcars, dv_var = "mpg",iv_vars = c("cyl", "gear", "carb"),
#'            mean = TRUE)
#'
#' aov_df(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE)
#'
#' aov_df(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = TRUE)
#'
#' }
#'
#'

aov_df <- function(data,
                      dv_var,
                      iv_vars,
                      grp_mean = FALSE,
                      unite_p = FALSE,
                      unite_F = FALSE) {
  # data: 데이터 프레임
  # dv_var: 종속변수 컬럼명 (문자열)
  # iv_vars: 독립변수 컬럼명 리스트 (문자열 벡터)
  if(!is.data.frame(data)){
    stop("you need input data.frame")
  }

  cat("\naov_df & aov_table Result
 grp_mean is TRUE -> add grp_mean / grp_mean is FLASE -> only levels \n\n")

  # 결과를 저장할 데이터 프레임 초기화
  result_df <- data.frame(Indv_Variable = character(0),
                          F_value = numeric(0),
                          p_value = numeric(0))

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
                         F_value = tidy_result$statistic[1],
                         p_value = tidy_result$p.value[1]))
  } #for

  # 결과를 데이터 프레임에 추가
  if(grp_mean){
    result_df  = result_df %>% dplyr::select(-levels)

  }else{
    # result_df  = result_df %>% dplyr::select(-Mean, -level)
    result_df = dplyr::distinct(result_df,
                                iv, dv, levels, df1, df2, F_value, p_value)
  }

  # UNite
  if(unite_F){
    result_df = result_df %>% p_mark_sig("p_value") %>%
      unite(F_value, Fvalue:sig)

  }else{
    result_df = result_df %>% p_mark_sig("p_value", unite = unite_p)
  }


  # result_df

}






