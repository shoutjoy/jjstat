#' Generate an AOV results table for multiple independent variables
#'
#' @param data data.frame
#' @param dv_var dv
#' @param iv_vars c(iv1, iv2....)
#' @param unite_F Fvalue unite
#' @param unite_p pvalue unite
#' @param sig 'sig = T' is add star
#' @param grp_mean 'grooup mean remove
#' @param posthoc 'tukey, scheffe, duncan, lsd
#' @param p.adj  p.adj=c("none","holm","hommel", "hochberg", "bonferroni", "BH", "BY", "fdr")
#'
#' @return table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mtcars <- mtcars%>%as_trt("cyl", "gear", "carb")
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"))
#'
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE)
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE) %>%
#' aov_table_apaOP
#'
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = TRUE)
#'
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = TRUE, unite_F = TRUE)
#'
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = TRUE, unite_F = TRUE) %>% markdown_table()
#' }
#'
#'
#'
aov_table = function(data,
                dv_var = NULL,
                iv_vars = NULL,
                grp_mean = TRUE,
                unite_p = FALSE,
                unite_F = FALSE,
                digits = 2,
                posthoc="lsd",
                p.adj= "none",
                sig = FALSE
) {
  # data: data.frame
  # dv_var: Dependent variable column name (string)
  # iv_vars: List of independent variable column names (string vector)
  if(!is.data.frame(data)){
    stop("you need input data.frame")
  }

  if(is.null(dv_var) | is.null(iv_vars)){
  }

  cat("\naov_df & aov_table Result
 grp_mean is TRUE -> add grp_mean / grp_mean is FLASE -> only levels \n\n")

  data = as_trt(data, iv_vars)  #factor treatment


  # Initialize a data frame to store the results
  result_df <- data.frame(Indv_Variable = character(0),
                          F_value = numeric(0),
                          p_value = numeric(0))

  # Perform an ANOVA analysis for each independent variable
  for (iv in iv_vars) {
    # ANOVA
    anova_result <- aov(formula(paste(dv_var, "~", iv)),data = data)
    #group_by mean
    meandata =  jjstat::mysummaryBy(formula(paste(dv_var, "~", iv)),
                                    data = data)[2]
    #
    #     lsd = multcompView::multcompLetters4(
    #                          anova_result, lsdHSD(anova_result ))
    #     lsdlist = lsd[[1]] %>% as.data.frame.list()
    #
    lsd = agricolae::LSD.test(anova_result, iv, console = FALSE, p.adj=p.adj)
    lsdlist = lsd$groups[[2]]

    duncan = agricolae::duncan.test(anova_result, iv, console = FALSE)
    duncanlist = duncan$groups[[2]]

    scheffe = agricolae::scheffe.test(anova_result, iv, console = FALSE)
    scheffelist = scheffe$groups[[2]]

    tukey = agricolae::HSD.test(anova_result, iv, console = FALSE)#
    tukeylist = lsd$groups[[2]]



    tidy_result <- broom::tidy(anova_result)
    levels_paste <- paste0(unique(data[[iv]]), collapse =", " )

    lsdlists <- paste0(unique(lsdlist), collapse =", " )
    duncanlists <- paste0(unique(duncanlist), collapse =", " )
    scheffelists <- paste0(unique(scheffelist), collapse =", " )
    tukeylists <- paste0(unique(tukeylist), collapse =", " ) #

    levels <- unique(data[[iv]])

    result_df <- rbind(result_df,
                       data.frame(
                         iv=iv,
                         dv=dv_var,
                         levels = levels_paste,   # unite
                         level = levels,
                         Mean = meandata,
                         posthoc_lsd = lsdlist, #posthoc
                         posthoc_scheffe = scheffelist,
                         posthoc_duncan = duncanlist,
                         posthoc_tukey = tukeylist,

                         lsdlists = lsdlists,
                         duncanlists = duncanlists,
                         scheffelists = scheffelists,
                         tukeylists = tukeylists,

                         df1= tidy_result$df[1],
                         df2= tidy_result$df[2],
                         F_value = tidy_result$statistic[1],
                         p_value = tidy_result$p.value[1]))
  } #for
  #
  if(posthoc == "lsd"){
    result_df = result_df %>%
      dplyr::select(-posthoc_scheffe, -posthoc_duncan, -posthoc_tukey,
                    -scheffelists,-duncanlists,-tukeylists) %>%
      dplyr::rename(POSTHOC = posthoc_lsd,
                    POSTHOCs = lsdlists )

    cat("   Using The Least Significant Difference (LSD) posthoc \n\n")

  }else if(posthoc == "scheffe"){
    result_df = result_df %>%
      dplyr::select(-posthoc_lsd, -posthoc_duncan, -posthoc_tukey,
                    -lsdlists,-duncanlists ,-tukeylists)%>%
      dplyr::rename(POSTHOC = posthoc_scheffe,
                    POSTHOCs = scheffelists )

    cat("   Using Scheffe's posthoc \n\n")

  }else if(posthoc == "duncan"){
    result_df = result_df %>%
      dplyr::select(-posthoc_scheffe, -posthoc_lsd, -posthoc_tukey,
                    -lsdlists,-scheffelists,-tukeylists)%>%
      dplyr::rename(POSTHOC = posthoc_duncan,
                    POSTHOCs = duncanlists )

    cat("   Using Duncan's posthoc \n\n")
  }else if(posthoc == "tukey"){
    result_df = result_df %>%
      dplyr::select(-posthoc_scheffe, -posthoc_duncan,-posthoc_lsd,
                    -scheffelists,-duncanlists, lsdlists) %>%
      dplyr::rename(POSTHOC = posthoc_tukey,
                    POSTHOCs = tukeylists )

    cat("   Using Tukey’s W Procedure (HSD)   posthoc \n\n")

  }




  # Adding results to a data frame
  if(grp_mean){
    result_df  = result_df %>% dplyr::select(-levels, -POSTHOCs)

    result_df2 = result_df %>%
      mutate(p_value = format_number(p_value, n3 = 3, n1=5)) %>%
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
    #


    result_df = dplyr::distinct(result_df,
                                iv, dv, levels, POSTHOCs,
                                df1, df2, F_value, p_value)




    result_df2 = result_df %>%
      mutate(p_value = format_number(as.vector(p_value), n3 = 3, n1=5)) %>%
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












#'
#'
#'#' Generate an AOV results table for multiple independent variables
#'
#' @param data data.frame
#' @param dv_var dv
#' @param iv_vars c(iv1, iv2....)
#' @param unite_F Fvalue unite
#' @param unite_p pvalue unite
#' @param sig 'sig = T' is add star
#' @param grp_mean 'grooup mean remove
#' @param posthoc 'tukey, scheffe, duncan, lsd
#' @param p.adj  p.adj=c("none","holm","hommel", "hochberg", "bonferroni", "BH", "BY", "fdr")
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
#' aov_table(data = mtcars, dv_var = "mpg",
#' iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE)
#' aov_table(data = mtcars, dv_var = "mpg",
#' iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE) %>%
#' aov_table_apa()
#'
#' aov_table(data = mtcars, dv_var = "mpg",
#' iv_vars = c("cyl", "gear", "carb"),
#'  grp_mean = TRUE)
#'
#' aov_table(data = mtcars, dv_var = "mpg",
#'  iv_vars = c("cyl", "gear", "carb"),
#'  grp_mean = TRUE, unite_F = TRUE)
#'
#' aov_table(data = mtcars, dv_var = "mpg",
#' iv_vars = c("cyl", "gear", "carb"),
#'  grp_mean = TRUE, unite_F = TRUE) %>%
#'   markdown_table()
#'
#' # park516d데이터 이용
#' park516d %>%
#'   select(1:6,teaching, academyType) %>%  #주요변수 선택
#'   bind_add_stat(c("RV_Teah",1:6)) %>% # 평균을 이용하여 항목묶음
#'   aov_df(dv_var="RV_Teah",
#'          iv_vars =c("teaching", "academyType"), sig = T)
#'
#'
#' }
#'
#'
#
aov_df <- function(data,
                   dv_var = NULL,
                   iv_vars = NULL,
                   grp_mean = TRUE,
                   unite_p = FALSE,
                   unite_F = FALSE,
                   digits = 2,
                   posthoc="lsd",
                   p.adj= "bonferroni",
                   sig = FALSE
) {
  # data: data.frame
  # dv_var: Dependent variable column name (string)
  # iv_vars: List of independent variable column names (string vector)
  if(!is.data.frame(data)){
    stop("you need input data.frame")
  }

  if(is.null(dv_var) | is.null(iv_vars)){
  }

  cat("\naov_df & aov_table Result
 grp_mean is TRUE -> add grp_mean / grp_mean is FLASE -> only levels \n\n")

  data = as_trt(data, iv_vars)  #factor treatment


  # Initialize a data frame to store the results
  result_df <- data.frame(Indv_Variable = character(0),
                          F_value = numeric(0),
                          p_value = numeric(0))

  # Perform an ANOVA analysis for each independent variable
  for (iv in iv_vars) {
    # ANOVA
    anova_result <- aov(formula(paste(dv_var, "~", iv)),data = data)
    #group_by mean
    meandata =  jjstat::mysummaryBy(formula(paste(dv_var, "~", iv)),
                                    data = data)[2]
    #
    #     lsd = multcompView::multcompLetters4(
    #                          anova_result, lsdHSD(anova_result ))
    #     lsdlist = lsd[[1]] %>% as.data.frame.list()
    #
    lsd = agricolae::LSD.test(anova_result, iv, console = FALSE, p.adj= p.adj)
    lsdlist = lsd$groups[[2]]

    duncan = agricolae::duncan.test(anova_result, iv, console = FALSE)
    duncanlist = duncan$groups[[2]]

    scheffe = agricolae::scheffe.test(anova_result, iv, console = FALSE)
    scheffelist = scheffe$groups[[2]]

    tukey = agricolae::HSD.test(anova_result, iv, console = FALSE)#
    tukeylist = lsd$groups[[2]]



    tidy_result <- broom::tidy(anova_result)
    levels_paste <- paste0(unique(data[[iv]]), collapse =", " )

    lsdlists <- paste0(unique(lsdlist), collapse =", " )
    duncanlists <- paste0(unique(duncanlist), collapse =", " )
    scheffelists <- paste0(unique(scheffelist), collapse =", " )
    tukeylists <- paste0(unique(tukeylist), collapse =", " ) #

    levels <- unique(data[[iv]])

    result_df <- rbind(result_df,
                       data.frame(
                         iv=iv,
                         dv=dv_var,
                         levels = levels_paste,   # unite
                         level = levels,
                         Mean = meandata,
                         posthoc_lsd = lsdlist, #posthoc
                         posthoc_scheffe = scheffelist,
                         posthoc_duncan = duncanlist,
                         posthoc_tukey = tukeylist,

                         lsdlists = lsdlists,
                         duncanlists = duncanlists,
                         scheffelists = scheffelists,
                         tukeylists = tukeylists,

                         df1= tidy_result$df[1],
                         df2= tidy_result$df[2],
                         F_value = tidy_result$statistic[1],
                         p_value = tidy_result$p.value[1]))
  } #for
  #
  if(posthoc == "lsd"){
    result_df = result_df %>%
      dplyr::select(-posthoc_scheffe, -posthoc_duncan, -posthoc_tukey,
                    -scheffelists,-duncanlists,-tukeylists) %>%
      dplyr::rename(POSTHOC = posthoc_lsd,
                    POSTHOCs = lsdlists )

    cat("   Using The Least Significant Difference (LSD) posthoc \n\n")

  }else if(posthoc == "scheffe"){
    result_df = result_df %>%
      dplyr::select(-posthoc_lsd, -posthoc_duncan, -posthoc_tukey,
                    -lsdlists,-duncanlists ,-tukeylists)%>%
      dplyr::rename(POSTHOC = posthoc_scheffe,
                    POSTHOCs = scheffelists )

    cat("   Using Scheffe's posthoc \n\n")

  }else if(posthoc == "duncan"){
    result_df = result_df %>%
      dplyr::select(-posthoc_scheffe, -posthoc_lsd, -posthoc_tukey,
                    -lsdlists,-scheffelists,-tukeylists)%>%
      dplyr::rename(POSTHOC = posthoc_duncan,
                    POSTHOCs = duncanlists )

    cat("   Using Duncan's posthoc \n\n")
  }else if(posthoc == "tukey"){
    result_df = result_df %>%
      dplyr::select(-posthoc_scheffe, -posthoc_duncan,-posthoc_lsd,
                    -scheffelists,-duncanlists, lsdlists) %>%
      dplyr::rename(POSTHOC = posthoc_tukey,
                    POSTHOCs = tukeylists )

    cat("   Using Tukey’s W Procedure (HSD)   posthoc \n\n")

  }




  # Adding results to a data frame
  if(grp_mean){
    result_df  = result_df %>% dplyr::select(-levels, -POSTHOCs)

    result_df2 = result_df %>%
      mutate(p_value = format_number(p_value, n3 = 3, n1=5)) %>%
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
    #


    result_df = dplyr::distinct(result_df,
                                iv, dv, levels, POSTHOCs,
                                df1, df2, F_value, p_value)
    result_df2 = result_df %>%
      mutate(p_value = format_number(as.vector(p_value), n3 = 3, n1=5)) %>%
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
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE)%>% aov_table_apa()
#'
#' aov_table_apa(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"))
#' }
#'
#'
aov_table_apa <- function(data,   show = TRUE, dv_var = NULL, iv_vars = NULL
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

