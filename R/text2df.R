#' Text to data.frame /It is characterized by the use of blank spaces to separate content.
#'
#' @param text text(Row variable names must not contain spaces. )
#' @param header header = TRUE
#' @param type res, check
#' @param add_vars additinal factor
#'
#' @return table data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' text <- "변인 평균 표준편차 왜도 첨도
#' 성취감 4.24 0.73 -0.82 0.39
#' 인정감 4.19 0.60 -0.36 -0.42
#' 책임감 4.36 0.56 -0.61 -0.29
#' 발전성 3.65 0.89 -0.50 -0.04
#' 감독 3.59 0.88 -0.46 0.13
#' 보수 3.56 0.90 -0.41 -0.18
#' 대인관계 3.75 0.81 -0.14 -0.54
#' 작업환경 3.83 0.75 -0.21 -0.44
#' 안정성 3.28 1.08 -0.28 -0.68
#' 감정이입 3.96 0.72 -0.40 -0.33
#' 직무적합 4.00 0.75 -0.35 -0.78
#' 일체감 4.02 0.75 -0.59 -0.13
#' 현학원재직 3.97 0.79 -0.77 0.37
#' 타학원이직 2.99 1.06 -0.41 -0.77
#' 타직종이직 2.76 1.07 -0.13 -0.70"
#'
#' text2df(text)
#'
#' text2df(text, vars=c(rep("동기요인", 4), rep("위생요인",5), rep("개인요인",6)  ))
#'
#' text2df(text, T)%>% make_df_text()
#'
#' #direct output
#' "var B se b t
#' 성취감 -.03 .09 -.03 -.33
#' 인정감 .24 .09 .20 2.50
#' 책임감 .30 .08 .23 3.56***
#' 발전성 -.08 .06 -.10 -1.29
#' 감독 .06 .05 .08 1.35
#' 보수 -.14 .07 -.18 -2.01*
#' 대인관계 .06 .06 .06 .89
#' 작업환경 .33 .09 .34 3.82***
#' 안정성 .17 .05 .26 3.42**
#' 성별 -.05 .09 -.03 -.52
#' 연령 -.20 .07 -.22 -3.01**
#' 학력 .05 .07 .04 .70
#' 소득 .00 .04 .01 .12
#' 종사기간 .17 .06 .23 2.71**
#' 기관규모 -.10 .03 -.20 -3.36**" %>%text2df()
#'
#' reg_text = "var B se b t
#' 성취감 -.03 .09 -.03 -.33
#' 인정감 .24 .09 .20 2.50
#' 책임감 .30 .08 .23 3.56***
#' 발전성 -.08 .06 -.10 -1.29
#' 감독 .06 .05 .08 1.35
#' 보수 -.14 .07 -.18 -2.01*
#' 대인관계 .06 .06 .06 .89
#' 작업환경 .33 .09 .34 3.82***
#' 안정성 .17 .05 .26 3.42**
#' 성별 -.05 .09 -.03 -.52
#' 연령 -.20 .07 -.22 -3.01**
#' 학력 .05 .07 .04 .70
#' 소득 .00 .04 .01 .12
#' 종사기간 .17 .06 .23 2.71**
#' 기관규모 -.10 .03 -.20 -3.36**"
#'
#' text2df(reg_text)
#' #'
#' text2df(reg_text) %>% tibble()
#'
#' #text to numeric
#' text2df(reg_text)%>% Char2num() %>% tibble()
#'
#' text2df(reg_text)%>% Char2num(iter=2:4) %>% tibble()
#' #'
#'
#' text ="d           Estimate  Std.Err  z-value  P(>|z|)
#'   read~ppsych           -0.275    0.037   -7.385    0.000
#'     read~motiv             0.461    0.037   12.404    0.000
#' arith~motiv             0.600    0.036   16.771    0.000
#' "
#' text2df(text)
#' text2df(text) %>% tibble::tibble()
#'
#' text ="             Estimate Std.Error tvalue Pr(>|t|)
#' (Intercept) -1.336e-07  3.608e-01   0.000        1
#' ppsych      -2.747e-01  3.730e-02  -7.363  7.51e-13***
#' motiv        4.613e-01  3.730e-02  12.367  <2e-16***
#' "
#' text2df(text) %>%tibble()
#' text2df(text) %>%make_df_text()
#'
#'
#' text ="                   Estimate  Std.Err  z-value  P(>|z|)
#'
#'     read~ppsych           -0.275    0.037   -7.385    0.000
#'     read~motiv             0.461    0.037   12.404    0.000
#'
#'     arith~ppsych           -0.096    0.037   -2.616    0.009
#'      arith~motiv             0.576    0.037   15.695    0.000
#' "
#' text2df(text)
#'
#'
#'
#' text =" 과장 대리 부장 사원 주임 차장 SUM
#' 남자 30(39.5%) 21(38.9%) 34(79.1%) 30(49.2%)  10(37%)     28(71.8%) 153
#' 여자 46(60.5%) 33(61.1%) 9(20.9%) 31(50.8%) 17(63%)    11(28.2%) 147
#' SUM 76 54 43 61 27 39 300
#' "
#' text2df(text)
#' #' }
#'
text2df <- function (text,
                     header = TRUE,
                     type = "res",
                     add_vars = NULL){

  text = text
  ROW <- strsplit(text, "\n")[[1]]
  colnames <- strsplit(ROW[[1]], "\\s+")[[1]]


  ROW <- gsub("^\\s+", "", ROW)
  ROW <- ROW[nchar(trimws(ROW)) > 0]
  ROW <- gsub("\\s*~\\s*", "~", ROW)
  DATA <- lapply(ROW, function(x) {
    strsplit(x, "\\s+")[[1]]
  })


  max_length <- max(sapply(DATA, length))
  DATA <- lapply(DATA, function(row) {
    length_diff <- max_length - length(row)
    c(row, rep(NA, length_diff))
  })


  #combine
  DATA <- do.call(rbind.data.frame, DATA)

  DATA[DATA == ""] <- NA
  if (header) {
    DATA <- DATA[-1, , drop = FALSE]
  }
  if (header) {
    colnames(DATA) <- colnames
  }
  else {
    first_col_name <- gsub("~.*", "", colnames[1])
    colnames(DATA) <- c(first_col_name, colnames[-1])
  }
  if ("" %in% colnames(DATA)) {
    res <- DATA
    colNames = colnames(res)
    colNames = colNames[-1]
    colnames(res) = c("variable", colNames)
  }
  else {
    res <- DATA
  }



  if (is.null(add_vars)) {
    res = res
  }
  else {
    res = bind_cols(Factor = add_vars, res)
  }
  # NAME = colnames(res[,1])
  # res= res%>%tibble::column_to_rownames(NAME)

  switch(type, check = list(ROW, colnames, DATA), res = res)
}






#' t_sep separate t value and star
#'
#' @param data  data
#'
#' @return add column
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' reg_text = "var B se b t
#' 성취감 -.03 .09 -.03 -.33
#' 인정감 .24 .09 .20 2.50
#' 책임감 .30 .08 .23 3.56***
#' 발전성 -.08 .06 -.10 -1.29
#' 감독 .06 .05 .08 1.35
#' 보수 -.14 .07 -.18 -2.01*
#' 대인관계 .06 .06 .06 .89
#' 작업환경 .33 .09 .34 3.82***
#' 안정성 .17 .05 .26 3.42**
#' 성별 -.05 .09 -.03 -.52
#' 연령 -.20 .07 -.22 -3.01**
#' 학력 .05 .07 .04 .70
#' 소득 .00 .04 .01 .12
#' 종사기간 .17 .06 .23 2.71**
#' 기관규모 -.10 .03 -.20 -3.36**"
#'
#'
#' text2df(reg_text)
#'
#' text2df(reg_text) %>%t_sep()
#'

#' }
#'
t_sep = function(data){
  data%>%mutate(t.value= gsub("\\*","", t),
                sig = gsub("[^*]", "", t) ) %>%
    select(-t)
}


