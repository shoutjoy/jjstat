#' Text to data.frame
#'
#' @param text text
#' @param header header = TRUE
#' @param type res, check
#' @param vars additinal factor
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
#'
#'
#' #' }
#'
text2df <- function(text, header=TRUE,
                    type="res",
                    vars=NULL) {
  text <- c(text)
  # Split text by line
  ROW <- strsplit(text, "\n")
  # Set the first row as the variable name
  colnames <- strsplit(ROW[[1]], " ")[[1]]

  # Convert the remaining rows to data
  ROW <- ROW[[1]]
  DATA <- lapply(ROW, function(x) {
    # Separate the alphanumeric part from the numeric part
    strsplit(x, " ")[[1]]
  })
  #data bind do.cal function
  DATA <- do.call( rbind.data.frame, DATA)
  #header check
  if(header){
    DATA<- DATA[-1,]
  }else{
    DATA<- DATA
  }
  colnames(DATA)= colnames

  check = list(ROW,colnames, DATA)

  if(is.null(vars)){
    res = DATA
  }else{
    res = bind_cols(Factor = vars, DATA)
  }


  switch(type, check = check, res = res )
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


