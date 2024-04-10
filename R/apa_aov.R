#' Report aov data (format APA)
#'
#' @param aov_post_data aov_post data
#' @param md md = TRUE is markdown outpput
#'
#' @return report data
#' @export
#'
#' @examples
#' \dontrun{
#' Mtcars <- mtcars%>%as_trt("cyl")
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post() %>% anova_apa()
# aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="df") %>% anova_apa()
#' # This is error
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="contrast") %>% anova_apa()
#'
#' # aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="all")
#'
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="all") %>% anova_apa()
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="res") %>% anova_apa()
#'
#' }
#'
anova_apa <- function(aov_post_data, md=FALSE) {
  if(length(aov_post_data) == 8){
    stop("You shouldn't do it with 'type=contrast', do it with type='df'. do it! ")
  }
  data <- aov_post_data$contrast

  aov_df = aov_post_data$aov
  Fvalue= aov_df[1, 5]
  pvalue = ifelse(aov_df[1, 6] < 0.001 ,
                  "< .001).",
                  paste0("= ",round( aov_df[1, 6],4),")." ))

  df1= aov_df[1, 2]
  df2= aov_df[2, 2]

  form = aov_post_data$call
  iv  = form[2]
  dv  = form[3]



  inter <- c()
  for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
    inter[i] <- paste0("가설",data[i, 1], "(", data[i, 2], ")",
                       "에 관한 차이", "분석 결과, 통계적으로",
                       ifelse(data[i, 8] == "ns", " 유의미한 효과는 없었다",
                              " 유의미한 효과가 나타났다"),
                       "(est = ", round(data[i, 3], 3), ", p ",
                       ifelse(data[i, 7]< 0.001,"< .001", paste0("= ", round(data[i, 7], 3))),
                       "). \n"
    )
  }

  print(data)
  if(md){  print(data%>% jjstat::md(digits=3))}

  cat("\n\n",paste0("'", iv,"'에 관한 '",dv, "'의 ANOVA 분석 결과 통계적으로 유의하였다(F(",df1,", ",df2,") = ",round(Fvalue, 2),", p ",
                    pvalue,
                    #  format( round(pvalue,3), digits=3, scienctific=TRUE),
                    " 이에 따라 ",  "The Least Significant Difference(LSD) 사후분석(POST HOC)을 실시하였다. 각 요인(factor)의 수준(level)에 따른 차이의 유의성 결과는 다음과 같다."),"\n\n", inter,"\n\n")
}




#' Report aov data (format APA)
#'
#' @param aov_post_data aov_post data
#' @param md md = TRUE is markdown outpput
#'
#' @return report data
#' @export
#'
#' @examples
#' \dontrun{
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post()%>% aov_apa()
# aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="df")%>% aov_apa()
#' # This is error
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="contrast")%>% aov_apa()
#'
#' # aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="all")
#'
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="all")%>% aov_apa()
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="res")%>% aov_apa()
#'
#' }
#'
aov_apa <- function(aov_post_data, md=FALSE) {
  if(length(aov_post_data) == 8){
    stop("You shouldn't do it with 'type=contrast', do it with type='df'. do it! ")
  }
  data <- aov_post_data$contrast

  aov_df = aov_post_data$aov
  Fvalue= aov_df[1, 5]
  pvalue = ifelse(aov_df[1, 6] < 0.001 ,
                  "< .001).",
                  paste0("= ",round( aov_df[1, 6],4),")." ))

  df1= aov_df[1, 2]
  df2= aov_df[2, 2]

  form = aov_post_data$call
  iv  = form[2]
  dv  = form[3]



  inter <- c()
  for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
    inter[i] <- paste0("가설",data[i, 1], "(", data[i, 2], ")",
                       "에 관한 차이", "분석 결과, 통계적으로",
                       ifelse(data[i, 8] == "ns", " 유의미한 효과는 없었다",
                              " 유의미한 효과가 나타났다"),
                       "(est = ", round(data[i, 3], 3), ", p ",
                       ifelse(data[i, 7]< 0.001,"< .001", paste0("= ", round(data[i, 7], 3))),
                       ")."
    )
  }

  print(data)
  if(md){  print(data%>% jjstat::md(digits=3))}

  cat("\n\n",paste0("'", iv,"'에 관한 '",dv, "'의 ANOVA 분석 결과 통계적으로 유의하였다(F(",df1,", ",df2,") = ",round(Fvalue, 2),", p ",
                    pvalue,
                    #  format( round(pvalue,3), digits=3, scienctific=TRUE),
                    " 이에 따라 ",  "The Least Significant Difference(LSD) 사후분석(POST HOC)을 실시하였다. 각 요인(factor)의 수준(level)에 따른 차이의 유의성 결과는 다음과 같다."),"\n\n", inter,"\n\n")
}

