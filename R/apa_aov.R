#' Report aov data (format APA)
#'
#' @param aov_post_data aov_post data
#' @param md md = TRUE is markdown outpput
#' @param ns ncol sig column 7
#' @param ndf ncol df column 4
#' @param nt ncol t value column 5
#' @param np ncol p value column 6
#' @param digits digits 3
#'
#' @return report data
#' @export
#'
#' @examples
#' \dontrun{
#' aov(mpg ~ cyl , data=Mtcars)%>% aov_post() %>% anova_apa()
# aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="df")%>% anova_apa()
#' # This is error
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="contrast")%>% anova_apa()
#'
#' # aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="all")
#'
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="all")%>% anova_apa()
#' aov(mpg ~ cyl , data=Mtcars)%>%aov_post(type="res")%>% anova_apa()
#'
#' }
#'
anova_apa <- function(aov_post_data, ns=7, np=6, ndf=4, nt=5,
                      digits=3, md=FALSE,
                      posthoc_type="The Multi Least Significant Difference(MLSD)") {

  if(length(aov_post_data) == 8){
    stop("You shouldn't do it with 'type=contrast', do it with type='df'. do it! ")
  }
  data <- aov_post_data$contrast

  anova_dfs = aov_post_data$aov

  Fvalue= anova_dfs[1, 5]

  pvalue = ifelse(anova_dfs[1, 6] < 0.001 ,
                  "< .001).",
                  paste0("= ",round( anova_dfs[1, 6],4),")." ))

  df1= anova_dfs[1, 2]
  df2= anova_dfs[2, 2]

  form = aov_post_data$call
  iv  = form[2]
  dv  = form[3]

  inter <- c()
  for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
    inter[i] <- paste0("가설[",i,"] ",data[i, 1], "(diff = ",
                       round(data[i, 2], digits), ")",
                       "에 관한 차이", "분석 결과, 통계적으로",
                       ifelse(data[i, ns] == "ns", " 유의미한 효과는 없었다",
                              " 유의미한 효과가 나타났다"),
                       "(t(",data[i, ndf],") = ", round(data[i, nt], 3), ", p ",
                       ifelse(data[i, np]< 0.001,"< .001",
                              paste0("= ", round(data[i, np], 3))),
                       ")."
    )
  }

  print(data)
  if(md){  print(data%>% jjstat::md(digits=3))}

  cat("\n\n",paste0("'", iv,"'에 관한 '",dv,
                    "'의 ANOVA 분석 결과 통계적으로 유의하였다(F(",
                    df1,", ",df2,") = ",round(Fvalue, 2),", p ",
                    pvalue,
                    #  format( round(pvalue,3), digits=3, scienctific=TRUE),
                    " 이에 따라 ", posthoc_type ,"사후분석(POST HOC)을 실시하였다. 각 요인(factor)의 수준(level)에 따른 차이의 유의성 결과는 다음과 같다."),
      "\n\n", inter,"\n\n")
}






#' Report aov data (format APA)
#'
#' @param aov_post_data aov_post data
#' @param md md = TRUE is markdown outpput
#' @param ns ncol sig column 7
#' @param ndf ncol df column 4
#' @param nt ncol t value column 5
#' @param np ncol p value column 6
#' @param digits digits 3
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
aov_apa <- function(aov_post_data, ns=7, np=6, ndf=4, nt=5,
                    digits=3, md=FALSE,
                    posthoc_type="The Multi Least Significant Difference(MLSD)") {

  if(length(aov_post_data) == 8){
    stop("You shouldn't do it with 'type=contrast', do it with type='df'. do it! ")
  }
  data <- aov_post_data$contrast

  anova_dfs = aov_post_data$aov

  Fvalue= anova_dfs[1, 5]

  pvalue = ifelse(anova_dfs[1, 6] < 0.001 ,
                  "< .001).",
                  paste0("= ",round( anova_dfs[1, 6],4),")." ))

  df1= anova_dfs[1, 2]
  df2= anova_dfs[2, 2]

  form = aov_post_data$call
  iv  = form[2]
  dv  = form[3]

  inter <- c()
  for(i in 1:nrow(data)) { # 수정된 부분: 1부터 nrow(data)까지 반복
    inter[i] <- paste0("가설[",i,"] ",data[i, 1], "(diff = ",
                       round(data[i, 2], digits), ")",
                       "에 관한 차이", "분석 결과, 통계적으로",
                       ifelse(data[i, ns] == "ns", " 유의미한 효과는 없었다",
                              " 유의미한 효과가 나타났다"),
                       "(t(",data[i, ndf],") = ", round(data[i, nt], 3), ", p ",
                       ifelse(data[i, np]< 0.001,"< .001",
                              paste0("= ", round(data[i, np], 3))),
                       ")."
    )
  }

  print(data)
  if(md){  print(data%>% jjstat::md(digits=3))}

  cat("\n\n",paste0("'", iv,"'에 관한 '",dv,
                    "'의 ANOVA 분석 결과 통계적으로 유의하였다(F(",
                    df1,", ",df2,") = ",round(Fvalue, 2),", p ",
                    pvalue,
                    #  format( round(pvalue,3), digits=3, scienctific=TRUE),
                    " 이에 따라 ", posthoc_type ,"사후분석(POST HOC)을 실시하였다. 각 요인(factor)의 수준(level)에 따른 차이의 유의성 결과는 다음과 같다."),
      "\n\n", inter,"\n\n")
}

