#' mcnemar_test
#'
#' @param mat matrix
#' @param correct McNemar's Chi-squared test with continuity correction
#' @param exact CI check binom.test
#'
#' @return reusult
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' Buying_car  = matrix(c(23,28,7,12), nrow=2,
#'                      dimnames= list(c("시승전_있음","시승전_없음"),
#'                                     c("시승후_있음","시승후_없음")))
#' Buying_car
#' mcnemar_test(Buying_car)
#' mcnemar_test(Buying_car, correct=FALSE)
#'
#' gen_table = matrix(c(c(9, 3, 13, 10)),ncol=2,
#'                    dimnames=list("case"=c("yes", "no"),
#'                                  "control"=c("yes", "no")   ))
#' gen_table
#' #Rbase
#' mcnemar.test(gen_table, correct = FALSE)
#' mcnemar_test(gen_table, correct = FALSE)
#'
#'
#'
#' #모든 종류의 분석을 시행한 경우
#' bind_cols(
#'   "TEST"= c("correct and exact", "not Correct"," Not exact", "Not correct, exact"),
#'   bind_rows(
#'     mcnemar_test(Buying_car),  # 디폴트ㅡ
#'     mcnemar_test(Buying_car, correct=FALSE), # 연속보정 중지
#'     mcnemar_test(Buying_car,exact=FALSE), # 정확도 중지
#'     mcnemar_test(Buying_car,exact=FALSE, correct=FALSE)# 연속성 보정, 정확도 중지,SAS
#'   )
#' )%>%select(-b,-c,-odds_ratio)
#' }
mcnemar_test = function(mat, correct = TRUE, exact=TRUE){
  mat= as.matrix(mat)
  if(exact){
    exacttest= "(exact)"
  }else{
    exacttest= NULL
  }

  matmargins =mat%>%addmargins()
  cat("\n Table with margins \n")
  print(matmargins)
  if(correct){
    cat("\n"," McNemar's Chi-squared test with continuity correction",exacttest,"\n\n")
    chisq = ( abs(mat[1,2]- mat[2,1])-1)^2 /(mat[1,2]+ mat[2,1])

  }else{
    cat("\n","McNemar's Chi-squared test",exacttest,"\n\n")
    chisq = (mat[1,2]- mat[2,1])^2 /(mat[1,2]+ mat[2,1])
  }
  #sixe
  N= matmargins[3,3]

  #odds
  odds_ratio = (mat[1,2]/mat[2,1])
  b = mat[1,2]
  c = mat[2,1]
  df = 1
  #exact data
  exact_data = binom.test(x=b, n=b+c)%>%broom::tidy()%>%
    rename(probability=estimate,b =statistic,c=parameter,
    ) %>%dplyr::select(probability, b,c,p.value,
                       conf.low, conf.high, method)


  #CI

  if(exact){
    p = exact_data[["p.value"]]
    CI95 = paste0( "[",round(exact_data[["conf.low"]],5),", " ,
                   round(exact_data[["conf.high"]],5),"]")

    res = cbind.data.frame(b,c,odds_ratio, N,chisq, df, p, CI95)%>%
      p_mark_sig("p")%>%dplyr::select(-1)

  }else{
    SE = sqrt(1/mat[1,2] +1/ mat[2,1])
    Lower = odds_ratio*exp(qnorm(0.025)*SE)
    Upper = odds_ratio*exp(qnorm(0.025, lower.tail=FALSE)*SE)
    CI95 = paste0("[", round(Lower,3) ,", ", round(Upper,3),"]")
    p = pchisq(chisq, df, lower.tail=FALSE)

    res = cbind.data.frame(b,c,odds_ratio, N,chisq, df, p, CI95)%>%
      p_mark_sig("p")%>%dplyr::select(-1)
  }

  #analysis result
  cat(paste0("     McNemar's χ2 = ", round(chisq,2),
             ", df = 1", ", p = ", round(p, 6) ),", 95%CI",CI95,"\n\n")

  res
}
