
#' Functions to check the chi-square test of a paper
#'
#' @param input vectro
#' @param ncol matrix col
#' @param colname  input
#' @param rowname  input
#' @param simulate.p.value  default FaLSE fisher exact test
#' @param digits  round 3
#' @param type  all, chisq
#' @param correct  default is Pearson’s Chi-squared test with Yates’ continuity correction (If you have something with a small sample count, you should use it. ), FALSE Pearson's Chi-squared test
#' @param B B is bootstrap default 2000
#' @return chisquare data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## Mina Choi. (2007). Analysis of learning effects
#' # according to online and offline learning environments
#' #  in the same class by the same instructor.
#' #  Educational Information Media Research, 13(4>  5-23.
#' ## jjpaper
#' ## https://blog.naver.com/shoutjoy/223005233887
#'
#' chisq_retest(c(111, 33, 64, 58), ncol=2 )
#' ## 논문에 나온 값이 잘못됨 :chisq = 17.793
#' ## this chisq = 16.7
#'
#' chisq_retest(c(111, 33, 64, 58), ncol=2, type="chisq" )
#'
#' chisq_retest(c(111, 33, 64, 58), ncol=2,
#'   colname = c("experience", "Non-experience"),
#'   rowname =c("E-Leanring","FaceToFace"))
#'
#' chisq_retest(c(4,10, 78, 46, 6, 0,5,79,36,2),
#'   ncol=2, simulate.p.value=T )
#'
#' chisq_retest(c(4,10, 78, 46, 6, 0,5,90,36,2),
#'   ncol=2 ,simulate.p.value=T)
#'
#' chisq_retest(c(4,10, 78, 46, 6, 0,5,79,36,2), ncol=2,
#'   colname = c("experience", "Non-experience"),
#'   rowname = c("maxlow","low","mid","high","maxhigh") )
#'
#'
#'##  Comparison of gender learning methods
#' # --> An invalid value was calculated.
#'## chisq = 5.238, p =0.015
#'# But recalculate chisq = 4.69, p = 0.0303
#' chisq_retest(c(84, 60, 54, 68), ncol=2 ,
#'              colname = c("experience", "Non-experience"),
#'                           rowname = c("male", "female"))
#'
#' ## Comparing Learner Characteristics by Grade Level, Mina Choi (2007)
#'
#' chisq_retest(c(18,38, 40, 48, 52,35,19,16), ncol=2 ,
#' colname = c("experience", "Non-experience"),
#' rowname = c("1학년","2학년","3학년","4학년")
#'
#'
#' ##If the value entered is not a vector value, but a value calculated with table(),
#' # it is automatically calculated with the ....
#' table(mtcars$am, mtcars$vs) %>% Chisq_retest()
#'
#' }
#'
#'
chisq_retest <- function(input, ncol = 2,
                         colname = NULL,
                         rowname = NULL,
                         type= "all",
                         simulate.p.value = FALSE,
                         digits=3,
                         correct=TRUE,
                         B=2000){
#first step
  if(is.matrix(input)){
    data <- input

    res <- chisq.test(data, simulate.p.value = simulate.p.value,
                      correct = correct, rescale.p =rescale.p )


  }else if(is.table(input)){
    data <- input
    res <- chisq.test(data, simulate.p.value = simulate.p.value,
                      correct = correct, rescale.p =rescale.p )

  }else{
    data <- matrix( input, ncol = ncol)
    if(!is.null(colname)){
      colnames(data) <- colname
      rownames(data) <- rowname
    }else{
      data <- data
    }

    res <- chisq.test(data, simulate.p.value = simulate.p.value,
                      correct = correct, rescale.p = rescale.p )
  }
  #정확도 문제를 해결하기 위한 replicates 적용
  # res
  Chisqure_Result = cbind(Chisq = res$statistic,
                          df = res$parameter,
                          p.value = res$p.value
  )%>% jjstat::p_mark_sig("p.value")

  # Add sig table
  ratio_cell = calculate_chi_sig(data,"ratio_cell")%>%format(justify="left")
 chisq_cell = calculate_chi_sig(data,"chisq_cell")%>%format(justify="left")


  reslist <- list(
    Chisqure_Result = Chisqure_Result,
    Cramers_V = cramers_v(data),
    # res$data.name,
    # Observed = res$observed,
    Observed_addmargins = res$observed %>% addmargins()%>% round(digits),
    Obs_porp = res$observed %>% prop.table()%>% round(digits),
    Expected = res$expected %>% addmargins()%>% round(digits),
    Residual = res$residuals%>% round(digits),
    Stdres = res$stdres%>% round(digits),
    Analysis_Method = res$method,
    Chisq_cell = res$residuals^2%>% round(digits)%>%addmargins(),
    # Ratio_obs_exp = (res$observed/res$expected)%>% round(digits),
    Ratio_obs_exp_sig= ratio_cell ,
    chisq_cell=chisq_cell

  )

  res1 = list(chi= Chisqure_Result,
              Observed = res$observed %>% addmargins()%>% round(digits),
              Obs_porp = res$observed %>% prop.table()%>% round(digits),
              Expected = res$expected %>% addmargins()%>% round(digits),
              Analysis_Method = res$method
  )

  summary =list(
    Chisqure_Result = Chisqure_Result,
    Cramers_V = cramers_v(data),
    chisq_sig= ratio_cell,
    chisq_cell= chisq_cell
  )

  switch(type,
         all = reslist,
         chisq = Chisqure_Result,
         res =  res1,
         summary= summary
  )

}

