
#' Functions to check the chi-square test of a paper
#'
#' @param input vectro
#' @param ncol matrix col
#' @param colname  input
#' @param rowname  input
#' @param simulate.p.value  default FaLSE fisher exact test
#' @param type  all, chisq
#'
#' @return chisquare data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ##Mina Choi. (2007).
#' Chisq_retest(c(111, 33, 64, 58), ncol=2 )
#'
#' Chisq_retest(c(111, 33, 64, 58), ncol=2, type="chisq" )
#'
#' Chisq_retest(c(111, 33, 64, 58), ncol=2,
#' colname = c("experience", "Non-experience"),
#' rowname =c("E-Leanring","FaceToFace"))
#'
#' }
#'
Chisq_retest <- function(input, ncol = 2,
                          colname = NULL,
                          rowname = NULL,
simulate.p.value = FALSE, type= "all"){

  data <- matrix( input, ncol = ncol)

  if(!is.null(colname)){
    dimnames(data) <- list(staus = colname, rowname)
  }else{
    data <- data
  }


  res <- chisq.test(data, simulate.p.value = simulate.p.value)
  #정확도 문제를 해결하기 위한 replicates 적용
  # res
  Chisqure_Result = cbind(Chisq = res$statistic,
                          pvalue = res$p.value,
                          df = res$parameter)
  reslist <- list(
    Chisqure_Result = Chisqure_Result,
    # res$data.name,
    Observed = res$observed,
    Expected = res$expected,
    Residual = res$residuals,
    Stdres = res$stdres,
    Analysis_Method = res$method,
    Ratio_obs_exp = res$observed/res$expected
  )

  switch(type,
         all= reslist,
         chisq = Chisqure_Result)

}
