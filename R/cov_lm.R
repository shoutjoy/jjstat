
#' Regression analysis with covariance matrix, for paper checking
#'
#' @param formula regression formula
#' @param cov cov matrix if your data cor, then you must sds
#' @param n sample
#' @param dv dv
#' @param ivs iv variales
#' @param method c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY")
#' @param md markdown default FALSE
#' @param lm lm option TREW, FALSE sem
#'
#' @return result table, plot, confint sig plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #' #method1
#' cov_lm_sem(formula="mpg ~ cyl+disp+hp+drat+wt" ,n = 32, cov = cov(mtcars[1:6]))
#' # method 2
#' cov_lm_sem(dv = "mpg", ivs = c("cyl", "disp", "hp", "drat", "wt"), n = 32, cov = cov(mtcars[1:6]))
#' # method 2 cor matrix
#' cov_lm_sem(formula="mpg ~ cyl+disp+hp+drat+wt" ,n = 32, cov = cor(mtcars[1:6]),
#'            sds = mtcars[1:6]%>%sapply(sd))
#' #est
#' cov_lm(formula="mpg ~ cyl+disp+hp+drat+wt; qsec ~ mpg +vs;" ,
#'        n = 32, cov = cov(mtcars[1:8]),lm=FALSE)
#' #std
#' cov_lm(formula="mpg ~ cyl+disp+hp+drat+wt; qsec ~ mpg +vs;" ,
#'        n = 32, cov = cor(mtcars[1:8]), sds = sapply(mtcars[1:8], sd),
#'        lm=FALSE)
#'
#' ###################
#' #'
#' Songkj2024 = lav_matrix_lower2full(
#'   c(
#'     1,
#'     .38,   1,
#'     .49, .54, 1,
#'     .58, .33, .45, 1,
#'     .35, .17, .35, .60, 1,
#'     .22, .11, .20, .30, .28, 1,
#'     .46, .25, .46, .61, .67, .31, 1,
#'     .52, .29, .37, .57, .47, .31, .55, 1,
#'     -.09, -.03,-.02, -.20,-.05, .03, .00, .01, 1,
#'     .23, .10, .13, .07, -.03, .06, -.06, .40,-.18, 1   )
#' )
#'
#' Songkj2024
#'
#' colnames(Songkj2024)=c("학습애정열정","학습기회개방성",
#'                        "솔선수범독립성","효율학습자자아개념",
#'                        "학업적효능감","사회적효능감","자기조절효능감","경력개발역량",
#'                        "성별","연령")
#' rownames(Songkj2024)=c("학습애정열정","학습기회개방성",
#'                        "솔선수범독립성","효율학습자자아개념",
#'                        "학업적효능감","사회적효능감","자기조절효능감","경력개발역량",
#'                        "성별","연령")
#'
#' #기술통계 데이터
#' songki2024_stat=  data.frame(
#'   var = c("학습애정열정","학습기회개방성",
#'           "솔선수범독립성","효율학습자자아개념",
#'           "학업적효능감","사회적효능감","자기조절효능감","경력개발역량",
#'           "성별","연령"),
#'   mean = c(4.14, 3.86, 4.14, 3.86, 3.61, 3.44, 3.95, 3.89, 0.65, 1.70),
#'   sd =   c(.52, .72, .70, .59, .64, .82, .54, .60, 0.48, 0.82)
#'
#' );songki2024_stat
#'
#' Songkj2024_cov = cor2cov(Songkj2024, sds= songki2024_stat$sd)
#' # Songkj2024_cov
#' # original
#' Songkj2024_model_1="
#' 학업적효능감 ~ 학습애정열정 + 학습기회개방성 + 솔선수범독립성 + 효율학습자자아개념+ 성별 + 연령
#' "
#' Songkj2024_sem1 = sem( Songkj2024_model_1,
#'                        sample.cov = Songkj2024 , sample.nobs =  389  )
#'
#' #cor, std
#' cov_lm(formula="학업적효능감 ~ 학습애정열정 + 학습기회개방성 + 솔선수범독립성 + 효율학습자자아개념 + 성별 + 연령" ,n = 389, cov = Songkj2024)
#' # cov
#' cov_lm(formula="학업적효능감 ~ 학습애정열정 + 학습기회개방성 + 솔선수범독립성 + 효율학습자자아개념 + 성별 + 연령" ,n = 389, cov = Songkj2024, sds = songki2024_stat$sd)
#' # cov
#' cov_lm(formula="학업적효능감 ~ 학습애정열정 + 학습기회개방성 + 솔선수범독립성 + 효율학습자자아개념 + 성별 + 연령" ,n = 389, cov = Songkj2024_cov)

#' }
#'
cov_lm_sem <- function(formula=NULL, cov, n, sds=NULL,
                       dv, ivs,
                       method= "none",
                       md = FALSE,
                       lm = TRUE) {
  # Assumes lavaan package
  # library(lavaan)
  # dv: charcter vector of length 1 with name of outcome variable
  # ivs: character vector of names of predictors
  # n: numeric vector of length 1: sample size
  # cov: covariance matrix where row and column names
  #       correspond to dv and ivs
  # Return
  #      list with lavaan model fit
  #      and various other features of the model

  if(is.null(sds)){
    cov = cor2cov(cov)
  }else{
    cov = jjstat::cor2cov(cov, sds = sds)
  }

  #regression
  if(lm){
    results <- list()
    if(is.null(formula)){
      eq <- paste(dv, "~", paste(ivs, collapse = " + "))
    }else{
      eq = formula
      form = formula(formula)%>%as.character()
      dv = form[2]
      ivs = strsplit(form[3] , "\\s*\\+\\s*" )[[1]]
    }

    # estimate
    results$fit <- lavaan::sem(eq, sample.cov = cov, sample.nobs = n)

    # coefficients
    ufit <- parameterEstimates(results$fit, ci=TRUE)
    ufit <- ufit[ufit$op == "~", ]
    results$coef <- ufit$est
    results$p.value <- ufit$pvalue  # p
    results$z <- ufit$z  # p
    names(results$coef) <- ufit$rhs

    AIC = fitMeasures( results$fit, fit.measures=c("aic"))
    BIC = fitMeasures( results$fit, fit.measures=c("bic"))

    #std
    sfit <- standardizedsolution(results$fit)
    sfit <- sfit[sfit$op == "~", ]
    results$std <- sfit$est.std
    names(results$std) <- sfit$rhs
    #se
    results$se <- ufit$se
    names(results$se) <- ufit$rhs

    # use unclass to not limit r2 to 3 decimals
    results$r.squared <- unclass(inspect(results$fit, 'r2')) # r-squared

    # adjusted r-squared
    adjr2 <- function(rsquared, n, p) 1 - (1-rsquared)  * ((n-1)/(n-p-1))
    results$adj.r.squared <- adjr2(unclass(inspect(results$fit, 'r2')),
                                   n = n,
                                   p = length(ivs))
    #ressult
    reg = cbind.data.frame(
      DV = dv,
      estimate = results$coef,
      std = results$std,
      se = results$se,
      t =  results$z
    )%>%
      mutate(
        p.value =  p.adjust( 2 * pnorm(-abs(t)),
                             method= method))%>% p_mark_sig()
    # p.value =  p.adjust( results$p.value ,
    #                     method= method))%>% p_mark_sig()

    R2 = cbind.data.frame(  R2 = results$r.squared,
                            adj.R2 = results$adj.r.squared ,
                            AIC,BIC)%>%
      tibble::tibble()

  }else{
    # estimate SEM-------------------------------------------
    eq = formula
    # Fit <- lavaan::sem(eq, sample.cov = cov, sample.nobs = n)
    results$fit <- lavaan::sem(eq, sample.cov = cov, sample.nobs = n)
    # coefficients
    reg = parameterEstimates(results$fit, stand = TRUE, ci=TRUE) %>%
      filter(op == "~") %>%
      p_mark_sig("pvalue")

    R2 = NULL

  }

  x11()
  diagram_sig = jjstat::confint_plot_sem(results$fit)

  # diagram
  x11()
  diagram = jjstat::sem_plot(results$fit,
                             sizeMan = 15,
                             mar = c(1,10,1,5),
                             curve = 3)

  # markdown
  if(md){
    reg_md = md(reg, caption = "회귀분석결과")
  }else{
    reg_md = NULL
  }
  #result
  res = list(Cov_to_regression = reg,
             Rsquare = R2,
             plot = diagram,
             confint = diagram_sig,
             markdown_table = reg_md)
  res
}
