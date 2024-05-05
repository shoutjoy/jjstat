#' Structural Equation Model fit
#'
#' @param ... lavaan model
#' @param nice_table markdown table TRUE
#'
#' @return table
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' model_test1 <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' model_test2 <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#' '
#'
#' fit1 <- sem(model_test1, data = PoliticalDemocracy)
#' fit2 <- sem(model_test2, data = PoliticalDemocracy)
#'
#' sem_fit(fit1, fit2)
#' sem_fit("model-1" = fit1,"model-2" = fit2)
#' #'
#' }
sem_fit = function(..., nice_table=FALSE){
  model = list(...)

  res = lavaanExtra::nice_fit(model , nice_table=nice_table)
  res

}
