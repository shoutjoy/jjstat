#' lavaan model fit
#'
#' @param ... sem models
#' @param nice_table output model fit web
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' example(sem)
#' lav_modelfit(fit)
#' lav_modelfit(fit, nice_table = T)
#'
#' lav_modelfit("model1"=fit,"model2"= fit)
#'
#' #'
#' }
#'
#'
lav_modelfit = function(..., nice_table=FALSE){
  model = list(...)

  if(nice_table){
    res = lavaanExtra::nice_fit(model, nice_table = TRUE)
  }else{
    res = lavaanExtra::nice_fit(model )%>%
      unite_ci("rmsea.ci.lower","rmsea.ci.upper", colname="rmsea.CI")

    res=res%>%dplyr::select(
      Model,chisq, df, pvalue,chi2.df, cfi, tli, srmr, rmsea, rmsea.CI)%>%
      dplyr::rename(CFI=cfi, TLI=tli, SRMR=srmr, RMSEA= rmsea)%>%
      Unite(9, 10)
  }
  res
}
