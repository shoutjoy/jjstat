#' SEM direct effect result
#'
#' @param model_sem  lavaan object
#'
#' @return repot
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  #library(lavaan) # only needed once per session
#'  models <- '
#'    # measurement model
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + y2 + y3 + y4
#'      dem65 =~ y5 + y6 + y7 + y8
#'  # regressions
#'      dem60 ~ ind60
#'      dem65 ~ ind60 + dem60
#'    # residual correlations
#'      y1 ~~ y5
#'      y2 ~~ y4 + y6
#'      y3 ~~ y7
#'      y4 ~~ y8
#'     y6 ~~ y8
#'  '
#'  fits <- sem(models, data=PoliticalDemocracy)
#'  summary(fits, standardized=TRUE)
#'
#'  ##Direct effect
#'  DE_effect(fits)

#'  DE_effect(fits)%>% interpretation_de()
#'  # same method
#'  DE_effect(fits)%>% report_sem_de()
#'
#' }
#'
#'
DE_effect = function(model_sem){

  res0 = parameterEstimates(model_sem, ci=F, stand=T)
  # Check if 'label' column exists
  if ("label" %in% colnames(res0)) {

    res = res0 %>%
      filter(op == "~") %>%
      unite(Path, rhs, lhs, sep = " -> ") %>%
      select(Path, "H" = label, est, "std" = std.all, se, z, pvalue) %>%
      p_mark_sig("pvalue") %>%
      select(-vars)
  } else {
    res = res0 %>%
      filter(op == "~") %>%
      unite(Path, rhs, lhs, sep = " -> ") %>%
      select(Path, est, "std" = std.all, se, z, pvalue) %>%
      p_mark_sig("pvalue") %>%
      select(-vars)
  }

  return(res)
}
