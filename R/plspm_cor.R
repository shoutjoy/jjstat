#' Tplspm result structral cor
#'
#' @param plsres plspm result
#'
#' @return matrix
#' @export
#'

plspm_cor = function(plsres){
  if(length(plsres)==13){
    plsdf = plsres$scores
  }else{
    plsdf = plsres
  }

  res =  plsdf %>%
    cor() %>%
    lowerMat("","") %>%
    dall()
  res
}
