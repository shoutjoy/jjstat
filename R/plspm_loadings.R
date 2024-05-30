#' plspm loadings output
#'
#' @param plsres plspm result
#' @param digits round =3
#'
#' @return data table
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' plsres %>% plspm_loadings()
#' plsres$boot %>% plspm_loadings()
#' plsres$boot$paths %>% plspm_loadings()
#'
#'
#' }
#'
plspm_loadings = function(plsres, digits=3){

  if(length(plsres)==13){
    plsdf = plsres$boot$paths
  }else if(length(plsres)==5 & is.data.frame(plsres)){
    plsdf = plsres
  }else if(length(plsres)==5 & is.list(plsres)){
    plsdf = plsres$paths
  }

  res = plsres$boot$loadings%>%
    row2col("paths")%>%
    Round(digits)%>%
    add_t_sig("Mean.Boot", "Std.Error", 5, unite=TRUE, ns="")%>%
    unite_ci()%>%
    dplyr::rename(측정항목 = paths,
                  비표준화계수=Original,
                  Boot.평균=Mean.Boot,
                  표준오차=Std.Error,
                  신뢰구간95=`95%CI`)
  res
}
