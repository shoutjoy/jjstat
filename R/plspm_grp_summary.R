#' plspm_grp_summary
#'
#' @param plsres plspm boot group
#' @param digits round 3
#'
#' @return table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' jutpath1 = plspm_paths(
#'   row_names = c("자기효능감","진로동기","진로태도","진로준비"),
#'   relationship = list(
#'     path(from="자기효능감", to=c("진로동기","진로태도","진로준비")),
#'     path("진로동기", c("진로준비")),
#'     path("진로태도", c("진로준비"))
#'   )  )
#' jutpath1
#'
#' plspm_paths2lav(jutpath1)%>%cat()
#' #측정모형
#' jut_blocks <- plspm_blocks(
#'   자기효능감 = item("C_S1","C_S2","C_S3","C_S4","C_S5"),
#'   진로동기 = item("A_M1","A_M2","A_M3"),
#'   진로태도 = item(  "B02", "B04", "B10","B14" ),#결정성
#'   진로준비 = item("D_P1","D_P2","D_P3")
#' )
#'
#' jutpls_boot = plspm_sem(Data= jut6, path_matrix = jutpath1,
#'                         blocks = jut_blocks,
#'                         br = 500, summary= TRUE)
#'
#' #'
#' jut_gender = plspm.groups(jutpls_boot,
#'                           group= factor(jut6$성별),
#'                           method="bootstrap")
#' jut_gender_p = plspm.groups(jutpls_boot,
#'                             group= factor(jut6$성별),
#'                             method="permutation")
#'
#' jut_gender%>%plspm_grp_summary()
#' jut_gender_p%>%plspm_grp_summary()
#' }
#'
plspm_grp_summary = function(plsres, digits=3){

  if(ncol(plsres$test)==8){
    res = plsres$test%>% row2col("paths")%>%
      rename(diff=diff.abs, t = t.stat, sig=sig.05 )%>%
      dplyr::select(-deg.fr)

  }else{
    res = plsres$test%>% row2col("paths")%>%
      rename(diff=diff.abs, sig=sig.05 )
  }
  res =  res%>%
    Round(digits)

  return(res)
}
