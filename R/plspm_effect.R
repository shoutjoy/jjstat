#' plspm_effect
#'
#' @param pls_boot pls boot
#' @param digits round 3
#'
#' @return data
#' @export
#'

plspm_effect= function(pls_boot, digits=3){
  if(length(pls_boot)!=13){
    stop("You must input Boostrapdata")
  }

  effec1 = pls_boot$effect
  effect_sig = pls_boot$boot$total.efs

  res= full_join(effec1, effect_sig%>%
                   row2col("relationships")%>%
                   unite_ci()%>%
                   add_t_sig(3,4,5,TRUE, ns="")%>%
                   dplyr::select(1,5,6),
                 by="relationships") %>% Round(digits)
  return(res)
}
