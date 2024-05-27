#' plspm_boot_effect Total effect
#'
#' @param plsres plsdata
#' @param accept Hypothesis
#' @param type res, all
#' @param digits 3
#'
#' @return data result
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(plspm)
#' data(satisfaction)
#'
#' # path matrix
#' IMAG = c(0,0,0,0,0,0)
#' EXPE = c(1,0,0,0,0,0)
#' QUAL = c(0,1,0,0,0,0)
#' VAL = c(0,1,1,0,0,0)
#' SAT = c(1,1,1,1,0,0)
#' LOY = c(1,0,0,0,1,0)
#' sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)
#'
#' # blocks of outer model
#' sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
#' sat_mod=c(rep("A", ncol(sat_path)))
#' # apply plspm
#' satpls = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod,
#'                scaled = FALSE)
#'
#'
#'
#'   # bootstrap data
#' pls_boot_effect(satpls_boot)
#' pls_boot_effect(satpls_boot, accept=T)
#'
#' }
#'
plspm_boot_effect <- function(plsres, accept = FALSE,
                              type = "res", digits = 3){



  #Judge input data: apply summary
  if( length(plsres) ==13){
    plsres_summary  = summary(plsres)
    boottotal.efs <- plsres_summary$boot$total.efs

  }else if(length(plsres) ==11){
    #bootsrap effect signification
    boottotal.efs <- plsres$boot$total.efs
  }else if(length(plsres)==5){
    boottotal.efs <- plsres
  }



  boottotal.efs = boottotal.efs%>%
    mutate(path.tvalue =  Original/Std.Error)%>%
    round2(digits)

  #t.value에 star * 표시
  boottotal.efs[is.na(boottotal.efs)]<- "0"
  boottotal.efs$star <- ""  #변수 생성
  boottotal.efs[boottotal.efs$path.tvalue > 1.96,"star"]= "*"
  boottotal.efs[boottotal.efs$path.tvalue > 2.58,"star"]= "**"
  boottotal.efs[boottotal.efs$path.tvalue > 3.29 ,"star"]= "***"
  # boottotal.efs
  #t-value에 선택에 ㄷ애한 것
  #boottotal.efs
  if(accept){
    boottotal.efs[,"Accept"]="No"
    boottotal.efs[boottotal.efs$path.tvalue >= 1.96,"Accept"]="Yes"

    boottotal.efs = boottotal.efs%>% tidyr::unite(CI, perc.025, perc.975,sep=", " )%>%
      dplyr::mutate(CI = paste0("[",CI,"]"))%>%
      dplyr::rename(se = Std.Error, t = path.tvalue,
                    EstBoot = Mean.Boot, Est = Original,
                    sig = star)%>%
      tidyr::unite(t, t, sig,sep="" )
    res = boottotal.efs%>%row2col("Paths")


    Res = res%>%dplyr::select(Paths, Est,EstBoot,t, CI, Accept)
    all= res

  }else{
    boottotal.efs = boottotal.efs%>%
      dplyr::mutate(perc.025= format(perc.025, nsmall=digits),
                    perc.975= format(perc.975, nsmall=digits) )%>%
      tidyr::unite(CI, perc.025, perc.975,sep=", " )%>%
      dplyr::mutate(CI = paste0("[",CI,"]"))%>%
      dplyr::rename(se = Std.Error, t = path.tvalue,
                    EstBoot = Mean.Boot, Est = Original,
                    sig = star)%>%
      tidyr::unite(t, t, sig,sep="" )
    res = boottotal.efs%>%row2col("Paths")


    Res = res%>%dplyr::select(Paths, Est, EstBoot,t, CI)%>%
      dplyr::rename(`95%CI`=CI)
    all= res
  }
  switch(type, all =all ,res =Res )

}
