#' #' Title
#' #'
#' #' @param plsres
#' #' @param accept
#' #' @param type
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #'
#' #'
#' #' }
#' #'
#' pls_boot_effect <- function(plsres, accept=FALSE, type="res"){
#'   #bootsrap effect signification
#'   plsres$boot$total.efs <- plsres$boot$total.efs %>% filter(Original>0)
#'
#'
#'   path.tvalue = with(plsres$boot$total.efs, Original/Std.Error)
#'   #path.tvalue
#'   cbind(plsres$boot$total.efs, path.tvalue)
#'   boottotal.efs<- round(cbind(plsres$boot$total.efs, path.tvalue),3)
#'   boottotal.efs
#'   #boottotal.efs[is.na(boottotal.efs)]<-"0"
#'
#'   #t.value에 star * 표시
#'   boottotal.efs[is.na(boottotal.efs)]<- "0"
#'   boottotal.efs$star <- ""  #변수 생성
#'   boottotal.efs[boottotal.efs$path.tvalue > 1.96,"star"]= "*"
#'   boottotal.efs[boottotal.efs$path.tvalue > 2.58,"star"]= "**"
#'   boottotal.efs[boottotal.efs$path.tvalue > 3.29 ,"star"]= "***"
#'   # boottotal.efs
#'   #t-value에 선택에 ㄷ애한 것
#'   #boottotal.efs
#'   if(accept){
#'     boottotal.efs[,"Accept"]="No"
#'     boottotal.efs[boottotal.efs$path.tvalue >= 1.96,"Accept"]="Yes"
#'
#'     boottotal.efs = boottotal.efs%>% unite(CI, perc.025, perc.975,sep=", " )%>%
#'       mutate(CI = paste0("[",CI,"]"))%>%
#'       rename(se = Std.Error, t = path.tvalue,
#'              EstBoot = Mean.Boot, Est = Original,
#'              sig = star)%>%
#'       unite(t, t, sig,sep="" )
#'     res = boottotal.efs%>%row2col("Paths")
#'
#'
#'     Res = res%>%dplyr::select(Paths, Est,EstBoot,t, CI, Accept)
#'     all= res
#'   }else{
#'     boottotal.efs = boottotal.efs%>% unite(CI, perc.025, perc.975,sep=", " )%>%
#'       mutate(CI = paste0("[",CI,"]"))%>%
#'       rename(se = Std.Error, t = path.tvalue,
#'              EstBoot = Mean.Boot, Est = Original,
#'              sig = star)%>%
#'       unite(t, t, sig,sep="" )
#'     res = boottotal.efs%>%row2col("Paths")
#'
#'
#'     Res = res%>%dplyr::select(Paths, Est,EstBoot,t, CI)
#'     all= res
#'   }
#'   switch(type, all =all ,res =Res )
#'
#' }
