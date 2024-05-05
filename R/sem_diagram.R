
#' diagram sem plot
#'
#' @param lav_obj lavaan
#' @param what est, std, model
#' @param rotation 4
#' @param nDigits 3
#' @param residScale 18
#' @param sig FALSE
#' @param edge.label.cex 1
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#' example(cfa)
#' fit %>% diagram()
#'
#' }
#'
diagram = function(lav_obj, what="est",
                   rotation = 4, nDigits=2,  residScale=18, sig=TRUE,
                   edge.label.cex = 1.5){

  dia= lav_obj %>% semPlot::semPaths(
    what= what,  nCharNodes = 10,
    fade = FALSE, posCol="gray20",
    rotation = rotation,
    edge.label.cex = edge.label.cex,
    residuals= T, residScale=  residScale,
    exoVar=TRUE,exoCov=TRUE,
    sizeMan = 10, sizeMan2 = 6,
    sizeLat = 15,
    shapeLat="circle",
    border.width = 2,
    nDigits = nDigits,
    style="lisrel", mar=c(2,8,3,8))

  if(sig){
    dia%>%
      semptools::mark_sig(lav_obj) %>%plot()
  }else{
    dia
  }

}

#' diagram sem plot 2
#'
#' @param lav_obj lavaan
#' @param whatLabels est, std, model(sig=FALSE)
#' @param rotation  rotation= 4
#' @param nDigits 3
#' @param residScale residScale=18
#' @param mar c(2,8,3,10)
#' @param edge.label.cex edge.label.cex= 1
#' @param sizeMan  sizeMan = 8,
#' @param sizeMan2  sizeMan2 = 4,
#' @param sizeLat  sizeLat = 10,
#' @param style  style="lisrel"
#' @param sig sig=TRUE
#' @param residuals residuals=TRUE
#' @param curve curve=1
#' @param groups true color
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' example(sem)
#' fit%>% diagram2()
#' #'
#' fit %>%diagram2(mar=c(3,8,3,6), sizeLat = 6, sizeMan = 6,
#'                 edge.label.cex = 0.7)
#'
#' #model
#' fit %>%diagram2("model", sig=F)
#'
#' fit %>%diagram2("model", sig=F, residuals=F, curve=2)
#'
#' fit %>%diagram2("model", sig=F, residuals=F, curve=2, rotation = 4)
#' #'
#'
#'
#' }
#'
#'
diagram2 = function(lav_obj, whatLabels = "est",
                    rotation = 2, nDigits=2,  residScale=18,
                    mar=c(2,8,3,10),
                    edge.label.cex= 1,    sizeMan = 8, sizeMan2 = 4,
                    sizeLat = 10, style="lisrel",sig=TRUE,
                    residuals=FALSE,curve=1,
                    groups=FALSE
){

  if(groups){
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10,
      edge.color="black",
      rotation = rotation,
      edge.label.cex =edge.label.cex,
      residuals= residuals,
      residScale=  residScale,
      exoVar=TRUE, exoCov=TRUE,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat,
      shapeLat="circle",
      border.width = 2,
      groups="lat",pastel=TRUE,
      edge.width=1.5,
      curve=curve,
      nDigits = nDigits,
      style =  style,  mar=mar)
  }else{
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10,
      edge.color="black",
      rotation = rotation,
      edge.label.cex =edge.label.cex,
      residuals= residuals,
      residScale=  residScale,
      exoVar=TRUE, exoCov=TRUE,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat,
      shapeLat="circle",
      border.width = 2,
      edge.width=1.5,
      curve=curve,
      nDigits = nDigits,
      style =  style,  mar=mar)
  }


  if(sig){
    dia%>%
      semptools::mark_sig(lav_obj) %>%plot()
  }else{
    dia
  }

}
