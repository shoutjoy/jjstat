
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
#' @param sizeLat2  sizeLat2 = 6
#' @param structual  structual=FALSE
#' @param style  style="lisrel"
#' @param sig sig=TRUE
#' @param residuals residuals=TRUE
#' @param curve curve=1
#' @param asize asize = 1.5
#' @param layout "tree"
#' @param shapeLat shapeLat ="circle",
#' @param edge.width edge.width 1.5
#' @param border.width border.width 2
#' @param exoVar exoVar=TRUE,
#' @param exoCov  exoCov=TRUE,
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
#'
#' #making method layout
#' make_layout(nc=10)
#' make_layout(nc=10, nr=6)
#' make_layout(3,6)
#'
#' lay1 = make_layout(6)
#' lay1
#'
#' edit_layout(lay1)
#' lay1 %>%edit_layout()
#' lay2 = lay1 %>%edit_layout()
#' lay2
#'
#' }
#'
#'
diagram2 = function(lav_obj, whatLabels = "est",
                    rotation = 2, nDigits=2,  residScale=18,
                    mar=c(2,8,3,8),
                    edge.label.cex= 1,
                    sizeMan = 8, sizeMan2 = 4,
                    sizeLat = 10,  sizeLat2 = 6,
                    border.width = 2,
                    edge.width = 1.5,
                    style="lisrel",sig=TRUE,
                    asize= 1.5, shapeLat ="circle",
                    residuals=FALSE,curve=1,layout = "tree",
                    exoVar=TRUE, exoCov=TRUE,
                    structual=FALSE,
                    groups=FALSE
){

  if(groups){
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10,layout = layout,
      edge.color="black",
      rotation = rotation,
      edge.label.cex =edge.label.cex,
      residuals= residuals,
      residScale=  residScale,
      exoVar=exoVar, exoCov=exoCov,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat, sizeLat2 = sizeLat2,
      shapeLat =shapeLat,
      border.width = border.width,
      groups ="lat",pastel = TRUE,
      edge.width = edge.width,
      curve = curve,
      asize= asize,
      nDigits = nDigits,
      structual=structual,
      style =  style,  mar=mar)
  }else{
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10,layout = layout,
      edge.color="black",
      rotation = rotation,
      edge.label.cex =edge.label.cex,
      residuals = residuals,
      residScale =  residScale,
      exoVar=exoVar, exoCov=exoCov,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat, sizeLat2 = sizeLat2,
      shapeLat=shapeLat,
      border.width = border.width,
      edge.width = edge.width,
      curve=curve,
      nDigits = nDigits,
      asize= asize,
      structual=structual,
      style =  style,  mar=mar)
  }


  if(sig){
    dia%>%
      semptools::mark_sig(lav_obj) %>%plot()
  }else{
    dia
  }

}
