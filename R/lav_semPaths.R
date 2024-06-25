
#' Drawing Structural Equation Models Using the lavaan Syntax (park JH PhD)
#'
#' @param plsres_boot plsres_boot to model syntax
#' @param whatLabels "model
#' @param residuals residauls TRUE
#' @param rotation rotation=2
#' @param residScale residScale=12
#' @param layout layout="tree"
#' @param edge.label.cex 1
#' @param edge.label.position 0.5
#' @param edge.color edge.color= black
#' @param edge.width edge.width=1.5
#' @param edgeLabels edgeLabels=NULL
#' @param sizeMan 8
#' @param sizeMan2 4
#' @param sizeLat 10
#' @param sizeLat2 6
#' @param style lisrel
#' @param sig FALSE
#' @param exoVar FALSE
#' @param exoCov TRUE
#' @param curve 1.5
#' @param asize 1.5
#' @param mar c(2,8,3,10)
#' @param nDigits 2
#' @param shapeLat "circle, ellipse
#' @param shapeMan retangle
#' @param shapeInt  triangle ,
#' @param Groups FALSE
#' @param group_match "FALSE"lat#'
#' @param border.width border.width=1
#' @param nodeLabels nodeLabels=NULL
#' @param structural structural = FALSE
#' @param fixedStyle fixedStyle=1 default rbase linetype
#' @param color list(lat="gray10", man="white")
#' @param label.cex label.cex
#' @param type type ="plot, and res is lavaan simdata result
#' @param rotate_resid c(lv= angle), c(f1 =90, f2=180)
#' @param se  se=FALSE
#' @param intercept  intercept
#' @return plot jutpls_boot
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#'
#' #data
#' data(MarshWenHau) # Marsh, Wen & Hau (2000)
#' # colnames(MarshWenHau) = c( paste0("x",1:6), paste0("y",1:3))
#' MarshWenHau %>%str()
#'
#'
#' model1 = "
#'
#' f1 =~ x1 + x2 + x3
#' f2 =~ x4 + x5 + x6
#' f3 =~ y1 + y2 + y3
#' "
#'
#' mwh_sem1 = sem(model1, MarshWenHau)
#' mwh_sem1%>%summary(fit=T)
#'
#'
#' re_mwh_data = lavPredict(mwh_sem1)%>%data.frame()
#' re_mwh_data
#'
#'
#'
#' # 이방법인 LMS방법임
#'
#' Intf1f2  = re_mwh_data[,"f1"] *  re_mwh_data[,"f2"]
#'
#' #fitst
#' MarshWenHau$f1_f2 = Intf1f2
#' MarshWenHau %>% str()
#'
#' model2 = "
#'
#' f1 =~ x1 + x2 + x3
#' f2 =~ x4 + x5 + x6
#' f3 =~ y1 + y2 + y3
#'
#' f1Xf2 =~ f1_f2
#'
#' f3 ~ f1 + f2 + f1Xf2
#'
#' f1Xf2 ~~ 0*f1
#' f1Xf2 ~~ 0*f2
#'
#' "
#'
#' mwh_sem2 = sem(model2, MarshWenHau)
#' mwh_sem2%>%summary(fit=T)
#' mwh_sem2%>% sem_effect()
#' x11()
#' mwh_sem2%>% lav_semPaths2(exoCov = FALSE, sig=F,
#'                           rotate_resid = c(f3 = 160))
#'
#' mwh_sem2%>% lav_semPaths2(exoCov = FALSE, sig=TRUE, Groups=TRUE,
#'                           rotate_resid = c(f3 = 160))
#'
#' mwh_sem2%>% lav_semPaths2(exoCov = FALSE, sig=TRUE,se=TRUE,
#'                           rotate_resid = c(f3 = 160))
#'
#'
#' #'
#'
#' }
#'

lav_semPaths2 = function(lav_obj,
                         whatLabels = "est",
                         rotation = 2,
                         residuals= TRUE,
                         residScale = 12,
                         layout = "tree2",
                         edge.label.cex= 0.7,
                         edge.label.position = 0.55,
                         edge.color = "black",
                         sizeMan = 6,
                         sizeMan2 = 4,
                         sizeLat = 9,
                         sizeLat2 = 6,
                         style="lisrel",
                         exoVar = FALSE,
                         exoCov = TRUE,
                         curve = 1.5,
                         asize=2,
                         mar = c(3,8,3,8),
                         nDigits = 3,
                         shapeLat="circle",
                         shapeMan="rectangle",
                         shapeInt = "triangle",
                         border.width = 2,
                         edge.width = 1.5,
                         Groups = FALSE,
                         group_match = "lat",
                         structural = FALSE,
                         edgeLabels=NULL,
                         nodeLabels=NULL,
                         fixedStyle = 1,
                         intercept= TRUE,
                         color = list(lat="gray10", man="white"),
                         label.cex = 1,
                         sig = TRUE,
                         rotate_resid= NULL, #rotation residuals c(lv = 90)
                         se=FALSE,
                         type="plot"){


  # diagram2(lavobj, "model", sig=F)
  if(Groups){
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10, layout = layout,
      edge.color = edge.color,
      rotation = rotation,
      edge.label.cex = edge.label.cex,
      edge.label.position=edge.label.position,
      residuals = residuals,
      residScale =  residScale,
      exoVar = exoVar,
      exoCov = exoCov,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat,
      sizeLat2 = sizeLat2,
      shapeLat = shapeLat,
      shapeMan = shapeMan,
      shapeInt = shapeInt,
      border.width = border.width,
      edge.width = edge.width,
      groups = group_match,
      pastel = TRUE,
      curve = curve,
      nDigits = nDigits,
      asize= asize,
      fixedStyle = fixedStyle,
      intercept= intercept,
      # edgeLabels = edgeLabels,
      # nodeLabels = nodeLabels,
      structural = structural,
      # color=color,
      label.cex = label.cex,
      style =  style,  mar=mar)
  }else{

    if(is.null(edgeLabels) & is.null(nodeLabels)){
      dia = lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol="black",
        nCharNodes = 10, layout = layout,
        edge.color= edge.color,
        rotation = rotation,
        edge.label.cex =edge.label.cex,
        edge.label.position= edge.label.position,
        residuals= residuals,
        residScale=  residScale,
        exoVar = exoVar,
        exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat,
        sizeLat2 = sizeLat2,
        shapeLat=shapeLat,
        shapeMan = shapeMan,
        shapeInt = shapeInt,
        border.width = border.width,
        edge.width = edge.width,
        curve=curve,
        nDigits = nDigits,
        asize= asize,
        fixedStyle = fixedStyle,
        intercept= intercept,
        # edgeLabels = edgeLabels,
        # nodeLabels = nodeLabels,
        structural = structural,
        color=color,
        label.cex = label.cex,
        style =  style,  mar=mar)

    }else if(!is.null(edgeLabels)){
      dia = lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol="black",
        nCharNodes = 10, layout = layout,
        edge.color=edge.color,
        rotation = rotation,
        edge.label.cex =edge.label.cex,
        edge.label.position= edge.label.position,
        residuals= residuals,
        residScale=  residScale,
        exoVar = exoVar,
        exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat,
        sizeLat2 = sizeLat2,
        shapeLat = shapeLat,
        shapeMan = shapeMan,
        shapeInt = shapeInt,
        border.width = border.width,
        edge.width = edge.width,
        curve=curve,
        nDigits = nDigits,
        asize= asize,
        fixedStyle = fixedStyle,
        edgeLabels = edgeLabels,
        intercept= intercept,
        # nodeLabels = nodeLabels,
        structural = structural,
        color = color,
        label.cex = label.cex,
        style =  style,  mar=mar)
    }else if(!is.null(nodeLabels)){
      dia = lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol="black",
        nCharNodes = 10, layout = layout,
        edge.color=edge.color,
        rotation = rotation,
        edge.label.cex =edge.label.cex,
        edge.label.position= edge.label.position,
        residuals= residuals,
        residScale=  residScale,
        exoVar = exoVar,
        exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat,
        sizeLat2 = sizeLat2,
        shapeLat=shapeLat,
        shapeMan = shapeMan,
        shapeInt = shapeInt,
        border.width = border.width,
        edge.width = edge.width,
        curve=curve,
        nDigits = nDigits,
        asize= asize,
        intercept= intercept,
        # edgeLabels = edgeLabels,
        nodeLabels = nodeLabels,
        structural = structural,
        color=color,
        label.cex= label.cex,
        style =  style,  mar=mar)
    }else if(!is.null(edgeLabels) & !is.null(nodeLabels)){
      dia = lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol="black",
        nCharNodes = 10, layout = layout,
        edge.color=edge.color,
        rotation = rotation,
        edge.label.cex =edge.label.cex,
        edge.label.position= edge.label.position,
        residuals= residuals,
        residScale=  residScale,
        exoVar = exoVar,
        exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat,
        sizeLat2 = sizeLat2,
        shapeLat=shapeLat,
        shapeMan = shapeMan,
        shapeInt = shapeInt,
        border.width = border.width,
        edge.width = edge.width,
        curve=curve,
        nDigits = nDigits,
        asize= asize,
        fixedStyle = fixedStyle,
        edgeLabels = edgeLabels,
        nodeLabels = nodeLabels,
        structural = structural,
        intercept= intercept,
        color=color,
        label.cex = label.cex,
        style =  style,  mar=mar)
    }
  }


  if(sig){

    if(is.null(rotate_resid)){
      if(se){
        dia = dia%>%
          semptools::mark_sig(lav_obj) %>%
          semptools::mark_se(lav_obj, digits= nDigits, sep="\n") %>%
          plot()

      }else{
        dia = dia%>%
          semptools::mark_sig(lav_obj) %>%
          plot()
      }

    }else{
      if(se){
        dia = dia%>%
          lav_rotate_resid(resid_list=rotate_resid)%>%
          semptools::mark_sig(lav_obj) %>%
          semptools::mark_se(lav_obj, digits= nDigits, sep="\n") %>%
          plot()

      }else{
        dia = dia%>%
          lav_rotate_resid(resid_list=rotate_resid)%>%
          semptools::mark_sig(lav_obj) %>%
          plot()
      }
    }

  }else{

    if(is.null(rotate_resid)){
      dia = dia

    }else{
      dia = dia%>%
        lav_rotate_resid(resid_list=rotate_resid)
    }
  }

  switch(type, plot = dia, res = lav_obj)
}


#lav_rotate_resid(c(진로동기= 160, 진로태도= 30))
lav_rotate_resid = function(plotdata, resid_list=NULL, plot=TRUE){
  if(!is.null(resid_list)){
    plotdata <- semptools::rotate_resid(plotdata, rotate_resid_list = resid_list)

    if(plot){

      plot(plotdata)
    }
    return(plotdata)
  } else {
    stop("Input resid list ")
  }
}


#' Drawing Structural Equation Models Using the lavaan Syntax (park JH PhD)
#'
#' @param plsres_boot plsres_boot to model syntax
#' @param whatLabels "model
#' @param residuals residauls TRUE
#' @param rotation rotation=2
#' @param residScale residScale=12
#' @param layout layout="tree"
#' @param edge.label.cex 1
#' @param edge.label.position 0.5
#' @param edge.color edge.color= black
#' @param edge.width edge.width=1.5
#' @param edgeLabels edgeLabels=NULL
#' @param sizeMan 8
#' @param sizeMan2 4
#' @param sizeLat 10
#' @param sizeLat2 6
#' @param style lisrel
#' @param sig FALSE
#' @param exoVar FALSE
#' @param exoCov TRUE
#' @param curve 1.5
#' @param asize 1.5
#' @param mar c(2,8,3,10)
#' @param nDigits 2
#' @param shapeLat "circle, ellipse
#' @param shapeMan retangle
#' @param shapeInt  triangle ,
#' @param Groups FALSE
#' @param group_match "FALSE"lat#'
#' @param border.width border.width=1
#' @param nodeLabels nodeLabels=NULL
#' @param structural structural = FALSE
#' @param fixedStyle fixedStyle=1 default rbase linetype
#' @param color list(lat="gray10", man="white")
#' @param label.cex label.cex
#' @param type type ="plot, and res is lavaan simdata result
#' @param rotate_resid c(lv= angle), c(f1 =90, f2=180)
#' @param se  se=FALSE
#' @return plot jutpls_boot
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#'
#' #data
#' data(MarshWenHau) # Marsh, Wen & Hau (2000)
#' # colnames(MarshWenHau) = c( paste0("x",1:6), paste0("y",1:3))
#' MarshWenHau %>%str()
#'
#'
#' model1 = "
#'
#' f1 =~ x1 + x2 + x3
#' f2 =~ x4 + x5 + x6
#' f3 =~ y1 + y2 + y3
#' "
#'
#' mwh_sem1 = sem(model1, MarshWenHau)
#' mwh_sem1%>%summary(fit=T)
#'
#'
#' re_mwh_data = lavPredict(mwh_sem1)%>%data.frame()
#' re_mwh_data
#'
#'
#'
#' # 이방법인 LMS방법임
#'
#' Intf1f2  = re_mwh_data[,"f1"] *  re_mwh_data[,"f2"]
#'
#' #fitst
#' MarshWenHau$f1_f2 = Intf1f2
#' MarshWenHau %>% str()
#'
#' model2 = "
#'
#' f1 =~ x1 + x2 + x3
#' f2 =~ x4 + x5 + x6
#' f3 =~ y1 + y2 + y3
#'
#' f1Xf2 =~ f1_f2
#'
#' f3 ~ f1 + f2 + f1Xf2
#'
#' f1Xf2 ~~ 0*f1
#' f1Xf2 ~~ 0*f2
#'
#' "
#'
#' mwh_sem2 = sem(model2, MarshWenHau)
#' mwh_sem2%>%summary(fit=T)
#' mwh_sem2%>% sem_effect()
#' x11()
#' mwh_sem2%>% lav_semPaths2(exoCov = FALSE, sig=F,
#'                           rotate_resid = c(f3 = 160))
#'
#' mwh_sem2%>% lav_semPaths2(exoCov = FALSE, sig=TRUE, Groups=TRUE,
#'                           rotate_resid = c(f3 = 160))
#'
#' mwh_sem2%>% lav_semPaths2(exoCov = FALSE, sig=TRUE,se=TRUE,
#'                           rotate_resid = c(f3 = 160))
#'
#'
#' #'
#'
#' }
#'

lav_semPaths = function(lav_obj,
                         whatLabels = "est",
                         rotation = 2,
                         residuals= TRUE,
                         residScale = 12,
                         layout = "tree2",
                         edge.label.cex= 0.7,
                         edge.label.position = 0.55,
                         edge.color = "black",
                         sizeMan = 6,
                         sizeMan2 = 4,
                         sizeLat = 9,
                         sizeLat2 = 6,
                         style="lisrel",
                         exoVar = FALSE,
                         exoCov = TRUE,
                         curve = 1.5,
                         asize=2,
                         mar = c(3,8,3,8),
                         nDigits = 3,
                         shapeLat="circle",
                         shapeMan="rectangle",
                         shapeInt = "triangle",
                         border.width = 2,
                         edge.width = 1.5,
                         Groups = FALSE,
                         group_match = "lat",
                         structural = FALSE,
                         edgeLabels=NULL,
                         nodeLabels=NULL,
                         fixedStyle = 1,
                        intercept= FALSE,
                         color = list(lat="gray10", man="white"),
                         label.cex = 1,
                         sig = TRUE,
                         rotate_resid= NULL, #rotation residuals c(lv = 90)
                         se=FALSE,
                         type="plot"){


  # diagram2(lavobj, "model", sig=F)
  if(Groups){
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10, layout = layout,
      edge.color = edge.color,
      rotation = rotation,
      edge.label.cex = edge.label.cex,
      edge.label.position=edge.label.position,
      residuals = residuals,
      residScale =  residScale,
      exoVar = exoVar,
      exoCov = exoCov,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat,
      sizeLat2 = sizeLat2,
      shapeLat = shapeLat,
      shapeMan = shapeMan,
      shapeInt = shapeInt,
      border.width = border.width,
      edge.width = edge.width,
      groups = group_match,
      pastel = TRUE,
      curve = curve,
      nDigits = nDigits,
      asize= asize,
      fixedStyle = fixedStyle,
      intercept= intercept,
      # edgeLabels = edgeLabels,
      # nodeLabels = nodeLabels,
      structural = structural,
      # color=color,
      label.cex = label.cex,
      style =  style,  mar=mar)
  }else{

    if(is.null(edgeLabels) & is.null(nodeLabels)){
      dia = lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol="black",
        nCharNodes = 10, layout = layout,
        edge.color= edge.color,
        rotation = rotation,
        edge.label.cex =edge.label.cex,
        edge.label.position= edge.label.position,
        residuals= residuals,
        residScale=  residScale,
        exoVar = exoVar,
        exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat,
        sizeLat2 = sizeLat2,
        shapeLat=shapeLat,
        shapeMan = shapeMan,
        shapeInt = shapeInt,
        border.width = border.width,
        edge.width = edge.width,
        curve=curve,
        nDigits = nDigits,
        asize= asize,
        fixedStyle = fixedStyle,
        intercept= intercept,
        # edgeLabels = edgeLabels,
        # nodeLabels = nodeLabels,
        structural = structural,
        color=color,
        label.cex = label.cex,
        style =  style,  mar=mar)

    }else if(!is.null(edgeLabels)){
      dia = lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol="black",
        nCharNodes = 10, layout = layout,
        edge.color=edge.color,
        rotation = rotation,
        edge.label.cex =edge.label.cex,
        edge.label.position= edge.label.position,
        residuals= residuals,
        residScale=  residScale,
        exoVar = exoVar,
        exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat,
        sizeLat2 = sizeLat2,
        shapeLat = shapeLat,
        shapeMan = shapeMan,
        shapeInt = shapeInt,
        border.width = border.width,
        edge.width = edge.width,
        curve=curve,
        nDigits = nDigits,
        asize= asize,
        fixedStyle = fixedStyle,
        edgeLabels = edgeLabels,
        intercept= intercept,
        # nodeLabels = nodeLabels,
        structural = structural,
        color = color,
        label.cex = label.cex,
        style =  style,  mar=mar)
    }else if(!is.null(nodeLabels)){
      dia = lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol="black",
        nCharNodes = 10, layout = layout,
        edge.color=edge.color,
        rotation = rotation,
        edge.label.cex =edge.label.cex,
        edge.label.position= edge.label.position,
        residuals= residuals,
        residScale=  residScale,
        exoVar = exoVar,
        exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat,
        sizeLat2 = sizeLat2,
        shapeLat=shapeLat,
        shapeMan = shapeMan,
        shapeInt = shapeInt,
        border.width = border.width,
        edge.width = edge.width,
        curve=curve,
        nDigits = nDigits,
        asize= asize,
        # edgeLabels = edgeLabels,
        nodeLabels = nodeLabels,
        structural = structural,
        color=color,
        label.cex= label.cex,
        style =  style,  mar=mar)
    }else if(!is.null(edgeLabels) & !is.null(nodeLabels)){
      dia = lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol="black",
        nCharNodes = 10, layout = layout,
        edge.color=edge.color,
        rotation = rotation,
        edge.label.cex =edge.label.cex,
        edge.label.position= edge.label.position,
        residuals= residuals,
        residScale=  residScale,
        exoVar = exoVar,
        exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat,
        sizeLat2 = sizeLat2,
        shapeLat=shapeLat,
        shapeMan = shapeMan,
        shapeInt = shapeInt,
        border.width = border.width,
        edge.width = edge.width,
        curve=curve,
        nDigits = nDigits,
        asize= asize,
        fixedStyle = fixedStyle,
        edgeLabels = edgeLabels,
        nodeLabels = nodeLabels,
        structural = structural,
        intercept= intercept,
        color=color,
        label.cex = label.cex,
        style =  style,  mar=mar)
    }
  }


  if(sig){

    if(is.null(rotate_resid)){
      if(se){
        dia = dia%>%
          semptools::mark_sig(lav_obj) %>%
          semptools::mark_se(lav_obj, digits= nDigits, sep="\n") %>%
          plot()

      }else{
        dia = dia%>%
          semptools::mark_sig(lav_obj) %>%
          plot()
      }

    }else{
      if(se){
        dia = dia%>%
          lav_rotate_resid(resid_list=rotate_resid)%>%
          semptools::mark_sig(lav_obj) %>%
          semptools::mark_se(lav_obj, digits= nDigits, sep="\n") %>%
          plot()

      }else{
        dia = dia%>%
          lav_rotate_resid(resid_list=rotate_resid)%>%
          semptools::mark_sig(lav_obj) %>%
          plot()
      }
    }

  }else{

    if(is.null(rotate_resid)){
      dia = dia

    }else{
      dia = dia%>%
        lav_rotate_resid(resid_list=rotate_resid)
    }
  }

  switch(type, plot = dia, res = lav_obj)
}

#
# #lav_rotate_resid(c(진로동기= 160, 진로태도= 30))
# lav_rotate_resid = function(plotdata, resid_list=NULL, plot=TRUE){
#   if(!is.null(resid_list)){
#     plotdata <- semptools::rotate_resid(plotdata, rotate_resid_list = resid_list)
#
#     if(plot){
#
#       plot(plotdata)
#     }
#     return(plotdata)
#   } else {
#     stop("Input resid list ")
#   }
# }

