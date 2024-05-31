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
#' @param sample.nobs sample.nobs = 100
#' @param Groups FALSE
#' @param group_match "FALSE"lat#'
#' @param border.width border.width=1
#' @param nodeLabels nodeLabels=NULL
#' @param growth growth= FALSE, TRUE  model.type ="grouwth"
#' @param structural structural = FALSE
#' @param fixedStyle fixedStyle=1 default rbase linetype
#' @param label.cex label.cex
#' @param type type ="plot, and res is lavaan simdata result
#'
#' @return plot
#'
#' @export
#'
plspm_semPaths = function(plsres_boot,
                          residuals= FALSE,
                          whatLabels = "model",
                          rotation = 2,
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
                          sig = FALSE,
                          exoVar = FALSE,
                          exoCov = FALSE,
                          curve = 1.5,
                          asize=2,
                          mar = c(2,4,2,4),
                          nDigits = 3,
                          shapeLat="circle",
                          shapeMan="rectangle",
                          shapeInt = "triangle",
                          sample.nobs = 100,
                          border.width = 2,
                          edge.width = 1.5,
                          Groups = FALSE,
                          group_match = "lat",
                          growth = FALSE,
                          structural = FALSE,
                          edgeLabels=NULL,
                          nodeLabels=NULL,
                          fixedStyle = 1,
                          label.cex = 1,
                          type="plot"){

  #first step : Determining the model type
  mms1 = summary(plsres_boot)$outer_model%>%
    dplyr::select(name, block, loading) %>% #Round(3)%>%
    dplyr::mutate(item = paste0(block," =~ ", name)) %>%
    dplyr::select(item) %>%
    unlist()%>%
    as.character() %>%
    paste( collapse="; ")

  sms1 = summary(plsres_boot)$effects %>%
    dplyr::select(relationships, direct)%>%
    dplyr::filter(direct != 0)%>%
    tidyr::separate(relationships, c("lhs","rhs"), sep="->")%>%
    dplyr::mutate(path = paste0(rhs," ~ ",lhs)) %>%
    dplyr::select(path) %>%
    unlist()%>%
    as.character() %>%
    paste( collapse="; ")

  model = paste(mms1, sms1, sep="; ")


  if(is.null(edgeLabels)){
    edgeLabels = c(plspm_loadings_values(plsres_boot, type="vec"),
                   plspm_boot_paths_sig(plsres_boot, type="vec"))
  }else{
    edgeLabels = edgeLabels
  }

  ####
  model_string = model

  # Remove instances of '~~' and '=~' from the model string
  modified_model_string <- gsub("~~", "", model_string)
  modified_model_string <- gsub("=~", "", modified_model_string)

  # Check if the modified model string contains any '~'

  if(growth){
    model.type ="growth"
  }else{
    if (grepl("~", modified_model_string, fixed = TRUE)) {
      # return("sem")
      model.type ="sem"
    } else {
      # return("cfa")
      model.type ="cfa"
    }
  }


  #generate sample data simulated
  testdata =  lavaan::simulateData( model = model,
                                    sample.nobs = sample.nobs,
                                    model.type = model.type)%>%
                                      suppressWarnings()

  # test calculated
  lav_obj = lavaan::sem(model, data= testdata)%>%
                        suppressWarnings()

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
      # edgeLabels = edgeLabels,
      # nodeLabels = nodeLabels,
      structural = structural,
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
        # edgeLabels = edgeLabels,
        # nodeLabels = nodeLabels,
        structural = structural,
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
        # nodeLabels = nodeLabels,
        structural = structural,
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
        label.cex = label.cex,
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
        label.cex = label.cex,
        style =  style,  mar=mar)
    }
  }


  if(sig){
    dia = dia%>%
      semptools::mark_sig(lav_obj) %>%plot()
  }else{
    dia = dia
  }

  cat("\n Node names in order of appearance \n\n")
  print(c(plsres_boot$model$gens$mvs_names,
          plsres_boot$model$gens$lvs_names))
  cat("\n\n")
  switch(type, plot = dia, res = lav_obj)


}
