#' Drawing Structural Equation Models Using the lavaan Syntax (park JH PhD)
#'
#' @param model lavaan syntax
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
#' @param type type ="plot, and res is lavaan simdata result
#'
#' @return plot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' model_test <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     # y1 ~~ y5
#'     # y2 ~~ y4 + y6
#'     # y3 ~~ y7
#'     # y4 ~~ y8
#'     # y6 ~~ y8
#' '
#'
#' HSmodel <- ' visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9 '
#'
#'
#' m1a <- "
#' y1 =~ x1 + x2 +x5
#' y2 =~ x3 + x4 +x6
#' y1 ~~ y2"
#'
#' diagram_model(model= HSmodel, groups=F,  residuals=F)
#'
#' diagram_model(model= model_test, style="ram", groups=F)
#' diagram_model(model= model_test, residuals=T)
#' diagram_model(model= m1a, residuals=T)
#'
#'
#' #plspm model
#' #model 1
#' model1 = "
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL+ EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#' sat_path = plspm_lav2path(model1,
#'                           fct_order=c("IMAG", "EXPE","QUAL", "VAL", "SAT", "LOY" ))
#' sat_path
#'
#' blocks_model ="
#' IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
#' EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
#' QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
#' VAL =~ val1 + val2 + val3 + val4
#' SAT =~ sat1 + sat2 + sat3 + sat4
#' LOY =~ loy1 + loy2 + loy3 + loy4
#' "
#' blocks_model
#' plspm_lav2blocks(blocks_model)
#'
#' #model 그리기
#' paste(model1, blocks_model)%>%
#'
#'   diagram_model(rotation = 1, sizeLat = 6, sizeMan = 4, sizeMan2=3,
#'                 mar=c(2,2,2,2))
#'
#' #'
#' #'
#' #'
#' }
diagram_model = function(model,
                         residuals= FALSE,
                         whatLabels = "model",
                         rotation = 2,
                         residScale = 12,
                         layout = "tree2",
                         edge.label.cex= 1,
                         edge.label.position =0.5,
                         edge.color = "black",
                         sizeMan = 8,
                         sizeMan2 = 4,
                         sizeLat = 10,
                         sizeLat2 = 6,
                         style="lisrel",
                         sig = FALSE,
                         exoVar = FALSE,
                         exoCov = TRUE,
                         curve = 1.5, asize=2,
                         mar = c(3,8,3,10),
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
                         type="plot"){

  #first step : Determining the model type
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
                                    model.type = model.type  )

  # test calculated
  lav_obj = lavaan::sem(model, data= testdata)

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
        style =  style,  mar=mar)
    }

  }


  if(sig){
    dia = dia%>%
      semptools::mark_sig(lav_obj) %>%plot()
  }else{
    dia = dia
  }
  switch(type, plot = dia, res = lav_obj)
}


#' Drawing Structural Equation Models Using the lavaan Syntax (park JH PhD)
#'
#' @param model lavaan syntax
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
#' @param group_match group_match lat
#' @param border.width border.width=1
#' @param nodeLabels nodeLabels=NULL
#' @param growth growth= FALSE, TRUE  model.type ="grouwth"
#' @param structural structural = FALSE
#' @param fixedStyle fixedStyle=1 default rbase linetype
#' @param type type ="plot, and res is lavaan simdata result
#'
#' @return plot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' model_test <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a*y2 + b*y3 + c*y4
#'      dem65 =~ y5 + a*y6 + b*y7 + c*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     # y1 ~~ y5
#'     # y2 ~~ y4 + y6
#'     # y3 ~~ y7
#'     # y4 ~~ y8
#'     # y6 ~~ y8
#' '
#'
#' HSmodel <- ' visual  =~ x1 + x2 + x3
#'              textual =~ x4 + x5 + x6
#'              speed   =~ x7 + x8 + x9 '
#'
#'
#' m1a <- "
#' y1 =~ x1 + x2 +x5
#' y2 =~ x3 + x4 +x6
#' y1 ~~ y2"
#'
#' sem_model(model= HSmodel, groups=F,  residuals=F)
#'
#' sem_model(model= model_test, style="ram", groups=F)
#' sem_model(model= model_test, residuals=T)
#' sem_model(model= m1a, residuals=T)
#'
#' #'
#' }
sem_model = function(model,
                         residuals= FALSE,
                         whatLabels = "est",
                         rotation = 2,
                         residScale = 12,
                         layout = "tree2",
                         edge.label.cex= 1,
                         edge.label.position =0.5,
                         edge.color = "black",
                         sizeMan = 8,
                         sizeMan2 = 4,
                         sizeLat = 10,
                         sizeLat2 = 6,
                         style="lisrel",
                         sig = FALSE,
                         exoVar = FALSE,
                         exoCov = TRUE,
                         curve = 1.5, asize=2,
                         mar = c(3,8,3,10),
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
                         type="plot"){

  #first step : Determining the model type
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
                                    model.type = model.type  )

  # test calculated
  lav_obj = lavaan::sem(model, data= testdata)

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
      groups = group_match, pastel = TRUE,
      curve = curve,
      nDigits = nDigits,
      asize= asize,
      fixedStyle = fixedStyle,
      # edgeLabels = edgeLabels,
      # nodeLabels = nodeLabels,
      structural = structural,
      style =  style,  mar=mar)
  }else{

    if(is.null(edgeLabels) & is.null(nodeLabels)){
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
        # edgeLabels = edgeLabels,
        # nodeLabels = nodeLabels,
        structural = structural,
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
        style =  style,  mar=mar)
    }

  }


  if(sig){
    dia = dia%>%
      semptools::mark_sig(lav_obj) %>%plot()
  }else{
    dia = dia
  }
  switch(type, plot = dia, res = lav_obj)
}
