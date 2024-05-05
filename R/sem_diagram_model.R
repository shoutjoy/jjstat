#' Drawing Structural Equation Models Using the lavaan Syntax (park JH PhD)
#'
#' @param model lavaan syntax
#' @param whatLabels "model
#' @param residuals residauls TRUE
#' @param rotation rotation=2
#' @param residScale residScale=12
#' @param layout layout="tree"
#' @param edge.label.cex 1
#' @param sizeMan 8
#' @param sizeMan2 4
#' @param sizeLat 10
#' @param style lisrel
#' @param sig FALSE
#' @param exoVar FALSE
#' @param exoCov TRUE
#' @param curve 1.5
#' @param asize 1.5
#' @param mar c(2,8,3,10)
#' @param nDigits 2
#' @param groups FALSE
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
#' #'
#' }
diagram_model = function(model,
                         whatLabels = "model",
                         residuals= TRUE,
                         rotation = 2,
                         residScale = 12,
                         layout = "tree",
                         edge.label.cex= 1,
                         sizeMan = 8,
                         sizeMan2 = 4,
                         sizeLat = 10,
                         style="lisrel",
                         sig= FALSE,
                         exoVar=FALSE,
                         exoCov=TRUE,
                         curve=1.5, asize=2,
                         mar=c(2,8,3,10),nDigits=2,
                         groups=FALSE){

  #first step : Determining the model type
  model_string = model

  # Remove instances of '~~' and '=~' from the model string
  modified_model_string <- gsub("~~", "", model_string)
  modified_model_string <- gsub("=~", "", modified_model_string)

  # Check if the modified model string contains any '~'
  if (grepl("~", modified_model_string, fixed = TRUE)) {
    # return("sem")
    model.type ="sem"
  } else {
    # return("cfa")
    model.type ="cfa"
  }



  #generate sample data simulated
  testdata =  lavaan::simulateData( model = model,
                                    sample.nobs = 100,
                                    model.type = model.type  )
  # testdata =  simdata_sem(model = model_test, N = 100 )
  # test calculated
  lav_obj = lavaan::sem(model, data= testdata)

  # diagram2(lavobj, "model", sig=F)
  if(groups){
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10, layout = layout,
      edge.color = "black",
      rotation = rotation,
      edge.label.cex = edge.label.cex,
      residuals = residuals,
      residScale =  residScale,
      exoVar = exoVar,
      exoCov = exoCov,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat,
      shapeLat = "circle",
      border.width = 2,
      groups = "lat",pastel = TRUE,
      edge.width = 1.5,
      curve = curve,
      nDigits = nDigits,
      asize= asize,
      style =  style,  mar=mar)
  }else{
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10, layout = layout,
      edge.color="black",
      rotation = rotation,
      edge.label.cex =edge.label.cex,
      residuals= residuals,
      residScale=  residScale,
      exoVar = exoVar,
      exoCov = exoCov,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat,
      shapeLat="circle",
      border.width = 2,
      edge.width=1.5,
      curve=curve,
      nDigits = nDigits,
      asize= asize,
      style =  style,  mar=mar)
  }


  if(sig){
    dia%>%
      semptools::mark_sig(lav_obj) %>%plot()
  }else{
    dia
  }

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
#' @param sizeMan 8
#' @param sizeMan2 4
#' @param sizeLat 10
#' @param style lisrel
#' @param sig FALSE
#' @param exoVar FALSE
#' @param exoCov TRUE
#' @param curve 1.5
#' @param asize 1.5
#' @param mar c(2,8,3,10)
#' @param nDigits 2
#' @param groups FALSE
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
                         whatLabels = "model",
                         residuals= TRUE,
                         rotation = 2,
                         residScale = 12,
                         layout = "tree",
                         edge.label.cex= 1,
                         sizeMan = 8,
                         sizeMan2 = 4,
                         sizeLat = 10,
                         style="lisrel",
                         sig= FALSE,
                         exoVar=FALSE,
                         exoCov=TRUE,
                         curve=1.5, asize=2,
                         mar=c(2,8,3,10),nDigits=2,
                         groups=FALSE){

  #first step : Determining the model type
  model_string = model

  # Remove instances of '~~' and '=~' from the model string
  modified_model_string <- gsub("~~", "", model_string)
  modified_model_string <- gsub("=~", "", modified_model_string)

  # Check if the modified model string contains any '~'
  if (grepl("~", modified_model_string, fixed = TRUE)) {
    # return("sem")
    model.type ="sem"
  } else {
    # return("cfa")
    model.type ="cfa"
  }



  #generate sample data simulated
  testdata =  lavaan::simulateData( model = model,
                                    sample.nobs = 100,
                                    model.type = model.type  )
  # testdata =  simdata_sem(model = model_test, N = 100 )
  # test calculated
  lav_obj = lavaan::sem(model, data= testdata)

  # diagram2(lavobj, "model", sig=F)
  if(groups){
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10, layout = layout,
      edge.color = "black",
      rotation = rotation,
      edge.label.cex = edge.label.cex,
      residuals = residuals,
      residScale =  residScale,
      exoVar = exoVar,
      exoCov = exoCov,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat,
      shapeLat = "circle",
      border.width = 2,
      groups = "lat",pastel = TRUE,
      edge.width = 1.5,
      curve = curve,
      nDigits = nDigits,
      asize= asize,
      style =  style,  mar=mar)
  }else{
    dia =   lav_obj %>% semPlot::semPaths(
      whatLabels = whatLabels, fade = FALSE, posCol="black",
      nCharNodes = 10, layout = layout,
      edge.color="black",
      rotation = rotation,
      edge.label.cex =edge.label.cex,
      residuals= residuals,
      residScale=  residScale,
      exoVar = exoVar,
      exoCov = exoCov,
      sizeMan = sizeMan, sizeMan2 = sizeMan2,
      sizeLat = sizeLat,
      shapeLat="circle",
      border.width = 2,
      edge.width=1.5,
      curve=curve,
      nDigits = nDigits,
      asize= asize,
      style =  style,  mar=mar)
  }


  if(sig){
    dia%>%
      semptools::mark_sig(lav_obj) %>%plot()
  }else{
    dia
  }

}
