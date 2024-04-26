
#' Fast sem plot draw
#'
#'
#' @param data lavaaan object
#' @param layout layY(opt =2, you make layout), or tree3
#' @param whatLabels ste
#' @param opt 1, 2, 3,
#' @param curve -2
#' @param sizeLat  8
#' @param sizeLat2  6
#' @param sizeMan 6
#' @param sizeMan2 3
#' @param edge.label.cex 0.7
#' @param edge.label.position  0.6
#' @param fade default FALSE when opt =3
#' @param rotation rotation = 2
#' @param intercepts   intercepts =FALSE becouse semptools
#' @param se se false
#' @param groups groups
#' @param pastel pastel
#' @param residuals residuals F
#' @param residScale 12

#' @param mar c(1,5,1,5)
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # library(lavaan)
#' #import data
#' example(sem)
#'
#' sem_plot(fit)
#'
#' example(cfa)
#' sem_plot(fit)
#'
#'
#' }
sem_plot = function(data,
                    whatLabels = "std",
                    type="res1",
                    opt= 1,
                   layout = "tree3",
                   curve = 2,
                   rotation =2,
                   sizeLat = 10, sizeLat2 = 6,
                   sizeMan = 8 , sizeMan2 = 4,
                   edge.label.cex = 0.9, edge.label.position=0.6,
                   mar = c(1,5,1,5),
                   groups = "lat", pastel = TRUE,
                   fade = FALSE,
                   se = FALSE,
                   residuals= FALSE,
                   residScale = 12,
                   intercepts = FALSE
                   ){


  if(opt==1){

   res = data %>%
      semPlot::semPaths(whatLabels = whatLabels, #fade=T, posCol="gray20",
                        rotation = rotation , nCharNodes = 10, nCharEdges = 10,
                        sizeLat = sizeLat, sizeLat2 = sizeLat2,
                        sizeMan = sizeMan , sizeMan2 = sizeMan2,
                        nDigits=3,
                        layout= layout, #new
                        shapeLat="circle",
                        shapeMan ="rectangle",
                        border.width = 1.5, style="lisrel",
                        edge.label.cex = edge.label.cex,
                        edge.label.position= edge.label.position,
                        edge.color = "black",
                        curve= curve,
                        residuals = residuals,
                        residScale =residScale,
                        intercepts  = intercepts ,
                        mar = mar
      )

  }else if(opt==2){

    res = data %>%
      # semPaths(what =  "std", fade=F, posCol="gray40",
      semPlot::semPaths(whatLabels = whatLabels,
                        rotation =2, nCharNodes = 10, nCharEdges = 10, asize=1.2,
                        sizeLat = sizeLat, sizeLat2 = sizeLat2,
                        sizeMan = sizeMan , sizeMan2 = sizeMan2,
                        nDigits=3,
                        # layout= "tree2", #new
                        layout = lay, #new
                        # shapeLat="circle",
                        # shapeMan ="rectangle",
                        # group="lat", pastel=T,
                        color="gray98",
                        border.width = 1.4, style="lisrel",
                        edge.label.cex = edge.label.cex,
                        edge.label.position= edge.label.position,
                        edge.width=1.3,
                        edge.color = "gray20",
                        curve = curve,
                        exoVar = FALSE,
                        residuals = residuals,
                        residScale =residScale,
                        intercepts = intercepts,
                        mar= mar
                        )
  }else if(opt== 3){
    res = data %>%
      semPlot::semPaths(what = whatLabels, fade=fade, posCol="gray20",
                        rotation = rotation, nCharNodes = 10, nCharEdges = 10,
                        sizeLat = sizeLat, sizeLat2 = sizeLat2,
                        sizeMan = sizeMan , sizeMan2 = sizeMan2,
                        nDigits=3,
                        layout= layout, #new
                        shapeLat="circle",
                        shapeMan ="rectangle",
                        border.width = 1.5, style="lisrel",
                        edge.label.cex = edge.label.cex,
                        edge.label.position= edge.label.position,
                        edge.color = "black",
                        curve = curve,
                        groups = groups, pastel = pastel,
                        residuals = residuals,
                        residScale =residScale,
                        intercepts = intercepts,
                        mar = mar
      )

  }else if(opt==4){
    res =  data %>%
      semPlot::semPaths(whatLabels = whatLabels, #fade=T, posCol="gray20",
                        rotation = rotation, nCharNodes = 10, nCharEdges = 10,
                        sizeLat = sizeLat, sizeLat2 = sizeLat2,
                        sizeMan = sizeMan , sizeMan2 = sizeMan2,
                        nDigits=3,
                        # layout= "tree2", #new
                        layout= layout, #new
                        shapeLat="circle",
                        shapeMan ="rectangle",
                        border.width = 1.5, style="lisrel",
                        edge.label.cex = edge.label.cex,
                        edge.label.position= edge.label.position,
                        edge.color = "black",
                        curve= curve,
                        groups = groups, pastel = pastel,
                        residuals = F,
                        residScale = 12,
                        intercepts = intercepts,
                        mar = mar
      )
  }

  if(se){
  res2 = res%>%
    semptools::mark_sig(data) %>%semptools::mark_se(data,sep="\n" ) %>% plot()
  }else{
    res1 = res %>%
      semptools::mark_sig(data) %>% plot()
  }

}

#'
#'
#' #' Fast sem plot draw
#' #'
#' #' @param data lavaaan object
#' #' @param layout layY(opt =2, you make layout), or tree3
#' #' @param opt opt =1 tree3
#' #' @param mar margin
#' #'
#' #' @return plot
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #'
#' #' # library(lavaan)
#' #' #import data
#' #' example(sem)
#' #'
#' #' semplot(fit)
#' #'
#' #' example(cfa)
#' #' semplot(fit)
#' #'
#' #'
#' #' }
#' #'
#' #'
#' semplot = function(data,
#'                    layout = "tree3",
#'                    opt= 1,
#'                    mar =c(1,5,1,5)
#' ){
#'
#'
#'   if(opt==1){
#'     # x11()
#'     data %>%
#'       semPlot::semPaths(whatLabels = "std",, fade=T, posCol="gray20",
#'                         rotation =2, nCharNodes = 10, nCharEdges = 10,
#'                         sizeLat = 8, sizeLat2 = 6,
#'                         sizeMan = 6 , sizeMan2 = 3,
#'                         nDigits=3,
#'                         # layout= "tree2", #new
#'                         layout= layout, #new
#'                         shapeLat="circle",
#'                         shapeMan ="rectangle",
#'                         border.width = 1.5, style="lisrel",
#'                         edge.label.cex = 0.7, edge.label.position=0.6,
#'                         edge.color = "black",
#'                         curve= -2,
#'                         residuals = F,
#'                         residScale = 12,
#'                         mar = mar
#'       ) %>%
#'       semptools::mark_sig(data) %>% plot()
#'
#'   }else if(opt==2){
#'
#'     data %>%
#'       # semPaths(what =  "std", fade=F, posCol="gray40",
#'       semPlot::semPaths(whatLabels = "std",
#'                         rotation =2, nCharNodes = 10, nCharEdges = 10, asize=1.2,
#'                         sizeLat = 9, sizeLat2 = 6,
#'                         sizeMan = 6 , sizeMan2 = 3,
#'                         nDigits=3,
#'                         # layout= "tree2", #new
#'                         layout = lay, #new
#'                         # shapeLat="circle",
#'                         # shapeMan ="rectangle",
#'                         # group="lat", pastel=T,
#'                         color="gray98",
#'                         border.width = 1.4, style="lisrel",
#'                         edge.label.cex = 0.7, edge.label.position=0.6, edge.width=1.3,
#'                         edge.color = "gray20",
#'
#'                         curve = -2,
#'                         residuals = T, exoVar = FALSE,
#'                         residScale = 10,
#'                         mar=c(5,5,5,5)) %>%
#'       semptools::mark_sig(data) %>% plot()
#'   }
#'
#' }
