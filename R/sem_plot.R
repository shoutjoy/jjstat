#' Fast sem plot draw
#'
#' @param data lavaaan object
#' @param layout layY(opt =2, you make layout), or tree3
#' @param opt opt =1 tree3
#' @param mar margin
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
#' semplot(fit)
#'
#' example(cfa)
#' semplot(fit)
#'
#'
#' }
#'
#'
semplot = function(data,
                   layout = "tree3",
                   opt= 1,
                   mar =c(1,5,1,5)
){


  if(opt==1){
    # x11()
    data %>%
      semPlot::semPaths(whatLabels = "std",, fade=T, posCol="gray20",
                        rotation =2, nCharNodes = 10, nCharEdges = 10,
                        sizeLat = 8, sizeLat2 = 6,
                        sizeMan = 6 , sizeMan2 = 3,
                        nDigits=3,
                        # layout= "tree2", #new
                        layout= layout, #new
                        shapeLat="circle",
                        shapeMan ="rectangle",
                        border.width = 1.5, style="lisrel",
                        edge.label.cex = 0.7, edge.label.position=0.6,
                        edge.color = "black",
                        curve= -2,
                        residuals = F,
                        residScale = 12,
                        mar = mar
      ) %>%
      semptools::mark_sig(data) %>% plot()

  }else if(opt==2){

    data %>%
      # semPaths(what =  "std", fade=F, posCol="gray40",
      semPlot::semPaths(whatLabels = "std",
                        rotation =2, nCharNodes = 10, nCharEdges = 10, asize=1.2,
                        sizeLat = 9, sizeLat2 = 6,
                        sizeMan = 6 , sizeMan2 = 3,
                        nDigits=3,
                        # layout= "tree2", #new
                        layout = lay, #new
                        # shapeLat="circle",
                        # shapeMan ="rectangle",
                        # group="lat", pastel=T,
                        color="gray98",
                        border.width = 1.4, style="lisrel",
                        edge.label.cex = 0.7, edge.label.position=0.6, edge.width=1.3,
                        edge.color = "gray20",

                        curve = -2,
                        residuals = T, exoVar = FALSE,
                        residScale = 10,
                        mar=c(5,5,5,5)) %>%
      semptools::mark_sig(data) %>% plot()
  }

}



#' Fast sem plot draw
#'
#' @param data lavaaan object
#' @param layout layY(opt =2, you make layout), or tree3
#' @param opt opt =1 tree3
#' @param mar margin
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
#' semplot(fit)
#'
#' example(cfa)
#' semplot(fit)
#'
#'
#' }
#'
#'
sem_plot = function(data,
                   layout = "tree3",
                   opt= 1,
                   mar =c(1,5,1,5)
){


  if(opt==1){
    # x11()
    data %>%
      semPlot::semPaths(whatLabels = "std",, fade=T, posCol="gray20",
                        rotation =2, nCharNodes = 10, nCharEdges = 10,
                        sizeLat = 8, sizeLat2 = 6,
                        sizeMan = 6 , sizeMan2 = 3,
                        nDigits=3,
                        # layout= "tree2", #new
                        layout= layout, #new
                        shapeLat="circle",
                        shapeMan ="rectangle",
                        border.width = 1.5, style="lisrel",
                        edge.label.cex = 0.7, edge.label.position=0.6,
                        edge.color = "black",
                        curve= -2,
                        residuals = F,
                        residScale = 12,
                        mar = mar
      ) %>%
      semptools::mark_sig(data) %>% plot()

  }else if(opt==2){

    data %>%
      # semPaths(what =  "std", fade=F, posCol="gray40",
      semPlot::semPaths(whatLabels = "std",
                        rotation =2, nCharNodes = 10, nCharEdges = 10, asize=1.2,
                        sizeLat = 9, sizeLat2 = 6,
                        sizeMan = 6 , sizeMan2 = 3,
                        nDigits=3,
                        # layout= "tree2", #new
                        layout = lay, #new
                        # shapeLat="circle",
                        # shapeMan ="rectangle",
                        # group="lat", pastel=T,
                        color="gray98",
                        border.width = 1.4, style="lisrel",
                        edge.label.cex = 0.7, edge.label.position=0.6, edge.width=1.3,
                        edge.color = "gray20",

                        curve = -2,
                        residuals = T, exoVar = FALSE,
                        residScale = 10,
                        mar=c(5,5,5,5)) %>%
      semptools::mark_sig(data) %>% plot()
  }

}
