#' semPlot::semPaths wrapper with customization for grouped or non-grouped path diagrams
#'
#' A convenient wrapper around `semPlot::semPaths` with additional options for layout, labels,
#' grouping, and significance marking using `semptools::mark_sig`.
#'
#' @param lav_obj A fitted lavaan object.
#' @param whatLabels Type of labels to display on edges (e.g., `"est"` for estimates).
#' @param rotation Rotation angle for layout.
#' @param nDigits Number of digits to round estimates.
#' @param residScale Scaling factor for residuals.
#' @param mar Margins of the plot.
#' @param edge.label.cex Character expansion factor for edge labels.
#' @param edge.label.position Position of edge labels along the edge.
#' @param sizeMan Size of manifest variables.
#' @param sizeMan2 Size of exogenous manifest variables.
#' @param sizeLat Size of latent variables.
#' @param sizeLat2 Size of exogenous latent variables.
#' @param border.width Border width of nodes.
#' @param edge.width Line width of edges.
#' @param style Style of path diagram (e.g., `"lisrel"`).
#' @param sig Logical. If TRUE, significant paths are marked.
#' @param asize Arrow size.
#' @param shapeLat Shape of latent variables (e.g., `"circle"`).
#' @param residuals Logical. If TRUE, show residuals.
#' @param curve Curvature of edges.
#' @param layout Layout of the graph (e.g., `"tree"`, `"circle"`).
#' @param exoVar Logical. If TRUE, include exogenous variables.
#' @param exoCov Logical. If TRUE, include covariances of exogenous variables.
#' @param structual Logical. If TRUE, plot structural model only.
#' @param nodeLabels Optional custom node labels.
#' @param groups Logical. If TRUE, color nodes by latent groupings.
#'
#' @return A path diagram object, optionally plotted if `sig = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' library(semPlot)
#' library(semptools)
#'
#' model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- cfa(model, data = HolzingerSwineford1939)
#'
#' semPaths2(fit)
#' semPaths2(fit, sig = FALSE)
#' }
semPaths2 = function(lav_obj, whatLabels = "est",
                     rotation = 2, nDigits = 2, residScale = 18,
                     mar = c(2, 8, 3, 8),
                     edge.label.cex = 1,
                     edge.label.position = 0.5,
                     sizeMan = 6,
                     sizeMan2 = 4,
                     sizeLat = 8,
                     sizeLat2 = 6,
                     border.width = 2,
                     edge.width = 1.5,
                     style = "lisrel",
                     sig = TRUE,
                     asize = 1.5,
                     shapeLat = "circle",
                     residuals = FALSE,
                     curve = 1,
                     layout = "tree",
                     exoVar = TRUE,
                     exoCov = TRUE,
                     structual = FALSE,
                     nodeLabels = NULL,
                     groups = FALSE) {

  if (groups) {
    if (is.null(nodeLabels)) {
      dia <- lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol = "black",
        nCharNodes = 10, layout = layout, edge.color = "black",
        rotation = rotation, edge.label.cex = edge.label.cex,
        edge.label.position = edge.label.position, residuals = residuals,
        residScale = residScale, exoVar = exoVar, exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat, sizeLat2 = sizeLat2,
        shapeLat = shapeLat, border.width = border.width,
        groups = "lat", pastel = TRUE, edge.width = edge.width,
        curve = curve, asize = asize, nDigits = nDigits,
        structual = structual, style = style, mar = mar)
    } else {
      dia <- lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol = "black",
        nCharNodes = 10, layout = layout, edge.color = "black",
        rotation = rotation, edge.label.cex = edge.label.cex,
        edge.label.position = edge.label.position, residuals = residuals,
        residScale = residScale, exoVar = exoVar, exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat, sizeLat2 = sizeLat2,
        shapeLat = shapeLat, border.width = border.width,
        groups = "lat", pastel = TRUE, edge.width = edge.width,
        curve = curve, asize = asize, nDigits = nDigits,
        structual = structual, nodeLabels = nodeLabels,
        style = style, mar = mar)
    }
  } else {
    if (is.null(nodeLabels)) {
      dia <- lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol = "black",
        nCharNodes = 10, layout = layout, edge.color = "black",
        rotation = rotation, edge.label.cex = edge.label.cex,
        edge.label.position = edge.label.position, residuals = residuals,
        residScale = residScale, exoVar = exoVar, exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat, sizeLat2 = sizeLat2,
        shapeLat = shapeLat, border.width = border.width,
        edge.width = edge.width, curve = curve, asize = asize,
        nDigits = nDigits, structual = structual,
        style = style, mar = mar)
    } else {
      dia <- lav_obj %>% semPlot::semPaths(
        whatLabels = whatLabels, fade = FALSE, posCol = "black",
        nCharNodes = 10, layout = layout, edge.color = "black",
        rotation = rotation, edge.label.cex = edge.label.cex,
        edge.label.position = edge.label.position, residuals = residuals,
        residScale = residScale, exoVar = exoVar, exoCov = exoCov,
        sizeMan = sizeMan, sizeMan2 = sizeMan2,
        sizeLat = sizeLat, sizeLat2 = sizeLat2,
        shapeLat = shapeLat, border.width = border.width,
        edge.width = edge.width, curve = curve, asize = asize,
        nDigits = nDigits, structual = structual,
        nodeLabels = nodeLabels, style = style, mar = mar)
    }
  }

  if (sig) {
    res <- dia %>%
      semptools::mark_sig(lav_obj) %>%
      plot()
  } else {
    res <- dia
  }

  return(res)
}



# semPaths2 = function(lav_obj, whatLabels = "est",
#                     rotation = 2, nDigits=2,  residScale=18,
#                     mar=c(2,8,3,8),
#                     edge.label.cex= 1,
#                     edge.label.position =0.5,
#                     sizeMan = 6,
#                     sizeMan2 = 4,
#                     sizeLat = 8,
#                     sizeLat2 = 6,
#                     border.width = 2,
#                     edge.width = 1.5,
#                     style="lisrel",
#                     sig=TRUE,
#                     asize= 1.5,
#                     shapeLat ="circle",
#                     residuals=FALSE,
#                     curve=1,
#                     layout = "tree",
#                     exoVar=TRUE,
#                     exoCov=TRUE,
#                     structual=FALSE,
#                     nodeLabels=NULL,
#                     groups=FALSE
# ){
#
#   if(groups){
#     if(is.null(nodeLabels)){
#       dia =   lav_obj %>% semPlot::semPaths(
#         whatLabels = whatLabels, fade = FALSE, posCol="black",
#         nCharNodes = 10,layout = layout,
#         edge.color="black",
#         rotation = rotation,
#         edge.label.cex =edge.label.cex,
#         edge.label.position = edge.label.position,
#         residuals= residuals,
#         residScale=  residScale,
#         exoVar=exoVar,
#         exoCov=exoCov,
#         sizeMan = sizeMan, sizeMan2 = sizeMan2,
#         sizeLat = sizeLat, sizeLat2 = sizeLat2,
#         shapeLat =shapeLat,
#         border.width = border.width,
#         groups ="lat",pastel = TRUE,
#         edge.width = edge.width,
#         curve = curve,
#         asize= asize,
#         nDigits = nDigits,
#         structual=structual,
#         # nodeLabels = nodeLabels,
#         style =  style,  mar=mar)
#
#     }else{
#       dia = lav_obj %>% semPlot::semPaths(
#         whatLabels = whatLabels, fade = FALSE, posCol="black",
#         nCharNodes = 10,layout = layout,
#         edge.color="black",
#         rotation = rotation,
#         edge.label.cex =edge.label.cex,
#         edge.label.position = edge.label.position,
#         residuals= residuals,
#         residScale=  residScale,
#         exoVar=exoVar,
#         exoCov=exoCov,
#         sizeMan = sizeMan, sizeMan2 = sizeMan2,
#         sizeLat = sizeLat, sizeLat2 = sizeLat2,
#         shapeLat =shapeLat,
#         border.width = border.width,
#         groups ="lat",pastel = TRUE,
#         edge.width = edge.width,
#         curve = curve,
#         asize= asize,
#         nDigits = nDigits,
#         structual=structual,
#         nodeLabels = nodeLabels,
#         style =  style,  mar=mar)
#     }
#
#
#   }else{
#     if(is.null(nodeLabels)){
#       dia =   lav_obj %>% semPlot::semPaths(
#         whatLabels = whatLabels, fade = FALSE, posCol="black",
#         nCharNodes = 10,layout = layout,
#         edge.color="black",
#         rotation = rotation,
#         edge.label.cex = edge.label.cex,
#         edge.label.position = edge.label.position,
#         residuals = residuals,
#         residScale =  residScale,
#         exoVar=exoVar, exoCov=exoCov,
#         sizeMan = sizeMan, sizeMan2 = sizeMan2,
#         sizeLat = sizeLat, sizeLat2 = sizeLat2,
#         shapeLat=shapeLat,
#         border.width = border.width,
#         edge.width = edge.width,
#         curve=curve,
#         nDigits = nDigits,
#         asize= asize,
#         structual=structual,
#         # nodeLabels = nodeLabels,
#         style =  style,  mar=mar)
#     }else{
#       dia =   lav_obj %>% semPlot::semPaths(
#         whatLabels = whatLabels, fade = FALSE, posCol="black",
#         nCharNodes = 10,
#         layout = layout,
#         edge.color = "black",
#         rotation = rotation,
#         edge.label.cex = edge.label.cex,
#         edge.label.position = edge.label.position,
#         residuals = residuals,
#         residScale =  residScale,
#         exoVar = exoVar, exoCov=exoCov,
#         sizeMan = sizeMan, sizeMan2 = sizeMan2,
#         sizeLat = sizeLat, sizeLat2 = sizeLat2,
#         shapeLat=shapeLat,
#         border.width = border.width,
#         edge.width = edge.width,
#         curve = curve,
#         nDigits = nDigits,
#         asize = asize,
#         structual = structual,
#         nodeLabels = nodeLabels,
#         style =  style,  mar=mar)
#     }
#
#
#   }
#
#
#   if(sig){
#     res= dia%>%
#       semptools::mark_sig(lav_obj) %>%plot()
#   }else{
#     res = dia
#   }
#
#   res
# }
