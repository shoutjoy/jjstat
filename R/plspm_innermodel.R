#' Draw a Model of a Measured PLSPM Structure
#'
#' @param data plspm data
#' @param digits 3
#' @param layout default spring
#' @param fade FALSE
#' @param border.width 1.5
#' @param border.color  gray30
#' @param edge.label.cex  1
#' @param edge.label.position 0.5
#' @param asize 3
#' @param posCol gray20
#' @param negCol red
#' @param shape cicle
#' @param groups NULL
#' @param pastel pastel
#' @param trans true
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # path matrix
#' IMAG = c(0,0,0,0,0,0)
#' EXPE = c(1,0,0,0,0,0)
#' QUAL = c(0,1,0,0,0,0)
#' VAL = c(0,1,1,0,0,0)
#' SAT = c(1,1,1,1,0,0)
#' LOY = c(1,0,0,0,1,0)
#' sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)
#'
#' # blocks of outer model
#' sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
#' # apply plspm
#' satpls = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod,
#'                scaled = FALSE)
#'
#' #default
#' satpls %>% innermodel_plspm()
#'
#'
#'
#' lay_p = matrix(c('IMA', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'EXP', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'QUA', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'VAL', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'SAT', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'LOY', NA, NA, NA, NA, NA, NA, NA, NA),
#'                nrow = 10, ncol = 10, byrow = FALSE)
#'
#' satpls %>% innermodel_plspm(layout=lay_p, edge.label.position = 0.6,
#'                             border.color="gray99", groups=1:6)
#'
#' #'
#' }
innermodel_plspm <- function(data,
                             digits = 3,
                             layout = "spring",
                             fade = FALSE,
                             border.width=1.5,
                             border.color="gray30",
                             edge.label.cex = 1,
                             edge.label.position = 0.5,
                             asize=3,
                             posCol="gray20",
                             negCol = "red",
                             shape="circle",
                             groups = NULL, pastel= 'pastel',
                             trans=TRUE) {
  # Check if data inherits from 'plspm'
  if (inherits(data, "plspm")) {
    data <- data[["path_coefs"]]
  }

  # Generate edge labels
  edge_labels <- matrix(sprintf(paste0("%.", digits, "f"), t(data)),
                        nrow = nrow(t(data)),
                        ncol = ncol(t(data)))

  if(trans){
    data = t(data)
  }else{
    data= data
  }
  # Plot using qgraph
  qgraph(data,
         layout = layout,
         posCol = posCol,
         negCol = negCol,
         fade = fade,
         border.width = border.width,
         border.color = border.color,
         shape = shape,
         edge.labels = edge_labels,
         asize = asize,
         edge.label.cex = edge.label.cex,
         edge.label.position = edge.label.position,
         groups = groups,
         palette = pastel)
}
