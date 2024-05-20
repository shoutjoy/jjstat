
#' inner_plot(Pre-draw a mockup )
#'
#' @param path_mat mati
#' @param layout spring or layout
#' @param posCol black
#' @param rotation 1
#' @param negCol red
#' @param fade false
#' @param edge.labels FALSE
#' @param edge.label.cex  1.2
#' @param edge.label.position 0.5
#' @param edge.width 2
#' @param vsize 8
#' @param asize 4
#' @param borders TRUE
#' @param border.width 2
#' @param border.color gray40
#' @param labels NULL
#' @param label.cex 1.2
#' @param label.color gray30
#' @param groups NULL
#' @param palette pastel
#' @param curve 0.2
#' @param width 1
#'
#' @return graph
#' @export
#'
#' @examples
#'
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
#' sat_path%>%inner_plot()
#' #'
#' path1 = plspm_path(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY")
#'   )
#' )
#' path1
#' path1%>%inner_plot()
#'
#' #'
#' model1 = "
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL+ EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#' # Generate the matrix
#' lavpath = plspm_lav2path(model1)
#' lavpath%>%inner_plot()
#' #'
#' #'
#' }
#'
#'
plspm_innerplot <- function(path_mat,
                       layout = "spring", posCol = "black", rotation=1,
                       negCol = "red",     fade = FALSE,
                       edge.labels = FALSE,     edge.label.cex = 1.2,
                       edge.label.position = 0.5,    edge.width=2,
                       vsize = 8,     asize = 4,     borders = TRUE,
                       border.width = 2, border.color = "gray40",
                       labels = NULL,  label.cex = 1.2,
                       label.color = "gray30", groups = NULL,
                       palette = "pastel", curve = 0.2,  width = 1
) {

  library(qgraph)

  # labels <- colnames(path_mat)
  labels <- rownames(path_mat)

  qgraph(
    t(path_mat),  # Transpose the matrix to match the expected format
    layout = layout,
    posCol = posCol,
    negCol = negCol,
    fade = fade,
    edge.labels = edge.labels,
    edge.label.cex = edge.label.cex,
    edge.label.position = edge.label.position,
    edge.width= edge.width,
    vsize = vsize,
    asize = asize,
    borders = borders,
    border.width = border.width,
    border.color = border.color,
    labels = labels,
    label.cex = label.cex,
    label.color = label.color,
    groups = groups,
    palette = palette,
    curve = curve,
    width = width
  )
}
