
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
#' @param mar  c(3,3,3,3)
#' @param edge_size edge.width size control
#' @param hypo paste H
#' @param angle rotation angle
#' @param digits  3#'
#'
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
#' sat_path%>%plspm_innerplot()
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
#' path1%>%plspm_innerplot()
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
#'
#'
#' lavpath%>%plspm_innerplot()
#'
#' #color
#' sat_path %>%plspm_innerplot(groups=1:6)
#' sat_path %>%plspm_innerplot(groups=1:6, hypo= FALSE ) #putput 1
#'
#'
#' sat_path %>%
#'  plspm_innerplot(groups=1:6, rotation = 3 )
#'
#'
#' lay_p = matrix(c('IMA', NA, NA, NA, NA, NA, NA, NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'EXP', NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'QUA',
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, 'VAL', NA, NA, NA, NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'SAT', NA, NA, NA,
#'                  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'LOY',
#'                  NA, NA, NA, NA, NA, NA, NA, NA),
#'                nrow = 10, ncol = 10, byrow = FALSE)
#' lay_p
#' #'
#' #'
#' }
#'
#'
plspm_innerplot <- function(path_mat,
                            edge.label.cex = 2,
                            edge.label.position = 0.5,
                            edge.labels = TRUE,
                            edge.width = 3,
                            label.cex = 1.1,
                            rotation = 0,  # 회전 각도(도)
                            angle=90,
                            sizeLat = 14,
                            groups = NULL,
                            layout = "circle",
                            negCol = "red",
                            fade = FALSE,
                            asize = 5,
                            borders = TRUE,
                            border.width = 4,
                            border.color = "gray10",
                            labels = NULL,
                            label.color = "darkblue",
                            posCol = "gray10",
                            palette = "pastel",
                            curve = 0.2,
                            width = 2,
                            mar = c(3, 3, 3, 3),
                            edge_size = 0.7,
                            hypo = TRUE,  # Added options
                            digits = 3  # Added arguments
) {
  library(qgraph)

  if(length(path_mat) == 13){
    path_mat <- path_mat$path_coef
  }

  path_mat <- round(path_mat, digits = digits)

  if (is.null(labels)) {
    labels <- rownames(path_mat)


  }

  edge_labels <- NULL
  if (hypo && all(path_mat %in% c(0, 1))) {
    edge_labels <- matrix("", nrow = nrow(path_mat), ncol = ncol(path_mat))
    count <- 1
    for (i in 1:nrow(path_mat)) {
      for (j in 1:ncol(path_mat)) {
        if (path_mat[i, j] == 1) {
          edge_labels[i, j] <- paste0("H", count)
          count <- count + 1
        }
      }
    }
  }

  # qgraph를 사용하여 네트워크 생성
  net <- qgraph(
    t(path_mat),  # Transpose the matrix to match the expected format
    layout = layout,
    posCol = posCol,
    negCol = negCol,
    fade = fade,
    edge.labels = if (is.null(edge_labels)) edge.labels else edge_labels,
    edge.label.cex = edge.label.cex,
    edge.label.position = edge.label.position,
    edge.width = edge.width * edge_size,
    vsize = sizeLat,
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
    width = width,
    mar = mar,
    DoNotPlot = TRUE  # 레이아웃 좌표를 가져오기 위해 그래프를 그리지 않음
  )

  coords <- net$layout
  angle <- rotation * angle * pi / 180  # 회전 각도를 라디안으로 변환

  rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), ncol = 2)
  rotated_coords <- coords %*% rotation_matrix  # 회전 적용

  # 회전된 레이아웃으로 다시 그리기
  qgraph(
    t(path_mat),
    layout = rotated_coords,
    posCol = posCol,
    negCol = negCol,
    fade = fade,
    edge.labels = if (is.null(edge_labels)) edge.labels else edge_labels,
    edge.label.cex = edge.label.cex,
    edge.label.position = edge.label.position,
    edge.width = edge.width * edge_size,
    vsize = sizeLat,
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
    width = width,
    mar = mar
  )
}
