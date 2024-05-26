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
#' @param edge_labels_sig Tedge_labels_sig= TRUE add sig star
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
#'
#' sat_mod = c(rep("A", 6))
#' # apply plspm
#' satpls = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod,
#'                scaled = FALSE)
#'
#'
#'  ##jjstat
#' data(satisfaction)
#' #model 1
#' model1 = "
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL+ EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#' # # Generate the matrix
#' sat_path = plspm_lav2path(model1,
#'                           fct_order=c("IMAG", "EXPE","QUAL", "VAL", "SAT", "LOY" ))
#' # sat_path
#'
#' # Method 2: paths
#' sat_path = plspm_paths(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY")
#'   )
#' )
#' # blokcs
#' sat_blocks1 <- plspm_blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#' sat_blocks1
#'
#' # vector of modes (reflective indicators):auto
#' sat_mod = rep("A", 6)
#' #############
#'
#' # apply plspm
#' satpls = plspm_sem(satisfaction,
#'                    path_matrix = sat_path,
#'                    blocks = sat_blocks1,
#'                    scaled = FALSE)
#'
#' satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1, scaled = FALSE,
#'                         boot.val =TRUE, br=100)
#'
#' #default
#' satpls %>% plspm_innermodel_plot()
#'
#'
#'
#' lay_p = matrix(c('IMA', NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#'  'EXP', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, 'QUA', NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, 'VAL', NA, NA, NA, NA,
#'  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'SAT',
#'  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#'  NA, NA, NA, NA, 'LOY', NA, NA, NA, NA, NA, NA, NA, NA),
#'  nrow = 10, ncol = 10, byrow = FALSE)
#'
#' satpls %>% plspm_innermodel_plot(layout=lay_p, edge.label.position = 0.6,
#'                             border.color="gray99", groups=1:6)
#'
#'
#' # various type
#' satpls %>% plspm_innermodel_plot()
#'
#' satpls %>% plspm_innermodel_plot(layout=lay_p, edge.label.position = 0.6,
#'                                  border.color="gray99", groups=1:6)
#'
#' satpls %>% plspm_innermodel_plot(shape="square", layout=lay_p,
#'                                  edge.label.position = 0.6)
#'
#' #'
#' }
plspm_innermodel_plot <- function(plsdata,
                                  digits = 3,
                                  layout = "spring",
                                  fade = FALSE,
                                  border.width=1.5,
                                  border.color="gray30",
                                  edge.label.cex = 1,
                                  edge.label.position = 0.5,
                                  asize=3,
                                  edge_labels_sig= TRUE,
                                  posCol="gray20",
                                  negCol = "red",
                                  shape="circle",
                                  groups = NULL, pastel= 'pastel',
                                  trans=TRUE) {
  # Check if data inherits from 'plspm'
  data = plsdata

  if (inherits(data, "plspm")) {
    data <- data[["path_coefs"]]


  }

  # Generate edge labels
  if(edge_labels_sig){
    edge_data <- plsdata[["inner_model"]]
    edge_labels = plspm_edge_values(edge_data)
  }else{
    edge_labels <- matrix(sprintf(paste0("%.", digits, "f"), t(data)),
                          nrow = nrow(t(data)),
                          ncol = ncol(t(data)))
  }

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
  # edge_labels
}





#
# plspm_edge_values = function(plsdata, digits=3, unite=TRUE){
#   if(length(plsdata)==13){
#     plsdf = plsdata$inner_model
#   }else{
#     plsdf = plsdata
#   }
#
#   plsdf = lapply(plsdf, function(x) round(x, digits) )
#   # paths coefficients
#   new_edge_labels <- plsdf %>%
#     plspm_paths_sig(unite=unite) %>%
#     dplyr::select(Est) %>%
#     unlist() %>%
#     as.character()
#   return(new_edge_labels)
# }
