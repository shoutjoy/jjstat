#' Draw a Model of a Measured PLSPM Structure
#'
#' @param data plspm data
#' @param digits 3
#' @param layout default spring
#' @param fade FALSE
#' @param node nodeNames legeng when node =TRUE
#' @param border.width 1.5
#' @param border.color  gray30
#' @param edge.label.cex  1
#' @param edge.label.position 0.5
#' @param edge_labels_sig Tedge_labels_sig= TRUE add sig star
#' @param asize 3 arrow size
#' @param vsize 8 nodesixe
#' @param esize 10 edgesize
#' @param posCol gray20
#' @param negCol red
#' @param shape cicle
#' @param groups NULL
#' @param grp groups on T
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
#' satpls %>% plspm_path_coefs_plot()
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
#' satpls %>% plspm_path_coefs_plot(layout=lay_p, edge.label.position = 0.6,
#'                             border.color="gray99", groups=1:6)
#'
#'
#' # various type
#' satpls %>% plspm_path_coefs_plot()
#' satpls %>% plspm_path_coefs_plot(node =TRUE)
#'
#' satpls %>% plspm_path_coefs_plot(layout=lay_p, edge.label.position = 0.6,
#'                                  border.color="gray99", groups=1:6)
#' satpls %>% plspm_path_coefs_plot(layout=lay_p, edge.label.position = 0.6,
#'                                  border.color="gray99", groups=1:6, node=TRUE)
#'
#' satpls %>% plspm_path_coefs_plot(shape="square", layout=lay_p,
#'                                  edge.label.position = 0.6)
#'
#'
#'
#' satpls %>% plspm_path_coefs_plot()
#' satpls %>% plspm_path_coefs_plot(node =TRUE)
#' satpls %>% plspm_path_coefs_plot(grp=FALSE)
#' satpls %>% plspm_path_coefs_plot(node = FALSE,grp=TRUE)
#' satpls %>% plspm_path_coefs_plot(node = TRUE,grp=FALSE)
#' satpls %>% plspm_path_coefs_plot(node = FALSE,grp=FALSE)
#' satpls %>% plspm_path_coefs_plot(node = TRUE,grp=TRUE)
#' #'
#' }
plspm_path_coefs_plot <- function(plsdata,
                                  grp = TRUE,
                                  groups = NULL,
                                  digits = 3,
                                  layout = "spring",
                                  fade = FALSE,
                                  node = FALSE,
                                  border.width = 1.5,
                                  border.color = "gray90",
                                  edge.label.cex = 1,
                                  edge.label.position = 0.6,
                                  asize = 4,
                                  vsize = 8,#
                                  esize = 10,#
                                  edge_labels_sig = TRUE,
                                  posCol = "gray20",
                                  negCol = "red",
                                  shape = "circle",
                                  pastel = 'pastel',
                                  trans = TRUE) {
  # Check if data inherits from 'plspm'
  if(length(plsdata) == 13){
    pathdata =  plsdata$path_coefs
  }else{
    #Cannot get the innermodel value in this case.
    pathdata = plsdata
  }
  # Generate edge labels
  if(edge_labels_sig){
    if(length(plsdata) != 13){
      # If you entered path_coefs
      edge_labels <- matrix(sprintf(paste0("%.", digits, "f"), t(pathdata)),
                            nrow = nrow(t(pathdata)),
                            ncol = ncol(t(pathdata)))
      cat("\n\nTo indicate the significance of a path, enter the full PLSPM data \n\n")
    }else{
      # This value should get the inner_model value from the full value.
      edge_data <- plsdata$inner_model


      #significant values

      edge_labels = plspm_edge_values(edge_data)
      # Converting to a data conversion factor value
    }

  }else{
    edge_labels <- matrix(sprintf(paste0("%.", digits, "f"), t(pathdata)),
                          nrow = nrow(t(pathdata)),
                          ncol = ncol(t(pathdata)))
  }

  #matrix transpose
  if(trans){
    pathdata = t(pathdata)
  }else{
    pathdata= pathdata
  }

  # full nodename
  if(node){
    nodeNames = colnames(pathdata) %>%unlist()%>%as.character()
  }else{
    nodeNames = NULL
  }

  # groups
  if(grp){
    groups = colnames(pathdata)
  }

  # Plot using qgraph
  qgraph::qgraph(pathdata,
                 layout = layout,
                 posCol = posCol,
                 negCol = negCol,
                 fade = fade,
                 border.width = border.width,
                 border.color = border.color,
                 shape = shape,
                 edge.labels = edge_labels,
                 asize = asize,
                 vsize = vsize,
                 esize = esize,
                 nodeNames = nodeNames,
                 edge.label.cex = edge.label.cex,
                 edge.label.position = edge.label.position,
                 groups = groups,
                 palette = pastel)

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
