#' plspm의 구조모형의 유의성 자료를 넣는 함수
#'
#' @param plsdata plsdata
#' @param digits round 3
#' @param unite data combine unite TRUE -> estimat + sig
#' @param type vec, df
#' @return estimates vectors
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
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
#' plspm_edges_values(satpls)
#' plspm_edges_values(satpls, unite=F) #no star
#' plspm_edges_values(satpls$inner_model)
#' plspm_edges_values(satpls$inner_model, unite=FALSE) #No star
#' #' #'
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
#' #  Specify a new value for the edgeLabels parameter when calling qgraph
#' qgraph(qgraph(t(satpls$path_coefs)),
#'        layout = lay_p,
#'        posCol = "black",
#'        fade = FALSE,
#'        edge.labels = plspm_edges_values(satpls),
#'        edge.label.cex = 1,
#'        edge.label.position = 0.4,
#'        groups = NULL,
#'        palette = 'pastel'
#' )
#' # Specify a new value for the edgeLabels parameter when calling qgraph
#' qgraph(qgraph(t(satpls$path_coefs)),
#'        layout = "spring",
#'        posCol = "black",
#'        fade = FALSE,
#'        edge.labels = plspm_edges_values(satpls),
#'        edge.label.cex = 1,
#'        edge.label.position = 0.4,
#'        groups = NULL,
#'        palette = 'pastel'
#' )
#'
#'  Specify a new value for the edgeLabels parameter when calling qgraph
#' qgraph(qgraph(t(satpls$path_coefs)),
#'        layout = lay_p,
#'        posCol = "black",
#'        fade = FALSE,
#'        edge.labels = matrix(sprintf("%.4f", t(satpls$path_coefs)),
#'                             nrow=nrow(t(satpls$path_coefs)),
#'                             ncol=ncol(t(satpls$path_coefs))),
#'        edge.label.cex = 1,
#'        edge.label.position=0.4,
#'        groups = NULL, palette='pastel' )
#'
#'  plspm_edges_values(satpls)
#' satpls %>% plspm_path_coefs_plot()
#' satpls %>% plspm_path_coefs_plot(node =TRUE)
#'
#'
#' }
plspm_edges_values = function(plsdata, digits=3, unite=TRUE, type="df"){
  if(length(plsdata)==13){
    plsdf = plsdata$inner_model
  }else{
    plsdf = plsdata
  }

  plsdf = lapply(plsdf, function(x) round(x, digits) )
  # paths coefficients
  new_edge_labels <- plsdf %>%
    plspm_paths_sig(unite=unite) %>%
    dplyr::select(Est) %>%
    unlist() %>%
    as.character()

  new_edge_labels2 <- plsdf %>%
    plspm_paths_sig(unite=unite) %>%
    dplyr::select(Path, Est) %>%
    rename(relationships= Path,
           coeff= Est)

  switch(type,
         vec=new_edge_labels,
         vector=new_edge_labels,
         data.frame=new_edge_labels2,
         df=new_edge_labels2)

}


#' Functions to include significance data for structural models in PLSPM
#'
#' @param plsdata plsdata
#' @param digits round 3
#' @param unite data combine unite TRUE -> estimat + sig
#' @param type vec, df
#' @return estimates vectors
#' @export
#'
plspm_edge_values <- function(plsdata, digits = 3, unite = TRUE, type = "vec") {

  if (length(plsdata) == 13) {
    plsdf <- plsdata$inner_model
  } else {
    plsdf <- plsdata
  }

  # Apply round only to numeric columns
  # plsdf <- plsdf %>%round2(digits)
  #  dplyr::mutate(across(where(is.numeric), ~ round(.x, digits)))

  # Paths coefficients
  new_edge_labels <- plsdf %>%
    plspm_inner_model_sig(unite = unite) %>%
    dplyr::select(Est) %>%
    unlist() %>%
    as.character()

  new_edge_labels2 <- plsdf %>%
    plspm_inner_model_sig(unite = unite) %>%
    dplyr::select(Path, Est) %>%
    rename(relationships = Path,
           coeff = Est)

  switch(type,
         vec = new_edge_labels,
         vector = new_edge_labels,
         data.frame = new_edge_labels2,
         df = new_edge_labels2)

}




#' plspm의 구조모형의 유의성 자료를 넣는 함수
#'
#' @param plsdata plsdata
#' @param digits round 3
#' @param unite data combine unite TRUE -> estimat + sig
#' @param type vec, df
#' @return estimates vectors
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
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
#' plspm_edges_values(satpls)
#' plspm_edges_values(satpls, unite=F) #no star
#' plspm_edges_values(satpls$inner_model)
#' plspm_edges_values(satpls$inner_model, unite=FALSE) #No star
#' #' #'
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
#' #  Specify a new value for the edgeLabels parameter when calling qgraph
#' qgraph(qgraph(t(satpls$path_coefs)),
#'        layout = lay_p,
#'        posCol = "black",
#'        fade = FALSE,
#'        edge.labels = plspm_edges_values(satpls),
#'        edge.label.cex = 1,
#'        edge.label.position = 0.4,
#'        groups = NULL,
#'        palette = 'pastel'
#' )
#' # Specify a new value for the edgeLabels parameter when calling qgraph
#' qgraph(qgraph(t(satpls$path_coefs)),
#'        layout = "spring",
#'        posCol = "black",
#'        fade = FALSE,
#'        edge.labels = plspm_edges_values(satpls),
#'        edge.label.cex = 1,
#'        edge.label.position = 0.4,
#'        groups = NULL,
#'        palette = 'pastel'
#' )
#'
#'  Specify a new value for the edgeLabels parameter when calling qgraph
#' qgraph(qgraph(t(satpls$path_coefs)),
#'        layout = lay_p,
#'        posCol = "black",
#'        fade = FALSE,
#'        edge.labels = matrix(sprintf("%.4f", t(satpls$path_coefs)),
#'                             nrow=nrow(t(satpls$path_coefs)),
#'                             ncol=ncol(t(satpls$path_coefs))),
#'        edge.label.cex = 1,
#'        edge.label.position=0.4,
#'        groups = NULL, palette='pastel' )
#'
#'  plspm_edges_values(satpls)
#' satpls %>% plspm_path_coefs_plot()
#' satpls %>% plspm_path_coefs_plot(node =TRUE)
#'
#'
#' }
plspm_edges_coefs = function(plsdata, digits=3, unite=TRUE, type="df"){
  if(length(plsdata)==13){
    plsdf = plsdata$inner_model
  }else{
    plsdf = plsdata
  }

  plsdf = lapply(plsdf, function(x) round(x, digits) )
  # paths coefficients
  new_edge_labels <- plsdf %>%
    plspm_paths_sig(unite=unite) %>%
    dplyr::select(Est) %>%
    unlist() %>%
    as.character()

  new_edge_labels2 <- plsdf %>%
    plspm_paths_sig(unite=unite) %>%
    dplyr::select(Path, Est) %>%
    rename(relationships= Path,
           coeff= Est)

  switch(type,
         vec=new_edge_labels,
         vector=new_edge_labels,
         data.frame=new_edge_labels2,
         df=new_edge_labels2)

}
