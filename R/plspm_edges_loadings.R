
#' plspm lodaings significant  vecgtor or df
#'
#' @param plsres plspm result
#' @param type df, vec, data.frame, vector
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
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
#'
#'
#'
#' # plspm_loadings_values
#' satpls %>%plspm_edge_loadings()
#' satpls %>%plspm_edge_loadings("vec")
#' satpls$boot %>%plspm_edge_loadings()
#' satpls %>%plspm_edge_loadings()
#' satpls$boot$loadings%>%plspm_edge_loadings()
#' satpls$boot$loadings%>%plspm_edge_loadings("vec")
#'
#' }
plspm_edges_loadings = function(plsres, type="df"){
  if(length(plsres)==13){
    plsdf = plsres$boot$loadings
  }else if(length(plsres)==5){

    if(is.data.frame(plsres)){
      plsdf = plsres
    }else if(is.list(plsres)){
      plsdf = plsres$loadings
    }
  }

  res = plsdf%>%
    row2col("relationships")%>%
    add_t_sig("Mean.Boot", "Std.Error", ns="")%>%
    round2(3)%>%
    Unite("Original", "sig")%>%
    dplyr::select(1,2)%>%
    rename(coeff=Original)
  res1= res
  res2= res %>% pull(coeff)

  switch(type,
         df = res1,
         data.frame = res1,
         vector = res2,
         vec= res2 )

}




#' plspm lodaings significant  vecgtor or df
#'
#' @param plsres plspm result
#' @param type df, vec, data.frame, vector
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
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
#'
#'
#'
#' # plspm_loadings_values
#' satpls %>%plspm_loadings_values()
#' satpls %>%plspm_loadings_values("vec")
#' satpls$boot %>%plspm_loadings_values()
#' satpls %>%plspm_loadings_values()
#' satpls$boot$loadings%>%plspm_loadings_values()
#' satpls$boot$loadings%>%plspm_loadings_values("vec")
#'
#' }
plspm_loadings_values = function(plsres, type="df"){
  if(length(plsres)==13){
    plsdf = plsres$boot$loadings
  }else if(length(plsres)==5){

    if(is.data.frame(plsres)){
      plsdf = plsres
    }else if(is.list(plsres)){
      plsdf = plsres$loadings
    }
  }

  res = plsdf%>%
    row2col("relationships")%>%
    add_t_sig("Mean.Boot", "Std.Error", ns="")%>%
    round2(3)%>%
    Unite("Original", "sig")%>%
    dplyr::select(1,2)%>%
    rename(coeff=Original)
  res1= res
  res2= res %>% pull(coeff)

  switch(type,
         df = res1,
         data.frame = res1,
         vector = res2,
         vec= res2 )

}
