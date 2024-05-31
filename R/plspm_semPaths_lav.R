#' Generate lavaan syntax from a model or plspm result
#'
#' @param plsres_boot plspm result
#' @param type lav, ,model, edge, edgeLabels
#' @param paths paths matrix
#' @param blocks blocks tree
#'
#' @return lavaan syntax
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(jjstat)
#' data(satisfaction)
#' sat_path = plspm_paths(
#'   row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
#'   relationship = list(
#'     path(from="IMAG", to=c("EXPE","SAT","LOY")),
#'     path("EXPE", c("QUAL","VAL","SAT")),
#'     path("QUAL", c("VAL","SAT")),
#'     path("VAL",c("SAT")),
#'     path("SAT","LOY"))
#' )
#' sat_path
#'
#' # blokcs
#' sat_blocks1 <- plspm_blocks(
#'   IMAG = item(paste0("imag",1:5)),
#'   EXPE = item(paste0("expe", 1:5)),
#'   QUAL = item( paste0("qual", 1:5)),
#'   VAL = item(paste0("val", 1:4)),
#'   SAT = item(paste0("sat", 1:4)),
#'   LOY = item(paste0("loy", 1:4))
#' )
#'
#' sat_blocks1
#'
#'
#' ###lav
#' satpls_boot%>%
#'   plspm_semPaths_lav(paths=sat_path, blocks=sat_blocks1)%>%cat()
#'
#' ## model
#' satpls_boot%>%plspm_semPaths_lav()
#'
#' ## edge
#' satpls_boot%>%plspm_semPaths_lav("edge")
#'
#' ## draw model
#' satpls_boot%>%
#'   plspm_semPaths_lav(paths=sat_path, blocks=sat_blocks1)%>%
#'   diagram_model(  edgeLabels= satpls_boot%>%plspm_semPaths_lav("edge"),
#'                   mar=c(2,2,2,2),
#'                   sizeLat=4, sizeMan=3, sizeMan2= 2, edge.label.cex=0.5, rotation=1)
#'
#' ###
#' satpls_boot%>%plspm_semPaths_lav()%>%
#'   diagram_model(edgeLabels=satpls_boot%>%plspm_semPaths_lav("edge"),
#'                 mar=c(2,2,2,2),edge.label.cex=0.5, edge.label.position= 0.7,
#'                 sizeLat=4, sizeMan=3, sizeMan2= 2, rotation=1)
#'
#' }
#'
#'
plspm_semPaths_lav= function(plsres_boot, type="lav", paths=NULL,blocks=NULL){

  if(!is.null(paths) && !is.null(blocks)){
    mms1 = plspm_blocks2lav(blocks)
    sms1 = plspm_paths2lav(paths)
    model = paste(mms1, sms1, sep="; ")

  }else{
    mms1 = summary(plsres_boot)$outer_model%>%
      dplyr::select(name, block, loading) %>% #Round(3)%>%
      dplyr::mutate(item = paste0(block," =~ ", name)) %>%
      dplyr::select(item) %>%
      unlist()%>%
      as.character() %>%
      paste( collapse="; ")

    sms1 = summary(plsres_boot)$effects %>%
      dplyr::select(relationships, direct)%>%
      dplyr::filter(direct != 0)%>%
      tidyr::separate(relationships, c("lhs","rhs"), sep="->")%>%
      dplyr::mutate(path = paste0(rhs," ~ ",lhs)) %>%
      dplyr::select(path) %>%
      unlist()%>%
      as.character() %>%
      paste( collapse="; ")

    model = paste(mms1, sms1, sep="; ")

  }
  #edge calculation
  edgeLabels = c(plspm_loadings_values(plsres_boot, type="vec"),
                 plspm_boot_paths_sig(plsres_boot, type="vec"))


  switch(type,
         lav = model,
         model=model,
         edgeLabels=edgeLabels,
         edge =  edgeLabels)
  # model
}
