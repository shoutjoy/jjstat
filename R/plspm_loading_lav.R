#' plspm_loading to lavaan syntax
#'
#' @param input_data plspm result
#' @param col selected col
#'
#' @return lavaan syntax
#' @export
#'
#' @examples
#' \dontrun{
#' #'
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
#' sat_blocks1
#'
#' # vector of modes (reflective indicators):auto
#' sat_mod = rep("A", 6)
#' #############
#'
#' # apply plspm
#' satpls = plspm(satisfaction,
#'                path_matrix = sat_path,
#'                blocks = sat_blocks1,
#'                scaled = FALSE)
#' #lavaan 1
#' satpls%>%plspm_loading_lav() %>%cat("\n")
#'
#' #method 2, lavan
#' satpls$outer_model%>%plspm_loading_lav() %>%cat("\n")
#'
#' #diagram
#' x11()
#' satpls%>%plspm_loading_lav() %>%
#'   diagram_model(whatLabels = "est",exoCov=F,
#'                 edge.label.cex = 0.6, rotation=4,
#'                 sizeLat=6,
#'                 sizeMan = 6, sizeMan2=1.5, mar=c(1,10,1,10))

#' }
#'
#'
plspm_loading_lav <- function(input_data, col="loading") {
  # Check if the input is the satpls object or directly the outer_model data frame
  if (length(input_data)==13) {
    outer_model <- input_data[["outer_model"]]
  } else {
    outer_model <- input_data
  }

  # Create the lavaan syntax
  lavaan_syntax <- outer_model %>%
    mutate(exo = paste0(block, " =~ ", outer_model[[col]], "*", name)) %>%
    select(exo) %>%
    pull(exo) %>%
    paste(collapse = "\n")

  cat("\n seleted:",col,"\n\n")

  return(lavaan_syntax)
}
