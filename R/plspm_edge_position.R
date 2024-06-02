#' plspm_edge_position
#'
#' @param semPaths_plot plot data
#' @param ...  edge v1 v2 position
#' @param plot  TRUE
#'
#'
#' @return graph
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' pathmodel = "
#' 자기효능감 =~ C_S1 +  C_S2 + C_S3 + C_S4 + C_S5
#' 진로태도 =~ B02 + B04 + B10 + B14
#' 진로동기 =~ A_M1 + A_M2 + A_M3
#' 진로준비 =~ D_P1 + D_P2 + D_P3
#'
#' 진로동기 ~ H1*자기효능감
#' 진로태도 ~  H2*자기효능감
#' 진로준비 ~ H5 *진로태도
#' 진로준비 ~  H4 * 진로동기
#' 진로준비 ~  H3*자기효능감
#' "
#'
#' plspm_diagram_model(pathmodel, layout="tree2",
#'                     rotation = 2,residuals = T, residScale = 6,
#'                     edge.label.position = 0.6)%>%
#'   plspm_rotate_resid(c(진로동기= 180, 진로태도= 30))%>%
#'   plspm_curve(c("진로준비~~ 진로동기" = -2))%>%
#'   plspm_edge_position("진로태도","진로준비", 0.4,
#'                       "자기효능감","진로동기", 0.3        )
#' #'
#' }
#'
#'
plspm_edge_position <- function(semPaths_plot, ..., plot=TRUE){

  # Convert ... to a list of arguments
  args <- list(...)

  # Ensure the number of arguments is a multiple of 3 (from, to, new_position)
  if (length(args) %% 3 != 0) {
    stop("Arguments should be provided in triples: from, to, new_position")
  }

  # Create a list of edge changes
  newPosition <- lapply(seq(1, length(args), by = 3), function(i) {
    list(from = args[[i]], to = args[[i + 1]], new_position = args[[i + 2]])
  })

  # Apply the new edge positions
  res <- semptools::set_edge_label_position(semPaths_plot, newPosition)

  if(plot){

  plot(res)
  }
  return(res)
}
