#' plspm_change_nodeLabels
#'
#' @param data plspm_semPaths
#' @param ... change node from name , to name
#' @param plot TRUE
#'
#' @return plot
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
#' plspm_semPaths2(jutpls_boot,   curve=1, layout="tree2",rotation=1,
#'                 edge.label.cex=0.7, mar=c(2,1,2,1),
#'                 edge.label.position=0.6, label.cex=1,
#'                 sizeLat= 10)%>%
#'   plspm_change_nodeLabels("B02","결정성",
#'                           "B04", "독립성",
#'                           "B10", "계획성",
#'                           "B14", "성향"
#'   )
#'
#'
#'
#'
#'
#'
#'
#' }
#'
#'
plspm_change_nodeLabels <- function(data, ..., plot=TRUE) {
  # Convert an input of ... to a list
  args <- list(...)

  # Create a list of node changes by pairing lists
  namechange <- lapply(seq(1, length(args), by = 2), function(i) {
    list(node = args[[i]], to = args[[i + 1]])
  })

  # apply rename
  res <- semptools::change_node_label(data, namechange)

  # Plot the results
  if(plot){

  plot(res)
  }

  # Returning changed data to keep pipelines connected
  return(res)
}
