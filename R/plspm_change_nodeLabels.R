#' plspm_change_nodeLabels
#'
#' @param data plspm_semPaths
#' @param ... change node from name , to name
#' @param label.cex label.cex
#' @param label.scale label.scale
#' @param label.prop label.prop
#' @param label.norm label.norm
#' @param plot TRUE
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
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

plspm_change_nodeLabels <- function(data, ...,
                                    label.cex = 1.2,
                                    label.scale = TRUE,
                                    label.prop = 1,
                                    label.norm = "width",
                                    plot = TRUE) {

  # ...로 받은 인자를 리스트로 변환
  args <- list(...)

  # 입력된 첫 번째 인자가 벡터인지 확인 (벡터일 경우 2개씩 묶어 처리)
  if (length(args) == 1 && is.vector(args[[1]])) {
    args <- args[[1]]
  }

  # 인자가 짝수여야 함을 확인
  if (length(args) %% 2 != 0) {
    stop("짝이 맞지 않는 인자가 있습니다. 노드와 레이블 쌍을 입력해야 합니다.")
  }

  # 인자를 2개씩 짝지어 리스트로 변환
  namechange <- lapply(seq(1, length(args), by = 2), function(i) {
    list(node = args[[i]], to = args[[i + 1]])
  })

  # Apply rename
  res <- semptools::change_node_label(data, namechange,
                                      label.cex = label.cex,
                                      label.scale = label.scale,
                                      label.prop = label.prop,
                                      label.norm = label.norm)

  # Plot the results
  if (plot) {
    plot(res)
  }

  # Returning changed data to keep pipelines connected
  return(res)
}
