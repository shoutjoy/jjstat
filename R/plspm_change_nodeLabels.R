#' plspm_change_nodeLabels
#'
#' @param data plspm_semPaths
#' @param ... change node from name , to name
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
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
#' }
#'
#'
plspm_change_nodeLabels <- function(data, ...) {
  # 입력받은 ...을 리스트로 변환
  args <- list(...)

  # 리스트를 짝지어 노드 변경 리스트 생성
  namechange <- lapply(seq(1, length(args), by = 2), function(i) {
    list(node = args[[i]], to = args[[i + 1]])
  })

  # 이름 변경 적용 및 플롯 생성
  data %>%
    semptools::change_node_label(namechange) %>%
    plot()
}
