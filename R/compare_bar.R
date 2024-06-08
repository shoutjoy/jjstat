
#' plspm_compare_bar
#'
#' @param data arragne data
#' @param x paths
#' @param y values
#' @param fill group
#' @param size_x textsize
#' @param angle xangle
#' @param position  dodge, stack
#' @param text  hide bartext
#' @param size  bargrahp text size
#' @param xlab  default ""
#' @param ylab  values
#'
#' @return  graph
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 데이터
#' REs <- tibble::tribble(
#'   ~paths, ~grp, ~values,
#'   "자기효능감->진로동기", "group.남자", 0.902,
#'   "자기효능감->진로동기", "group.여자", 0.866,
#'   "자기효능감->진로태도", "group.남자", -0.639,
#'   "자기효능감->진로태도", "group.여자", -0.672,
#'   "자기효능감->진로준비", "group.남자", 0.149,
#'   "자기효능감->진로준비", "group.여자", 0.487,
#'   "진로동기->진로준비", "group.남자", 0.63,
#'   "진로동기->진로준비", "group.여자", 0.432,
#'   "진로태도->진로준비", "group.남자", -0.153,
#'   "진로태도->진로준비", "group.여자", 0.059
#' )
#'
#' # # 함수 호출 예시
#' REs %>% compare_bar()
#' REs %>% compare_bar(text=FALSE)
#'
#'
#'
#'
#' }
compare_bar <- function(data, x = NULL, y = NULL, fill = NULL, size = 3,
                        size_x = 12, angle = 90, position = "dodge",
                        xlab = "", ylab = "Values", text = TRUE, digits = 3) {
  Data <- data

  if (is.null(x) && is.null(y) && is.null(fill)) {
    x <- sym(names(Data)[1])
    y <- sym(names(Data)[3])
    fill <- sym(names(Data)[2])
  } else {
    x <- enquo(x)
    y <- enquo(y)
    fill <- enquo(fill)
  }

  gg <- Data %>% ggplot(aes(x = !!x, y = !!y, fill = !!fill)) +
    geom_bar(stat = "identity", position = position) +
    theme_bw() +
    scale_fill_grey(start = 0.3, end = 0.7) +
    theme(axis.text = element_text(size = size_x, angle = angle, face = "bold")) +
    labs(x = xlab, y = ylab)

  if (text) {
    gg <- gg +
      geom_text(aes(label = format(round(!!y, digits), digits = digits)),
                vjust = ifelse(Data[[quo_name(y)]] > 0, -0.5, 1.5),
                position = position_dodge(0.9),
                size = size)
  }

  return(gg)
}
