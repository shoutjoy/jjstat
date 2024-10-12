#' table_df_bar grap
#'
#' @param df table_df res
#' @param size.x size.x =14  axis text
#' @param add_range  graph ragne +150
#' @param size.text size text label 4.5
#' @param x.title x title
#' @param flip flip FALSE
#' @param hjust  hjust -0.1
#' @param vjust vjust -0.5
#' @param x11_width x11_width 10
#' @param x11_height x11_height 7
#' @param show_line show_line, default FALSE
#' @param sort FALSE (arrange graph )
#'
#' @return graph
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' table_df_multit(eduteck2 %>% select(contains("Q4_")))%>%
#'   table_df_bar(sort=TRUE)

#' }
#'
table_df_bar <- function(df, size.x = 20, size.text = 6, add_range = 100,
                         x.title = "Items",
                         flip = TRUE,
                         hjust = -0.1, vjust=-0.5,
                         x11_width=8, x11_height=5,
                         linetype="dashed", linecolor="gray50",
                         show_line = FALSE,  # 선을 보이게 할지 여부를 결정하는 인수 추가
                         sort = FALSE) {
  # 데이터 정렬: sort가 TRUE일 경우 Freq에 따라 정렬
  if (sort) {
    df <- df[order(df$Freq, decreasing = TRUE), ]
  }

  # 빈도(Freq)의 최대값에 add_range를 더하여 y 축 범위 설정
  max_value <- max(df$Freq, na.rm = TRUE) + add_range


  # flip이 TRUE일 경우 그래프를 가로로 회전
  if (flip) {
    # ggplot으로 막대 그래프 생성
    p <- ggplot(df, aes(x = reorder(term, Freq), y = Freq, fill = term)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Freq, " (", round(`prop(%)`, 2), "%)")),
                hjust = hjust, size = size.text) +
      labs(x = x.title, y = "빈도(Freq)") +
      ylim(0, max_value) +  # y 축 범위를 설정
      theme_bw() +
      theme(
        axis.text = element_text(size = size.x, face = "bold"),
        axis.title = element_text(size = size.x, face = "bold"),
        legend.position = "none")  # 범례를 숨김 (각 막대가 색상으로 구분되므로 범례가 불필요함)

    p <- p + coord_flip()

  } else {
    # 범주의 순서 설정
    df$term <- factor(df$term, levels = c("전혀 아니다", "아니다", "중간이다", "그렇다", "매우 그렇다"))

    # flip이 FALSE일 경우
    p <- ggplot(df, aes(x = term, y = Freq, group = 1, fill = term)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Freq, "\n (", round(`prop(%)`, 2), "%)")),
                vjust = vjust, size = size.text)

    # show_line이 TRUE일 경우에만 선을 그리도록 설정
    if (show_line) {
      p <- p + geom_line(aes(x = term, y = Freq), show.legend = FALSE,
                         linetype=linetype, color=linecolor, linewidth=0.8) +
        geom_point(aes(x = term, y = Freq), show.legend = FALSE)
    }

    p <- p + labs(x = x.title, y = "빈도(Freq)") +
      ylim(0, max_value) +  # y 축 범위를 설정
      theme_bw() +
      theme(
        axis.text = element_text(size = size.x, face = "bold"),
        axis.title = element_text(size = size.x, face = "bold"),
        legend.position = "none")  # 범례를 숨김 (각 막대가 색상으로 구분되므로 범례가 불필요함)
  }

  x11(width = x11_width, height = x11_height)
  # 그래프 출력
  print(p)
}
# table_df_bar <- function(df, size.x = 20, size.text = 6, add_range = 150,
#                          x.title = "Items",
#                          flip = TRUE,
#                          hjust = -0.1, vjust=-0.5,
#                          x11_width=8, x11_height=5,
#                          linetype="dashed", linecolor="gray50",
#                          show_line = FALSE,  # 선을 보이게 할지 여부를 결정하는 인수 추가
#                          sort = FALSE) {
#   # 데이터 정렬: sort가 TRUE일 경우 Freq에 따라 정렬
#   if (sort) {
#     df <- df[order(df$Freq, decreasing = TRUE), ]
#   }
#
#   # 빈도(Freq)의 최대값에 add_range를 더하여 y 축 범위 설정
#   max_value <- max(df$Freq, na.rm = TRUE) + add_range
#
#
#   # flip이 TRUE일 경우 그래프를 가로로 회전
#   if (flip) {
#     # ggplot으로 막대 그래프 생성
#     p <- ggplot(df, aes(x = reorder(term, Freq), y = Freq, fill = term)) +
#       geom_bar(stat = "identity") +
#       geom_text(aes(label = paste0(Freq, " (", round(`prop(%)`, 2), "%)")),
#                 hjust = hjust, size = size.text) +
#       labs(x = x.title, y = "빈도(Freq)") +
#       ylim(0, max_value) +  # y 축 범위를 설정
#       theme_bw() +
#       theme(
#         axis.text = element_text(size = size.x, face = "bold"),
#         axis.title = element_text(size = size.x, face = "bold"),
#         legend.position = "none")  # 범례를 숨김 (각 막대가 색상으로 구분되므로 범례가 불필요함)
#
#     p <- p + coord_flip()
#
#   } else {
#     # 범주의 순서 설정
#     df$term <- factor(df$term, levels = c("전혀 아니다", "아니다", "중간이다", "그렇다", "매우 그렇다"))
#
#     # flip이 FALSE일 경우
#     p <- ggplot(df, aes(x = term, y = Freq, group = 1, fill = term)) +
#       geom_bar(stat = "identity") +
#       geom_text(aes(label = paste0(Freq, "\n (", round(`prop(%)`, 2), "%)")),
#                 vjust = vjust, size = size.text)
#
#     # show_line이 TRUE일 경우에만 선을 그리도록 설정
#     if (show_line) {
#       p <- p + geom_line(aes(x = term, y = Freq), show.legend = FALSE,
#                          linetype=linetype, color=linecolor, linewidth=0.8) +
#         geom_point(aes(x = term, y = Freq), show.legend = FALSE)
#     }
#
#     p <- p + labs(x = x.title, y = "빈도(Freq)") +
#       ylim(0, max_value) +  # y 축 범위를 설정
#       theme_bw() +
#       theme(
#         axis.text = element_text(size = size.x, face = "bold"),
#         axis.title = element_text(size = size.x, face = "bold"),
#         legend.position = "none")  # 범례를 숨김 (각 막대가 색상으로 구분되므로 범례가 불필요함)
#   }
#
#   x11(width = x11_width, height = x11_height)
#   # 그래프 출력
#   print(p)
# }
#
