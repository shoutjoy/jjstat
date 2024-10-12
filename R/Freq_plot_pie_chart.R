#' pie chart(using fei)
#'
#' @param data data
#' @param title title
#' @param size.text 5
#' @param title.size  20
#' @param vjust 0.5
#' @param palette "Set2", "Set3"
#'
#' @return graph
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 데이터
#' data <- data.frame(
#'   구분 = c("전혀 아니다", "아니다", "중간이다", "그렇다", "매우 그렇다"),
#'   빈도 = c(52, 90, 304, 257, 114),
#'   비율 = c(6.36, 11.02, 37.21, 31.46, 13.95)
#' )
#'
#' # 함수 호출
#' plot_pie_chart(data,,"Q1응답",0.5)

#' }
#'
plot_pie_chart <- function(data,
                           title = "",
                           size.text=5,
                           title.size = 20,
                           vjust=0.5,
                           palette = "Set2") {
  # 데이터프레임 확인
  if(!all(c("구분", "빈도", "비율(%)") %in% colnames(data))) {
    stop("데이터는 '구분', '빈도', '비율'이라는 열이 필요합니다.")
  }

  # 라벨 추가: 구분: 빈도(비율)
  data$label <- paste(data$구분, ": \n", data$빈도, "(", round(data$`비율(%)`, 1), "%)", sep = "")


  x11()
  # 파이 차트를 그리기 위한 비율 및 구분 라벨 추가
  ggplot(data, aes(x = "", y = `비율(%)`, fill = 구분)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    geom_text(aes(label = label),
              position = position_stack(vjust = vjust), size= size.text) +
    theme_void() +
    labs(title = title) +
    theme(plot.title = element_text(size=title.size))+
    scale_fill_brewer(palette =  palette)
}
