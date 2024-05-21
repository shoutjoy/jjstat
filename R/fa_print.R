#' Print Factor Analysis
#'
#' @param data suammry fa
#' @param cut cut off 0.4
#' @param sort TRUE FALSE
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data <- data.frame(
#'   source = c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09", "B10",
#'              "B11", "B12", "B13", "B14", "B15", "B16", "B17", "B18", "B19", "B20",
#'              "B21", "B22", "B23", "B24", "B25", "eigen_vlaue", "Proportion_Var", "Cumulative_Var"),
#'   MR1 = c(-0.487, 0.966, -0.411, 0.579, 0.075, 0.04, 0.04, -0.195, 0.15, 0.494, 0.059, -0.157, -0.149, 0.58, -0.39, -0.203, 0.109, 0.161, -0.173, -0.046, -0.156, 0.064, -0.161, -0.267, -0.06, 2.77, 0.111, 0.111),
#'   MR2 = c(-0.006, 0.029, 0.039, 0.021, 0.535, 0.689, 0.67, 0.427, 0.793, -0.122, -0.035, 0.014, 0.072, -0.019, -0.019, 0.141, -0.004, 0.139, 0.069, 0.013, -0.029, 0.01, -0.146, 0.646, 0.72, 3.05, 0.122, 0.233),
#'   MR4 = c(0.144, -0.051, 0.017, -0.034, -0.111, 0.171, -0.008, -0.043, 0.009, -0.013, 0.023, 0.661, 0.288, -0.111, 0.198, 0.297, 0.471, 0.268, -0.058, 0.751, 0.308, 0.429, 0.17, -0.161, 0.07, 1.93, 0.077, 0.31),
#'   MR5 = c(0.034, 0.157, 0.201, -0.231, -0.044, -0.003, 0.23, 0.236, -0.094, -0.351, 0.868, 0.155, 0.165, -0.296, 0.159, -0.059, -0.064, 0.22, 0.126, -0.077, 0.186, -0.072, 0.29, -0.037, -0.06, 1.49, 0.059, 0.369),
#'   MR3 = c(-0.037, 0.032, 0.389, -0.233, 0.195, -0.153, -0.228, -0.525, 0.174, 0.01, 0.011, 0.077, 0.174, 0.029, 0.021, 0.346, 0.176, 0.383, 0.561, -0.157, 0.399, 0.236, 0.242, 0.191, -0.104, 1.62, 0.065, 0.434)
#' )

# # 함수 실행 예시
# fa_print(data, cut = 0.3, sort = TRUE)
#' }
#'
fa_print <- function(data, cut = 0.4, sort = TRUE) {
  # 조건에 맞는 행을 필터링하여 data_head와 data_tail을 분리
  data_head <- data[!data$source %in% c("eigen_vlaue", "Proportion_Var", "Cumulative_Var"), ]
  data_tail <- data[data$source %in% c("eigen_vlaue", "Proportion_Var", "Cumulative_Var"), ]

  # data_head에 대해 cut 값을 적용하여 ""로 대체
  for (i in 2:ncol(data_head)) {
    if (is.numeric(data_head[[i]])) {
      data_head[[i]] <- ifelse(abs(data_head[[i]]) < cut, "", data_head[[i]])
    }
  }

  # sort가 TRUE일 경우 모든 열을 기준으로 정렬
  if (sort) {
    # 열 이름을 제외한 데이터 부분만 정렬
    data_head <- data_head[do.call(order, as.list(data_head[ , -1])), ]
  }

  # 두 부분을 다시 결합
  res <- rbind(data_head, data_tail)

  return(data.frame(res))
}
