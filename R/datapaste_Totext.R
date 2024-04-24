#' data.frame to input text
#'
#' @param data data.frame
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # 데이터 프레임 생성
#' cho2013_cor <- data.frame(
#'   필요성 = c(1.000, 0.320, 0.266, 0.506, 0.429, 0.401),
#'   수업 = c(0.320, 1.000, 0.647, 0.463, 0.548, 0.554),
#'   교사 = c(0.266, 0.647, 1.000, 0.383, 0.491, 0.545),
#'   계획 = c(0.506, 0.463, 0.383, 1.000, 0.531, 0.491),
#'   영향 = c(0.429, 0.548, 0.491, 0.531, 1.000, 0.743),
#'   만족도 = c(0.401, 0.554, 0.545, 0.491, 0.743, 1.000)
#' )
#' cho2013_cor %>% make_df_text()
#'
#'
#' }
#'
make_df_text <- function(data) {
  # 데이터프레임의 칼럼 이름 가져오기
  col_names <- names(data)

  # 텍스트로 변환
  df_text <- "New = data.frame("
  for (col in col_names) {
    df_text <- paste(df_text,
                     paste(col, " = c(",
                           paste(data[, col], collapse = ", "), ")",
                           sep = ""), sep = ",\n  ")
  }
  df_text <- sub(",\n", "\n", df_text)  # 첫 번째 줄의 쉼표와 줄바꿈 제거
  df_text <- paste(df_text, ")\n", sep = "")

  # 결과 출력
  cat(df_text,"\n\n")
}




#' data.frame to input text
#'
#' @param data data.frame
#'
#' @return text
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # 데이터 프레임 생성
#' cho2013_cor <- data.frame(
#'   필요성 = c(1.000, 0.320, 0.266, 0.506, 0.429, 0.401),
#'   수업 = c(0.320, 1.000, 0.647, 0.463, 0.548, 0.554),
#'   교사 = c(0.266, 0.647, 1.000, 0.383, 0.491, 0.545),
#'   계획 = c(0.506, 0.463, 0.383, 1.000, 0.531, 0.491),
#'   영향 = c(0.429, 0.548, 0.491, 0.531, 1.000, 0.743),
#'   만족도 = c(0.401, 0.554, 0.545, 0.491, 0.743, 1.000)
#' )
#' cho2013_cor %>% datapaste_text()
#'
#'
#' }
#'
datapaste_text <- function(data) {
  # 데이터프레임의 칼럼 이름 가져오기
  col_names <- names(data)

  # 텍스트로 변환
  df_text <- "New = data.frame("
  for (col in col_names) {
    df_text <- paste(df_text,
                     paste(col, " = c(",
                           paste(data[, col],
                           collapse = ", "), ")", sep = ""),
                     sep = ",\n  ")
  }
  df_text <- sub(",\n", "\n", df_text)  # 첫 번째 줄의 쉼표와 줄바꿈 제거
  df_text <- paste(df_text, ")\n", sep = "")

  # 결과 출력
  cat(df_text,"\n\n")
}
