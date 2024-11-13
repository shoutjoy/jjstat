#' add_prop
#'
#' @param df df
#' @param freq col
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mtcars$am %>% Freq_table() %>% add_prop()
#'
#' }
#'
#'
add_prop <- function(df, freq = "Freq") {
  # freq가 숫자일 경우 열 이름으로 변환
  if (is.numeric(freq)) {
    freq_col <- colnames(df)[freq]
  } else {
    freq_col <- freq
  }

  # Freq 열의 합계 계산
  total_freq <- sum(df[[freq_col]], na.rm = TRUE)

  # 비율 열 생성
  df$`prop(%)` <- round(df[[freq_col]] / total_freq, 2)

  return(df)
}

