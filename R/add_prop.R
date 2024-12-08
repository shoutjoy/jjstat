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
#' #'
#' freq:
#'
#'   기본값은 "Freq".
#' 숫자일 경우 열 이름으로 변환.
#' percentage:
#'
#'   기본값은 TRUE.
#' TRUE일 경우, 비율에 100을 곱해 백분율로 나타냄.
#' FALSE일 경우, 단순 비율 값을 나타냄.
#' 출력 열:
#'
#'   percentage = TRUE → 열 이름은 prop(%).
#' percentage = FALSE → 열 이름은 prop.
#' mtcars$am %>% Freq_table() %>% add_prop()
#' #'
#' # 예제 데이터
#' df <- data.frame(Category = c("A", "B", "C"), Freq = c(30, 50, 20))
#'
#' # percentage = TRUE (백분율로 출력)
#' add_prop(df, freq = "Freq", percentage = TRUE)
#'
#' # percentage = FALSE (비율로 출력)
#' add_prop(df, freq = "Freq", percentage = FALSE)
#'
#' }
#'
#'
add_prop <- function(df, freq = "Freq", percentage = TRUE) {
  # freq가 숫자일 경우 열 이름으로 변환
  if (is.numeric(freq)) {
    freq_col <- colnames(df)[freq]
  } else {
    freq_col <- freq
  }

  # Freq 열의 합계 계산
  total_freq <- sum(df[[freq_col]], na.rm = TRUE)

  # 비율 열 생성
  if (percentage) {
    df$`prop(%)` <- round(df[[freq_col]] / total_freq * 100, 2)
  } else {
    df$`prop` <- round(df[[freq_col]] / total_freq, 2)
  }

  return(df)
}

# add_prop <- function(df, freq = "Freq") {
#   # freq가 숫자일 경우 열 이름으로 변환
#   if (is.numeric(freq)) {
#     freq_col <- colnames(df)[freq]
#   } else {
#     freq_col <- freq
#   }
#
#   # Freq 열의 합계 계산
#   total_freq <- sum(df[[freq_col]], na.rm = TRUE)
#
#   # 비율 열 생성
#   df$`prop(%)` <- round(df[[freq_col]] / total_freq, 2)
#
#   return(df)
# }

