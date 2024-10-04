#' add_partial_sum
#'
#' @param df df
#' @param col select col
#' @param freq_col select count frequency col
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 함수 사용 예시
#' df <- tibble::tribble(
#'   ~학교급, ~term, ~Freq, ~`prop(%)`,
#'   "-", "합계", 3110, 100,
#'   "고등학교", "고1", 341, 11.0,
#'   "고등학교", "고2", 322, 10.4,
#'   "고등학교", "고3", 192, 6.17,
#'   "중학교", "중1", 591, 19.0,
#'   "중학교", "중2", 527, 16.9,
#'   "중학교", "중3", 290, 9.32,
#'   "초등학교", "초1", 8, 0.257,
#'   "초등학교", "초2", 48, 1.54,
#'   "초등학교", "초3", 47, 1.51,
#'   "초등학교", "초4", 191, 6.14,
#'   "초등학교", "초5", 207, 6.66,
#'   "초등학교", "초6", 346, 11.1
#' )
#'
#' # 함수 실행
#' result_df <- add_partial_sum(df, col = "학교급", freq_col = "Freq")
#' print(result_df)
#' }
#'
add_partial_sum <- function(df, col, freq_col) {
  # col 인덱스 또는 이름에 따라 그룹화
  col_name <- if (is.numeric(col)) names(df)[col] else col
  freq_name <- if (is.numeric(freq_col)) names(df)[freq_col] else freq_col

  # 그룹별로 freq_col 값의 합을 계산한 데이터프레임 생성
  df_summary <- df %>%
    group_by(!!sym(col_name)) %>%
    summarise(partial_sum = sum(!!sym(freq_name), na.rm = TRUE))

  # 원래 데이터프레임에 partial_sum을 추가
  df <- df %>%
    left_join(df_summary, by = col_name)

  return(df)
}
