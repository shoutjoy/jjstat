#' 범위를 변환하는 데이터프레임 전용 함수 정의
#'
#' @param df df
#' @param from_min  from_min
#' @param from_max from_max
#' @param to_min tomin
#' @param to_max to max
#' @param select_col col
#' @param remove  FALSE 유지
#' @param post_fix _re
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' convert_scale_df(jen01, from_min = -4, from_max = 4, to_min = 1, to_max = 9,
#'                  select_col = contains("Q6_"), remove = FALSE, post_fix = "_re") %>%
#'                  str()
#'
#' # 변환된 데이터 구조 확인
#' str(jen01_transformed)
#' }
convert_scale_df <- function(df, from_min = -4, from_max = 4, to_min = 1, to_max = 9,
                             select_col = everything(), remove = FALSE, post_fix = "_re") {

  # 데이터 변환 함수 정의
  convert_scale <- function(x, from_min, from_max, to_min, to_max) {
    scaled_value <- ((x - from_min) / (from_max - from_min)) * (to_max - to_min) + to_min
    return(scaled_value)
  }

  # 선택된 열에 대해 변환 수행
  if (remove) {
    # 기존 열을 대체하는 경우
    df_transformed <- df %>%
      mutate(across({{ select_col }}, ~convert_scale(., from_min, from_max, to_min, to_max)))
  } else {
    # 기존 열을 유지하고 변환된 열을 추가하는 경우
    df_transformed <- df %>%
      mutate(across({{ select_col }}, ~convert_scale(., from_min, from_max, to_min, to_max),
                    .names = "{.col}{post_fix}"))
  }

  return(df_transformed)
}
