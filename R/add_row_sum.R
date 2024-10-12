#' add_row_sum, row sum
#'
#' @param df df
#' @param freq_col "Freq" or number
#' @param prop_col "prop(%)" or number
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars$cyl %>% Freq_table(prop = TRUE) %>% addrow_sum()
#' }
add_row_sum <- function(df, freq_col = 2, prop_col = 3) {
  # 열 번호나 이름을 지원하는 함수
  get_column <- function(df, col) {
    if (is.numeric(col)) {
      return(df[[col]])  # 열 번호인 경우
    } else {
      return(df[[as.character(col)]])  # 열 이름인 경우
    }
  }

  # Freq와 prop(%)의 합을 계산, 숫자 타입(int, dbl)만 합산
  total_freq <- if (is.numeric(get_column(df, freq_col))) {
    sum(get_column(df, freq_col), na.rm = TRUE)
  } else {
    NA
  }

  total_prop <- if (is.numeric(get_column(df, prop_col))) {
    sum(get_column(df, prop_col), na.rm = TRUE)
  } else {
    NA
  }

  # 열 이름 추출
  freq_col_name <- if(is.numeric(freq_col)) names(df)[freq_col] else freq_col
  prop_col_name <- if(is.numeric(prop_col)) names(df)[prop_col] else prop_col

  # 합계 행 생성
  total_row <- df[1, ]  # 기존 데이터 구조 유지
  total_row[1, ] <- NA  # 첫 열(구분)에 NA 할당
  total_row[[freq_col_name]] <- total_freq
  total_row[[prop_col_name]] <- total_prop
  total_row[[1]] <- "합계"  # 첫 열에 '합계' 입력

  # chr 타입 열에서 NA를 "-"로 변환
  total_row <- total_row %>% mutate(across(where(is.character), ~ ifelse(is.na(.), "-", .)))

  # 원래 데이터에 합계 행 추가하여 반환
  df_with_sum <- bind_rows(df, total_row)

  return(df_with_sum)
}

# add_row_sum <- function(df, freq_col = "Freq", prop_col = "prop(%)") {
#   # 열 번호나 이름을 지원하도록 열을 선택하는 함수
#   get_column <- function(df, col) {
#     if (is.numeric(col)) {
#       return(df[[col]])  # 열 번호인 경우
#     } else {
#       return(df[[as.character(col)]])  # 열 이름인 경우
#     }
#   }
#
#   # Freq와 prop(%)의 합을 계산
#   total_freq <- sum(get_column(df, freq_col), na.rm = TRUE)
#   total_prop <- sum(get_column(df, prop_col), na.rm = TRUE)
#
#   # 합계를 나타내는 행을 추가
#   total_row <- tibble(term = "합계",
#                       !!names(df)[if(is.numeric(freq_col)) freq_col else which(names(df) == freq_col)] := total_freq,
#                       !!names(df)[if(is.numeric(prop_col)) prop_col else which(names(df) == prop_col)] := total_prop)
#
#   # 원래 데이터에 합계 행을 추가하여 반환
#   df_with_sum <- bind_rows(df, total_row)
#
#   return(df_with_sum)
# }
