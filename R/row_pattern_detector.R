#' row pattern detector
#'
#' @param df df
#' @param pattern pattern =""
#' @param rev Find the opposite row of the pattern
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 샘플 데이터프레임
#' df <- data.frame(
#'   A = c(1, NA, 3, 4),
#'   B = c("Yes", "No", NA, "Yes"),
#'   C = c(NA, 2, 3, 4)
#' )
#'
#' # NA 값을 포함한 행을 찾음
#' na_rows <- pattern_detector(df, pattern = NA)
#' print(na_rows)
#'
#' # "No" 값을 포함한 행을 찾음
#' pattern_rows <- pattern_detector(df, pattern = "No")
#' print(pattern_rows)
#'
#' # "No" 값을 포함하지 않은 행을 찾음 (rev = TRUE)
#' non_pattern_rows <- pattern_detector(df, pattern = "No", rev = TRUE)
#' print(non_pattern_rows)

#' }
pattern_detector <- function(df, pattern = "", rev = FALSE) {
  # 패턴에 맞는 행을 찾는지 여부를 결정하는 함수
  find_pattern <- function(row) {
    if (is.na(pattern)) {
      return(any(is.na(row)))
    } else {
      return(any(row == pattern, na.rm = TRUE))
    }
  }

  # 패턴에 맞는 행 또는 맞지 않는 행을 선택
  if (rev == TRUE) {
    # 패턴에 맞지 않는 행을 반환
    return(df[apply(df, 1, function(row) !find_pattern(row)), ])
  } else {
    # 패턴에 맞는 행을 반환
    return(df[apply(df, 1, function(row) find_pattern(row)), ])
  }
}
