#' Delete cols in the data
#'
#' @param data data
#' @param rm_col first column name or number
#' @param ... add columns
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 예시 사용법
#' col_remove(mtcars, 1, "am", 2, "vs", "wt")
#'
#' # 예시 사용법
#' col_remove(mtcars, 1, "am", 2, "vs", "wt",4)
#'
#'
#' # 예시 사용법
#' col_remove(mtcars, 1, "am", 2, "vs", "wt",3)
#'
#'
#' # 예시 사용법
#' col_remove(mtcars, 1, "am", 2, "vs", "wt",3,4)
#'
#' # 예시 사용법
#' col_remove(mtcars, rm_col=1:3)
#' col_remove(mtcars, c(1, 3))
#' col_remove(mtcars, 1,2, 3)
#' col_remove(mtcars, 1, "am",2,"vs","wt")
#' col_remove(mtcars, rm_col="mpg")
#' col_remove(mtcars, "mpg", "am","vs")
#'
#' }
#'
#'
remove_cols <- function(data, rm_col=NULL, ...) {
  # 추가적인 열 제거 인자를 리스트로 변환
  additional_rm_cols <- list(...)

  # rm_col이 NULL일 경우 빈 벡터로 초기화
  if (is.null(rm_col)) {
    rm_col <- character(0)
  }

  # rm_col과 추가적인 열 제거 인자를 하나의 벡터로 결합
  all_rm_cols <- c(rm_col, unlist(additional_rm_cols))

  # 모든 요소를 문자로 변환
  all_rm_cols <- sapply(all_rm_cols, as.character)

  # 숫자인 경우 열 이름으로 변환
  num_cols <- as.numeric(all_rm_cols[!is.na(suppressWarnings(as.numeric(all_rm_cols)))])
  char_cols <- all_rm_cols[is.na(suppressWarnings(as.numeric(all_rm_cols)))]

  if (length(num_cols) > 0) {
    num_col_names <- names(data)[num_cols]
    char_cols <- c(char_cols, num_col_names)
  }

  # 지정된 열 이름 제거
  if (length(char_cols) > 0) {
    data <- data[ , !(names(data) %in% char_cols), drop = FALSE]
  }

  return(data)
}
