#' fact varaible to num  in data.frame
#'
#' @param df df
#' @param col select col
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
Char2num_fct <- function(df, col = 1) {
  # 열 번호나 이름을 지원하도록 열을 선택하는 함수
  get_column <- function(df, col) {
    if (is.numeric(col)) {
      return(df[[col]])  # 열 번호인 경우
    } else {
      return(df[[as.character(col)]])  # 열 이름인 경우
    }
  }

  # 선택된 열이 factor인지 확인하고 변환
  if (is.factor(get_column(df, col))) {
    df[[col]] <- as.numeric(as.character(get_column(df, col)))
  } else {
    warning("The selected column is not a factor.")
  }

  return(df)
}
