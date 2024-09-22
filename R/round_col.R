#' round_col, 한개의 열만 round처리
#'
#' @param df df
#' @param col select col , default last column
#' @param digits default 3
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'
#' }
round_col <- function(df, col = ncol(df), digits = 3) {
  # 입력 데이터가 데이터프레임인지 확인
  if (!is.data.frame(df)) {
    stop("The input df must be a data frame.")
  }

  # col이 숫자인지 또는 문자(열 이름)인지 확인
  if (is.numeric(col)) {
    # 숫자일 경우 유효한 범위인지 확인
    if (col > ncol(df) || col < 1) {
      stop("The col index is out of bounds.")
    }
  } else if (is.character(col)) {
    # 문자열(열 이름)일 경우 해당 열이 존재하는지 확인
    if (!(col %in% colnames(df))) {
      stop("The column name does not exist in the data frame.")
    }
    # 열 이름을 열 번호로 변환
    col <- which(colnames(df) == col)
  } else {
    stop("The col parameter should be either a numeric index or a column name.")
  }

  # 지정된 열이 숫자형인지 확인
  if (!is.numeric(df[[col]])) {
    stop("The selected column is not numeric.")
  }

  # 지정된 열을 반올림하고 과학적 표기법으로 변환
  df[[col]] <- format(round(df[[col]], digits), scientific = TRUE)

  return(df)
}
