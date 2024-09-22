
#' rownames2colnames
#'
#' @param df df
#' @param reverse   reverse = FALSE
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#'
#' # 예시 데이터프레임
#' df <- data.frame(a = 1:3, b = 4:6)
#' rownames(df) <- c("row1", "row2", "row3")
#'
#' # rownames를 colnames로 변환
#' new_df <- rownames2colnames(df)
#' print(new_df)
#'
#' # colnames를 rownames로 변환
#' df_with_colnames_as_rownames <- rownames2colnames(new_df, reverse = TRUE)
#' print(df_with_colnames_as_rownames)
#' }
rownames2colnames <- function(df, reverse = FALSE) {
  # df가 데이터프레임이 아니면 에러를 발생시킵니다.
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  if (reverse) {
    # reverse = TRUE: colnames를 rownames로 변환
    if (!all(colnames(df) != "")) {
      stop("All columns must have names to use as rownames.")
    }
    rownames(df) <- colnames(df)  # colnames를 rownames로 변환
    colnames(df) <- NULL          # colnames를 제거
  } else {
    # reverse = FALSE: rownames를 colnames로 변환
    if (is.null(rownames(df))) {
      stop("Data frame does not have row names.")
    }
    df <- cbind(rownames(df), df) # rownames를 첫 번째 열로 추가
    colnames(df)[1] <- "rownames" # 첫 번째 열의 이름을 설정
    rownames(df) <- NULL          # rownames를 제거
  }

  return(df)
}
