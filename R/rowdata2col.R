
#' Select from ROW to make it a column name
#'
#' @param data data
#' @param row select row
#'
#' @return data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#' #'
#' # 예제 데이터 생성
#' data <- data.frame(
#'   X1 = c(1.000000, 13.437500, 5.464659, 16.000000),
#'   X2 = c(2.000000, 11.800000, 4.813671, 15.000000),
#'   X3 = c(3.000000, 12.473684, 4.032761, 19.000000)
#' )
#' rownames(data) <- c("var", "Mean", "SD", "N")

#' # 함수 사용 예시
#' result <- rowdata2col(data, "var")
#' print(result)
#'
#'
#' }
rowdata2col <- function(data, row) {
  # Set the row of data to the column name
  new_colnames <- as.character(data[row, ])

  # Create a new dataframe and set column names
  new_data <- data[-row, ]
  colnames(new_data) <- new_colnames

  return(new_data)
}
