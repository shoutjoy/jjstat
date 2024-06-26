#' 행을 결합해주는 함수
#'
#' @param data data
#' @param row1 row1 합쳐질 곳
#' @param row2 row2 가져올 곳
#' @param sep sep "," default
#' @param left left "["
#' @param right right"]"
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data17 <- data.frame(
#'   criterian = c("Chisq", "df", "p >0.05", "RMSEA <0.05",
#'                 "90%CI.lower", "90%CI.upper", "p <= 0.05",
#'                 "SRMR <0.08", "GFI >0.95", "CFI >0.95", "TLI >0.90", "lower", "lower"),
#'   Value = c(40.179, 38.000, 0.374, 0.028, 0.000, 0.087,
#'             0.665, 0.056, 0.920, 0.997, 0.995, 3153.636, 3218.526)
#' )
#' data17
#' # 함수 실행 예시
#' unite_rows(data17, row1 = 5, row2 = 6)
#' unite_rows(data17, row1 = 5, row2 = 6)%>%move_row(6,5)
#'
#' mtcars[1:5,]
#' mtcars[1:5,] %>%unite_rows(1,3)
#' }
#'
#'
#'
unite_rows <- function(data, row1, row2, sep = ", ", left = "[", right = "]") {
  # Convert data from specific rows (row1, row2) to a character type and merge them.
  # combined_row <- paste0(left, data[row1,], sep, data[row2,], right)
  # # Insert the merged rows into the original dataframe and keep the remaining rows intact.
  # data[row1, ] <- combined_row
  # data <- data[-row2, ]
  combined_row <- sapply(seq_along(data[row1, ]), function(i) {
    paste0(left, data[row1, i], sep, data[row2, i], right)
  })


  data[row1, ] <- combined_row
  data <- data[-row2, , drop = FALSE]
  # # Set the combined row name
  # rownames(data)[row1] <- paste0(left, rownames(data)[row1], sep, rownames(data)[row2], right)

  return(data)
}
