
#' colnames_to_row: Insert column names as a row in a data frame
#'
#' @param df A data frame whose column names will be inserted as a row.
#' @param row An integer specifying the row number where column names should be inserted. Default is 1.
#'
#' @return A data frame with column names inserted as a row at the specified position.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#' colnames_to_row(df)
#' colnames_to_row(df, row = 2)
#' mtcars
#' mtcars %>% nrow()
#' mtcars %>% colnames_to_row(1)
#' mtcars %>% colnames_to_row(3)
#' mtcars %>% colnames_to_row(32)
#' }
colnames_to_row <- function(df, row = 1) {
  if (!is.data.frame(df)) stop("Input must be a data frame.")
  if (!is.numeric(row) || row <= 0 || row != as.integer(row)) stop("Row must be a positive integer.")

  # Extract column names
  col_names <- as.character(colnames(df))

  # Create a data frame of column names
  col_row <- as.data.frame(t(col_names), stringsAsFactors = FALSE)
  colnames(col_row) <- colnames(df)

  # Handle edge cases for row insertion
  if (row == 1) {
    # Add column names at the top
    result <- rbind(col_row, df)
  } else if (row > nrow(df)) {
    # Add column names as the last row
    result <- rbind(df, col_row)
  } else {
    # Insert column names at the specified position
    result <- rbind(
      df[1:(row - 1), , drop = FALSE],
      col_row,
      df[row:nrow(df), , drop = FALSE]
    )
  }

  # Reset row names
  rownames(result) <- NULL

  return(result)
}
