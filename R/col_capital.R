
#' col_capital: Convert letters in a selected column to upper or lower case
#'
#' @param df A data.frame. The input dataset.
#' @param col A column name (character) or index (numeric) indicating which column to transform.
#' @param direction Logical. If TRUE (default), convert to upper case. If FALSE, convert to lower case.
#'
#' @return A data.frame with the selected column transformed to upper or lower case.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   구분 = c("chisq", "df", "pvalue"),
#'   criterian = c("Chisq", "df", "p > .05"),
#'   Value = c("1732.243", "269", "0")
#' )
#'
#' # Use column name
#' col_capital(df, "criterian", TRUE)
#'
#' # Use column index
#' col_capital(df, 2, FALSE)
#' }

col_capital <- function(df, col, direction = TRUE) {
  # Check if column exists
  if (is.character(col)) {
    if (!col %in% colnames(df)) {
      stop("The specified column name does not exist in the dataframe.")
    }
    col_index <- which(colnames(df) == col)
  } else if (is.numeric(col)) {
    if (col < 1 || col > ncol(df)) {
      stop("The specified column index is out of bounds.")
    }
    col_index <- col
  } else {
    stop("col must be either a column name (character) or index (numeric).")
  }

  # Convert to character if needed
  df[[col_index]] <- as.character(df[[col_index]])

  # Apply case conversion
  if (direction) {
    df[[col_index]] <- toupper(df[[col_index]])
  } else {
    df[[col_index]] <- tolower(df[[col_index]])
  }

  return(df)
}
