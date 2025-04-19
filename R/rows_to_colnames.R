#' Convert a Row into Column Names of a Data Frame
#'
#' This function sets the values of a specified row in a data frame as the new column names.
#' The row used for column names is then removed from the data.
#'
#' @param df A data frame. The input data where one row will be promoted to column names.
#' @param row An integer indicating the row number to use as column names. Defaults to 1.
#'
#' @return A data frame with updated column names and the specified row removed.
#' @export
#' @importFrom utils head
#'
#' @examples
#' # Create a simple data frame
#' df <- data.frame(
#'   V1 = c("Name", "Alice", "Bob"),
#'   V2 = c("Age", 25, 30),
#'   V3 = c("Gender", "F", "M"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Convert the first row to column names
#' new_df <- rows_to_colnames(df)
#' print(new_df)
#'
#' # Use the second row instead
#' df2 <- data.frame(
#'   A = c("Header1", "X", "Y"),
#'   B = c("Header2", 1, 2),
#'   stringsAsFactors = FALSE
#' )
#' rows_to_colnames(df2, row = 2)
rows_to_colnames <- function(df, row = 1) {
  # Extract the desired row as column names
  new_names <- as.character(df[row, ])

  # Remove the specified row from the data
  df_new <- df[-row, , drop = FALSE]

  # Clear current column names and assign new ones
  names(df_new) <- NULL
  names(df_new) <- new_names

  # Reset row names
  rownames(df_new) <- NULL

  return(df_new)
}
