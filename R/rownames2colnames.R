
#' Convert Row Names to a Column and Vice Versa
#'
#' Converts row names of a data frame into a new column,
#' or restores a column as row names.
#'
#' @param df A data frame to be converted.
#' @param colname Character or NULL. Name of the new column
#'   when reverse = FALSE. Default NULL uses "rownames".
#'   When reverse = TRUE, specifies the column to use
#'   as row names. Default NULL uses the first column.
#' @param reverse Logical. If FALSE (default), converts
#'   row names into a column. If TRUE, converts a column
#'   into row names and removes that column.
#'
#' @return A data frame with converted row or column names.
#'
#' @details
#' When reverse = FALSE, the original row names are
#' preserved in a new column.
#' When reverse = TRUE, the selected column must contain
#' unique, non-missing, non-empty values.
#'
#' @export
#'
#' @examples
#' # Example 1: Create sample data
#' df <- data.frame(a = 1:3, b = 4:6)
#' rownames(df) <- c("row1", "row2", "row3")
#'
#' # Example 2: Default column name
#' rownames2colnames(df)
#'
#' # Example 3: Custom column name
#' rownames2colnames(df, colname = "Factor")
#'
#' # Example 4: Korean column name
#' rownames2colnames(df, colname = "요인")
#'
#' # Example 5: Restore original row names
#' new_df <- rownames2colnames(df, colname = "Factor")
#' rownames2colnames(new_df, reverse = TRUE)
#'
#' # Example 6: Specify reverse column
#' rownames2colnames(new_df,
#'                   colname = "Factor",
#'                   reverse = TRUE)
#'
#' # Example 7: Using mtcars
#' rownames2colnames(mtcars, colname = "Car")
#'
#' # Example 8: Round-trip conversion
#' df2 <- rownames2colnames(df, colname = "ID")
#' df3 <- rownames2colnames(df2, reverse = TRUE)
#' identical(df, df3)
#'
rownames2colnames <- function(df,
                              colname = NULL,
                              reverse = FALSE) {

  # 1. Validate input data
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  # 2. Validate reverse option
  if (!is.logical(reverse) ||
      length(reverse) != 1L ||
      is.na(reverse)) {
    stop("'reverse' must be TRUE or FALSE.")
  }

  # 3. Validate colname option
  if (!is.null(colname)) {
    if (!is.character(colname) ||
        length(colname) != 1L ||
        is.na(colname) ||
        !nzchar(colname)) {
      stop("'colname' must be NULL or a non-empty string.")
    }
  }

  # 4. Convert a column into row names
  if (reverse) {

    # Use the first column when colname is NULL
    if (is.null(colname)) {
      if (ncol(df) == 0L) {
        stop("Data frame has no columns.")
      }
      colname <- names(df)[1]
    }

    # Check whether the column exists
    if (!colname %in% names(df)) {
      stop("Column not found: ", colname)
    }

    # Extract row name values
    new_rownames <- as.character(df[[colname]])

    # Check missing, empty, and duplicated values
    if (anyNA(new_rownames) ||
        any(!nzchar(new_rownames)) ||
        anyDuplicated(new_rownames)) {
      stop("Row names must be unique, non-missing, ",
           "and non-empty.")
    }

    # Remove the selected column
    df[[colname]] <- NULL

    # Assign new row names
    rownames(df) <- new_rownames

  } else {

    # 5. Convert row names into a column
    if (is.null(colname)) {
      colname <- "rownames"
    }

    # Prevent duplicate column names
    if (colname %in% names(df)) {
      stop("Column name already exists: ", colname)
    }

    # Extract existing row names
    old_rownames <- rownames(df)

    # Create the new column at the first position
    df <- data.frame(
      setNames(list(old_rownames), colname),
      df,
      check.names = FALSE,
      row.names = NULL
    )

    # Reset row names
    rownames(df) <- NULL
  }

  # 6. Return the converted data frame
  return(df)
}
