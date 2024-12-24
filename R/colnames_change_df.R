
#' colnames_change_df: Change column names of a data frame within a specified range
#'
#' @param df A data frame whose column names need to be changed.
#' @param range A vector or range indicating the columns to change. It can be numeric (e.g., 1:27) or character specifying column names (e.g., "disp:vs").
#' @param impute A character vector of new column names to assign to the specified range.
#'
#' @return The data frame with updated column names.
#' @export
#'
#' @examples
#' # Example 1: Change the first 11 columns of mtcars
#' new_colnames <- c(
#'   paste0("feature_", 1:6),
#'   paste0("metric_", 1:5)
#' )
#' updated_mtcars <- colnames_change_df(
#'   df = mtcars,
#'   range = 1:11,
#'   impute = new_colnames
#' )
#'
#' # Example 2: Change a subset of columns in mtcars by name range
#' new_colnames <- c(
#'   paste0("feature_", 1:6)   # Replace columns "disp" to "vs"
#' )
#' updated_mtcars <- colnames_change_df(
#'   df = mtcars,
#'   range = "disp:vs",
#'   impute = new_colnames
#' )
#'
colnames_change_df <- function(df, range = 1:ncol(df), impute = NULL) {
  # Convert range to numeric indices if it is character-based
  if (is.character(range)) {
    range_split <- unlist(strsplit(range, ":"))
    if (length(range_split) == 2) {
      start_col <- which(colnames(df) == range_split[1])
      end_col <- which(colnames(df) == range_split[2])
      if (length(start_col) == 0 || length(end_col) == 0) {
        stop("Column names specified in 'range' are not found in the data frame.")
      }
      cols_to_update <- seq(start_col, end_col)
    } else {
      stop("Invalid character range format. Use 'start_col:end_col'.")
    }
  } else if (is.numeric(range)) {
    cols_to_update <- range
  } else {
    stop("'range' must be either numeric or a character string.")
  }

  # Validate range indices
  if (any(cols_to_update > ncol(df)) || any(cols_to_update < 1)) {
    stop("The 'range' contains invalid column indices.")
  }

  # Validate impute length
  if (is.null(impute) || length(impute) != length(cols_to_update)) {
    stop("The 'impute' vector must be provided and its length must match the range.")
  }

  # Assign new column names
  colnames(df)[cols_to_update] <- impute
  return(df)
}
