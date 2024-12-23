#' colnames_change: Change column names partially or entirely, with optional preservation of original names
#'
#' @param df A data frame. The target data frame whose column names need to be changed.
#' @param ... Character vector of new column names. Use an empty string (`""`) to keep the original column name for specific positions.
#'
#' @return A data frame with updated column names. Columns corresponding to `""` in the input retain their original names.
#' @export
#' @examples
#' \dontrun{
#' # Example with partial column renaming using mtcars
#' data(mtcars)
#' head(mtcars)
#'
#' # Rename selected columns
#' mtcars_updated <- mtcars %>%
#'   colnames_change("Miles_per_Gallon", "Cylinders", "Displacement", "", "Weight")
#'
#' head(mtcars_updated)
#' }
#'
colnames_change <- function(df, ...) {
  # Get new column names
  new_colnames <- c(...)

  # Get current column names from the data frame
  current_colnames <- colnames(df)

  # Check if the number of new column names exceeds the number of columns in the data frame
  if (length(new_colnames) > ncol(df)) {
    stop("The number of input column names exceeds the number of columns in the data frame.")
  }

  # Update column names: Preserve original names for empty strings
  updated_colnames <- current_colnames
  updated_colnames[1:length(new_colnames)] <- ifelse(new_colnames == "",
                                                     current_colnames[1:length(new_colnames)],
                                                     new_colnames)

  # Apply the updated column names to the data frame
  colnames(df) <- updated_colnames

  return(df)
}
