#' Remove all ATTRs present in the data, deselect
#'
#' @param df data
#' @param range col range
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example usage with your data frame jut3
#' # Remove attributes from all columns
#' # jut3_cleaned_all <- remove_attr(jut3)
#'
#' # Remove attributes from columns 1 to 5
#' # jut3_cleaned_partial <- remove_attr(jut3, range = 1:5)
#'
#' # Check the structure
#' # str(jut3_cleaned_all)
#' # str(jut3_cleaned_partial)
#' }
#'
#'
remove_attr <- function(df, range = 1:ncol(df)) {
  # Function to remove attributes from a single column
  remove_column_attrs <- function(column) {
    attributes(column) <- NULL
    return(column)
  }

  # Apply the attribute removal to the specified columns
  df_cleaned <- df
  df_cleaned[range] <- lapply(df_cleaned[range], remove_column_attrs)

  # Optionally, remove attributes from the data frame itself (excluding column attributes)
  overall_attrs <- attributes(df_cleaned)
  overall_attrs$names <- names(df_cleaned)
  attributes(df_cleaned) <- overall_attrs

  return(df_cleaned)
}




#' Remove all ATTRs present in the data
#'
#' @param df data
#'
#' @return data
#' @export
#'
remove_all_attr <- function(df) {
  df <- df %>% mutate(across(everything(), ~ {
    attributes(.) <- NULL
    .
  }))
  return(df)
}
