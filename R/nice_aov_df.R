#' Nice tables for papers in aov_df results  to  word, html
#'
#' @param df aov_Df
#' @param digits 2
#'
#' @return result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' aov_df(data = mtcars,
#'        dv_var = "mpg",
#'        iv_vars = c("cyl", "gear", "carb") )
#'
#' ## generate nice table
#' aov_df(data = mtcars,
#'        dv_var = "mpg",
#'        iv_vars = c("cyl", "gear", "carb") )%>%
#'   nice_aov_df()
#'
#'
#' aov_df(data = mtcars,
#'        dv_var = "mpg",
#'        iv_vars = c("cyl", "gear", "carb") )%>%
#'   nice_aov_df() %>%web()
#'
#'
#' }
nice_aov_df <- function(df, digits = 2) {
  # Convert all columns to character to avoid factor issues
  df= df%>%Round(digits=digits)
  df[] <- lapply(df, as.character)

  # Get the unique values of the 'iv' column
  unique_iv <- unique(df$iv)



  # Initialize an empty data frame to store the result
  result_df <- df[0, ]
  # Iterate over each unique value in the 'iv' column
  for (value in unique_iv) {
    # Subset the rows where 'iv' is equal to the current unique value
    subset_df <- df[df$iv == value, ]

    # If there are more than one rows, blank out the duplicate rows
    if (nrow(subset_df) > 1) {
      subset_df[-1, -c(1, which(names(subset_df) %in% c("level","Mean", "POSTHOC")))] <- ""  # Blank out all columns except the first row and 'iv'

    }
    # Bind the subset to the result data frame
    result_df <- bind_rows(result_df, subset_df)
  }
  # Blank out duplicates in the 'iv' column of the result_df
  duplicated_iv <- duplicated(result_df$iv)
  result_df$iv[duplicated_iv] <- ""
  return(result_df)

}
