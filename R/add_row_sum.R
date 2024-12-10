#' Add Row Sum to Data Frame
#'
#' This function calculates the sum of specified numeric columns and appends a summary row
#' to the input data frame. The summary row includes totals for `freq_col` and `prop_col`.
#'
#' @param df A data frame containing the data to be summed.
#' @param freq_col Column name or index for the frequency column. Default is 2.
#' @param prop_col Column name or index for the proportion column. Default is 3.
#' @return A data frame with an added summary row containing the sums.
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   Group = c("A", "B", "C"),
#'   Freq = c(10, 20, 30),
#'   `prop(%)` = c(25.0, 50.0, 25.0)
#' )
#' add_row_sum(data, freq_col = "Freq", prop_col = "prop(%)")
#' }
add_row_sum <- function(df, freq_col = 2, prop_col = 3) {
  # Helper function to get column by index or name
  get_column <- function(df, col) {
    if (is.numeric(col)) {
      return(df[[col]])
    } else {
      return(df[[as.character(col)]])
    }
  }

  # Calculate totals for freq_col and prop_col
  total_freq <- if (is.numeric(get_column(df, freq_col))) {
    sum(get_column(df, freq_col), na.rm = TRUE)
  } else {
    NA
  }

  total_prop <- if (is.numeric(get_column(df, prop_col))) {
    sum(get_column(df, prop_col), na.rm = TRUE)
  } else {
    NA
  }

  # Get column names
  freq_col_name <- if (is.numeric(freq_col)) names(df)[freq_col] else freq_col
  prop_col_name <- if (is.numeric(prop_col)) names(df)[prop_col] else prop_col

  # Create a total row
  total_row <- df[1, , drop = FALSE]  # Maintain structure
  total_row[] <- NA  # Initialize with NA
  total_row[[freq_col_name]] <- total_freq
  total_row[[prop_col_name]] <- total_prop
  total_row[[1]] <- "합계"  # First column as '합계'

  # Replace NA in character columns with "-"
  total_row <- total_row %>% mutate(across(where(is.character), ~ ifelse(is.na(.), "-", .)))

  # Append total row to the original data frame
  df_with_sum <- bind_rows(df, total_row)

  return(df_with_sum)
}
