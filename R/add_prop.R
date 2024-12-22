#' Add Proportions to Frequency Data
#'
#' @description
#' This function adds a column to a data frame representing the proportion of each frequency value,
#' either as a percentage or a simple ratio.
#'
#' @param df A data frame containing frequency data.
#' @param freq A column name or index representing the frequency column. Defaults to "Freq".
#' If a numeric value is provided, it is converted to the corresponding column name.
#' @param prop Logical, whether to return proportions as percentages (TRUE, default) or ratios (FALSE).
#'
#' @return A data frame with an additional column for proportions.
#' If `prop = TRUE`, the column is named `prop(%)`.
#' If `prop = FALSE`, the column is named `prop`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' df <- data.frame(Category = c("A", "B", "C"), Freq = c(30, 50, 20))
#'
#' # Adding proportions as percentages
#' add_prop(df, freq = "Freq", prop = TRUE)
#'
#' # Adding proportions as simple ratios
#' add_prop(df, freq = "Freq", prop = FALSE)
#' }
add_prop <- function(df, freq = "Freq", prop = TRUE) {
  # Check if freq is numeric and convert to column name
  if (is.numeric(freq)) {
    freq_col <- colnames(df)[freq]
  } else {
    freq_col <- freq
  }

  # Calculate the total sum of the frequency column
  total_freq <- sum(df[[freq_col]], na.rm = TRUE)

  # Add a proportion column
  if (prop) {
    df$`prop(%)` <- round(df[[freq_col]] / total_freq * 100, 2)
  } else {
    df$`prop` <- round(df[[freq_col]] / total_freq, 2)
  }

  return(df)
}
