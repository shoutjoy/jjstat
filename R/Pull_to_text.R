#' Convert Data Frame Row to Text with Rounded Digits
#'
#' @param df A data frame or tibble with a single row.
#' @param digits Integer, number of decimal places to round numeric values (default = 3).
#'
#' @return A character string representing the key-value pairs from the data frame.
#' @export
#'
#' @examples
#' df <- tibble::tibble(R = 0.205678, adj.R = 0.204123, F = 195.456, p = 3.67e-222, dof = 6, AIC = 10028.1234, BIC = 10080.9876)
#' Pull_to_text(df, digits = 3)
Pull_to_text <- function(df, digits = 3) {
  # Check if the input is a single-row data frame
  if (nrow(df) != 1) {
    stop("The input data frame must have exactly one row.")
  }

  # Format each column: round numeric values and keep others as is
  output <- sapply(names(df), function(col) {
    value <- df[[col]]
    if (is.numeric(value)) {
      value <- round(value, digits)  # Apply rounding for numeric values
    }
    paste0(col, " = ", value)
  })

  # Combine into a single string with commas
  result <- paste(output, collapse = ", ")

  cat("\n", result,"\n\n")
  return(result)
}
