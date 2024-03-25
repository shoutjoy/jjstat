#' Define a function: Convert numbers to scientific notation or common number
#'
#' @param number number
#'
#' @return  formatting
#' @export
#'
#' @examples
#' \dontrun{
#' ## number of data
#' nchar(sub("\\d+\\.", "", as.character(0.123456)))
#'
#' numbers <- c(0.123456, 123.456789, 0.0000123456)
#' sapply(numbers, format_number)
#'
#' for (i in seq_along(numbers)) {
#'    cat("Original:", numbers[i], "Formatted:", formatted_numbers[i], "\n")}
#' }
#'
format_number <- function(number) {
  # Use scientific notation when there are more than 18 decimal places
  if (nchar(sub("\\d+\\.", "", as.character(number))) >= 8) {
    return(format(number, scientific = TRUE, digits = 3))
  } else {
    return(format(number, scientific = FALSE, digits = 5))
  }
}
