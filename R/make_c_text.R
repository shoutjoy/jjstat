#' make_c_text
#'
#' @param data vector data
#'
#' @return  text
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#' # Example usage
#' data <- c("FieldGoals", "OtherTDs", "YardsRushAtt", "RushYards",
#'           "RushFirstDown", "YardsPassComp", "PassYards", "PassFirstDown",
#'           "YardsRushAtt", "RushYards", "RushFirstDown", "YardsPassComp",
#'           "PassYards", "PassFirstDown", "PointsGame", "OffensTD",
#'           "TDGame", "Speical", "Rushing", "Passing",
#'           "Offense", "Scoring", 123, 456.78)
#'
#' make_c_text(data)
#'
#' }
make_c_text <- function(data) {
  # Initialize the text_New vector
  text_New <- c(data)

  # Function to determine the correct format for each element
  format_element <- function(element) {
    if (is.numeric(element)) {
      return(as.character(element))
    } else {
      return(shQuote(element))
    }
  }

  # Format each element in the text_New vector
  formatted_elements <- sapply(text_New, format_element)

  # Print the result in the desired format
  cat("text_New =", paste("c(",
                          paste(formatted_elements, collapse = ", "), ")\n", sep=""))
}
