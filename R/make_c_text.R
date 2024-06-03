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

  # Get the names of the elements if they exist
  element_names <- names(text_New)

  # Function to determine the correct format for each element
  format_element <- function(element, name = NULL) {
    formatted_element <- if (is.numeric(element)) {
      as.character(element)
    } else {
      shQuote(element)
    }

    if (!is.null(name)) {
      return(paste0(name, "=", formatted_element))
    } else {
      return(formatted_element)
    }
  }

  # Format each element in the text_New vector
  if (!is.null(element_names)) {
    formatted_elements <- mapply(format_element, text_New, element_names)
  } else {
    formatted_elements <- sapply(text_New, format_element)
  }

  # Print the result in the desired format
  cat("text_New =", paste("c(",
                          paste(formatted_elements, collapse = ", "), ")\n", sep=""))
}
