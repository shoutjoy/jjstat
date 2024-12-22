#' read_text: Read a Text File with Flexible Handling Using read.delim
#'
#' @param file_path The path of the file to read.
#' @param header Logical. Does the file contain a header? Defaults to TRUE.
#' @param stringsAsFactors Logical. Should character columns be converted to factors? Defaults to FALSE.
#'
#' @return A data frame containing the file's contents.
#' @export
#'
#' @examples
#' \dontrun{
#' # Save and read a file
#' teacher_question <- c("Example question 1", "Example question 2")
#' write_text(teacher_question, file_path = "output.txt")
#' result <- read_text("output.txt")
#' }
#'
read_text <- function(file_path, header = TRUE, stringsAsFactors = FALSE, sep = "\t") {
  # Convert file path to absolute path
  full_path <- normalizePath(file_path, mustWork = TRUE)

  # Try reading the file
  tryCatch({
    data <- read.delim(file = full_path, sep = sep, header = header, stringsAsFactors = stringsAsFactors)
    message("File successfully read from: ", full_path)
    return(data)
  }, error = function(e) {
    stop("An error occurred while reading the file: ", e$message)
  })
}
