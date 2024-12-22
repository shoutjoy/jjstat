#' write_text: Save R Object as Text File and Provide Read Instructions
#'
#' @param data Any R object to be saved as a text file.
#' @param file_path Optional. A string specifying the file path where the text file will be saved.
#'        If not provided, the file name is derived from the object's name with ".text" extension.
#' @param folder_open Logical indicating whether to open the folder containing the saved file. Defaults to TRUE.
#'
#' @return NULL. The function saves a text file and optionally opens the folder where it is saved.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Save a summary of a linear model
#' lmres <- lm(mpg ~ hp + wt, data = mtcars) %>% summary()
#' write_text(lmres)
#' #'
#' #'
#' lmres = lm(mpg~ hp+wt, data=mtcars) %>% summary()
#' write_text(lmres)
#'
#' # resultlm <- read_text("F:/Rwork/07 futureDuck/2024FEI/lmres.text")
#' # resultlm
#' resultlm <- readLines("F:/Rwork/07 futureDuck/2024FEI/lmres.text")
#'
#'
#' }
write_text <- function(data, file_path = NULL, folder_open = TRUE) {
  # Get the name of the input object
  object_name <- deparse(substitute(data))

  # If file_path is not provided, use the object's name with ".text" extension
  if (is.null(file_path)) {
    file_path <- paste0(object_name, ".text")
  }

  # Convert file path to absolute path
  full_path <- normalizePath(file_path, mustWork = FALSE)

  # Escape backslashes for compatibility in output messages
  escaped_path <- gsub("\\\\", "/", full_path)

  # Save the object as text
  tryCatch({
    capture.output(data, file = full_path)
    message("File successfully saved to: ", full_path)

    # Provide usage instructions for reading the text
    cat("Use the following code to load the text file into R:\n")
    cat('result <- readLines("', escaped_path, '")\n', sep = "")
  }, error = function(e) {
    stop("An error occurred while writing the file: ", e$message)
  })

  # Open the folder if folder_open is TRUE
  if (folder_open) {
    folder_path <- dirname(full_path)
    if (.Platform$OS.type == "windows") {
      shell.exec(folder_path)
    } else {
      system(paste("open", shQuote(folder_path)))
    }
  }

  return(NULL)
}
