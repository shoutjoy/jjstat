#' lavaan syntax to plspm path matrix
#'
#' @param model_syntax lavaan syntax
#' @param fct_order fct_order is reorder option for rownamnes of matrix
#'
#' @return matrix
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # Example usage
#' model1 <- "
#' EXPE ~ IMAG; QUAL ~ EXPE
#' VAL ~ QUAL + EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "
#'
#'  #or
#' model1 = "
#' EXPE ~ IMAG
#' QUAL ~ EXPE
#' VAL ~  QUAL+ EXPE
#' SAT ~ IMAG + EXPE + QUAL + VAL
#' LOY ~ SAT + IMAG
#' "

#' # Generate the matrix
#' plspm_lav2paths(model1)
#'
#' plspm_lav2paths(model1, fct_order=c("IMAG", "EXPE","QUAL", "VAL", "SAT", "LOY" ))
#'
#'
#' }
#'
plspm_lav2paths <- function(model_syntax, fct_order = NULL) {
  # Split the model syntax into individual lines and remove empty lines
  lines <- unlist(strsplit(model_syntax, "\\s*;\\s*|\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  # Extract variable names
  all_vars <- unique(unlist(strsplit(lines, "[~+\\s]+")))
  all_vars <- trimws(all_vars)
  all_vars <- all_vars[all_vars != ""]
  #add unique check
  all_vars = unique(all_vars)

  # Reorder variable names if fct_order is provided
  if (!is.null(fct_order)) {
    all_vars <- fct_order
  }

  # Initialize the matrix
  n <- length(all_vars)
  path_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(all_vars, all_vars))

  # Fill the matrix based on the model syntax
  for (line in lines) {
    parts <- unlist(strsplit(line, "~"))
    dependent <- trimws(parts[1])
    independents <- unlist(strsplit(parts[2], "\\+"))
    independents <- trimws(independents)

    for (indep in independents) {
      path_matrix[indep, dependent] <- 1
    }
  }

  return(t(path_matrix))
}



#' plspm_paths2lav
#'
#' @param path_matrix matrix paths
#'
#' @return lavaan syntax
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # Example path_matrix
#' path_matrix <- matrix(c(0, 1, 0, 0, 1, 1,
#'                         0, 0, 1, 1, 1, 0,
#'                         0, 0, 0, 1, 1, 0,
#'                         0, 0, 0, 0, 1, 0,
#'                         0, 0, 0, 0, 0, 1,
#'                         0, 0, 0, 0, 0, 0),
#'                       nrow = 6, byrow = TRUE)
#' rownames(path_matrix) <- c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")
#' colnames(path_matrix) <- c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")
#'
#' # Example of running a function
#' cat(plspm_paths2lav(path_matrix))
#' #'
#' }
#'
plspm_paths2lav <- function(path_matrix) {
  # Initialize an empty string at the beginning
  paths_model <- "paths_model= \"\n"

  # Get the column and row names of the path_matrix
  cols <- colnames(path_matrix)
  rows <- rownames(path_matrix)

  # Traversing the path_matrix to identify dependent and independent variables
  for (i in seq_along(cols)) {
    dependent <- cols[i]
    independent_vars <- rows[path_matrix[, i] == 1]

    if (length(independent_vars) > 0) {
      # Connect independent variables with a "+"
      independents <- paste(independent_vars, collapse = " + ")
      # Concatenate dependent and independent variables to create a single expression
      paths_model <- paste0(paths_model, dependent, " ~ ", independents, "\n")
    }
  }

  # Adding double quotes and line breaks to the last line
  paths_model <- paste0(paths_model, "\"")

  # Output the results
  return(paths_model)
}

