#' plspm_paths2lav
#'
#' @param path_matrix matrix paths
#' @param name default FALSE, TRUE : paths_model=
#'
#' @return lavaan syntax
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' #' # Example path_matrix
#' path_matrix <- matrix(c(0, 1, 0, 0, 1, 1,
#'                         0, 0, 1, 1, 1, 0,
#'                         0, 0, 0, 1, 1, 0,
#'                         0, 0, 0, 0, 1, 0,
#'                         0, 0, 0, 0, 0, 1,
#'                         0, 0, 0, 0, 0, 0),
#'                       nrow = 6, ncol = 6, byrow = FALSE)
#'
#' rownames(path_matrix) <- c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")
#' colnames(path_matrix) <- c("IMAG", "EXPE", "QUAL", "VAL", "SAT", "LOY")
#'
#' cat(plspm_paths2lav(path_matrix))
#' #'
#' }
#'
plspm_paths2lav <- function(path_matrix, name=FALSE) {
  # Initialize an empty string at the beginning
  if(name){
    paths_model <- "paths_model= \"\n"
  }else{
    paths_model <- ""

  }
  #transpose
  path_matrix<- t(path_matrix)
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

  paths_models = paths_model
  # Adding double quotes and line breaks to the last line
  if(name){
    paths_models <- paste0(paths_models, " \" " )
  }else{
    paths_models <- paste0(paths_models, "")
  }

  # Output the results
  return(paths_model)
}
