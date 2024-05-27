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




