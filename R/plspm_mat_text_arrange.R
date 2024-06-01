#' mat_text_arrange
#'
#'
#' @param mat matrix data
#' @param text TRUE '' FALSE number
#' @param byrow FALSE, byrow =TRUE fill in row
#'
#' @return  text
#' @export
#'
#' @examples
#' \dontrun{
#' data(offense)
#' offense %>%str()
#'
#' nfl_path = matrix(
#'   c(0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0,
#'     0, 1, 1, 0, 0,
#'     1, 0, 0, 1, 0),
#'   nrow = 5, ncol = 5, byrow = TRUE )
#'
#' rownames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' colnames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
#' nfl_path
#'
#' nfl_blocks = list(7:8, 1:3, 4:6,1:6, 9:11)
#'
#' plspm_blocks_match(data=offense, blocks=nfl_blocks,
#'                    paths=nfl_path)
#'
#' nfl_mode =c("B","A","A","A","A")
#'
#' nfl_pls = plspm(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#' nfl_pls%>%summary()
#'
#' #########
#' nfl_pls_boot = plspm_sem(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
#'
#' #Layouts extracted from mockups
#' plspm_semPaths2(nfl_pls_boot,sizeLat = 6)%>%
#'   plspm_factor_layout()
#'
#' #Make it input material
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%layout_mat(text=T)
#'
#' #Organizing materials for input
#' plspm_semPaths2(nfl_pls_boot,sizeLat =6)%>%
#'   plspm_factor_layout() %>%layout_mat()%>%
#'   mat_text_arrange()
#' }
#'
mat_text_arrange <-  function(mat, text = TRUE, byrow = FALSE) {
  # Convert the matrix to a vector based on byrow parameter
  if (byrow) {
    mat_vector <- as.vector(t(mat))
  } else {
    mat_vector <- as.vector(mat)
  }

  # Create a formatted string
  if (text) {
    formatted_string <- paste0("  ",
                               paste(sapply(mat_vector,
                                            function(x) if (is.na(x)) "NA" else paste0("'", x, "'")),
                                     collapse = ", "))
  } else {
    formatted_string <- paste0("  ",
                               paste(sapply(mat_vector,
                                            function(x) if (is.na(x)) 0 else x),
                                     collapse = ", "))
  }

  # Add line breaks after every ncol elements
  ncol <- ncol(mat)
  lines <- strsplit(formatted_string, ", ")[[1]]

  formatted_lines <- sapply(seq(1, length(lines), by = ncol),
                            function(i) paste(lines[i:(i + ncol - 1)],
                                              collapse = ", "))

  # Combine the lines into a final string
  final_string <- paste0("layout_New = matrix(\n  c(",
                         paste(formatted_lines, collapse = ",\n    "),
                         "),\n    nrow = ", nrow(mat),
                         ", ncol = ", ncol(mat),
                         ", byrow = ", ifelse(byrow, "TRUE", "FALSE"), ")\n")

  return(cat(final_string))
}

# mat_text_arrange <- function(mat) {
#   # Convert the matrix to a vector
#   mat_vector <- as.vector(t(mat))
#
#   # Create a formatted string
#
#   formatted_string <- paste0("  ",
#        paste(sapply(mat_vector,
#            function(x) if (is.na(x)) "NA" else paste0("'", x, "'")),
#                   collapse = ", ")
#   )
#   # Add line breaks after every ncol elements
#   ncol <- ncol(mat)
#   lines <- strsplit(formatted_string, ", ")[[1]]
#   formatted_lines <- sapply(seq(1, length(lines), by = ncol),
#                             function(i) paste(lines[i:(i + ncol - 1)], collapse = ", "))
#
#   # Combine the lines into a final string
#   final_string <- paste0("layout_New = matrix(\n  c(",
#                          paste(formatted_lines, collapse = ",\n    "),
#                          "),\n    nrow = ", nrow(mat),
#                          ", ncol = ", ncol(mat),
#                          ", byrow = FALSE)","\n")
#
#   return(cat(final_string))
# }
