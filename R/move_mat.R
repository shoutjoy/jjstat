#' move_mat matrix element coord position
#'
#' @param data mat
#' @param ... coord
#' @param imp NA
#' @param replace  FALSE
#'
#' @return mat
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # example
#' data1 <- matrix(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "D_P3",
#'                   NA, NA, NA, NA, NA, NA, "진로태도", NA, NA, "D_P2",
#'                   NA, NA, NA, NA, NA, NA, NA, NA, NA, "D_P1",
#'                   "C_S5", NA, NA, NA, NA, NA, NA, NA, NA, "B14",
#'                   "C_S4", NA, NA, NA, NA, NA, NA, NA, NA, "B10",
#'                   "C_S3", NA, "자기효능감", NA, NA, NA, "진로준비", NA, NA, "B04",
#'                   "C_S2", NA, NA, NA, NA, NA, NA, NA, NA, "B02"))
#'
#' # Test with replace = FALSE
#' move_mat(data, list(10, 10, 1, 2), list(9, 10, 1, 3), list(8, 10, 6,3), replace = FALSE)
#' move_mat(data, c(10, 10, 1, 2), c(9, 10, 1, 3), c(8, 10, 6,3), replace = FALSE)
#'
#'
#' # Test with replace = TRUE
#' move_mat(data, list(10, 10, 1, 2), list(9, 10, 1, 3), list(8, 10, 6,3), replace = TRUE)
#' move_mat(data, c(10, 10, 1, 2), c(9, 10, 1, 3), c(8, 10, 6,3), replace = TRUE)
#' }
#'
#'
move_mat <- function(data, ..., imp = NA, replace = FALSE) {
  positions <- list(...)

  for (pos in positions) {
    old_row <- pos[[1]]
    old_col <- pos[[2]]
    new_row <- pos[[3]]
    new_col <- pos[[4]]

    # Get the element at the old position
    element <- data[old_row, old_col]

    if (is.na(element)) {
      next # Skip if the element is NA
    }

    # Check if replace is FALSE and the new position is not NA
    if (!replace && !is.na(data[new_row, new_col])) {
      next # Skip if the new position is not NA
    }

    # Move the element to the new position
    data[new_row, new_col] <- element

    # Replace the old position with imp
    data[old_row, old_col] <- imp
  }

  return(data)
}
