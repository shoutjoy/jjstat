#' move_matpos is element change position
#'
#' @param data mat
#' @param ... position var
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
#' # # Original matrix
#' layout_jut <- matrix(
#'   c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 'D_P3',
#'     NA, NA, NA, NA, NA, NA, '진로태도', NA, NA, 'D_P2',
#'     NA, NA, NA, NA, NA, NA, NA, NA, NA, 'D_P1',
#'     'C_S5', NA, NA, NA, NA, NA, NA, NA, NA, 'B14',
#'     'C_S4', NA, NA, NA, NA, NA, NA, NA, NA, 'B10',
#'     'C_S3', NA, '자기효능감', NA, NA, NA, '진로준비', NA, NA, 'B04',
#'     'C_S2', NA, NA, NA, NA, NA, NA, NA, NA, 'B02',
#'     'C_S1', NA, NA, NA, NA, NA, NA, NA, NA, 'A_M3',
#'     NA, NA, NA, NA, NA, NA, NA, NA, NA, 'A_M2',
#'     NA, NA, NA, NA, NA, NA, '진로동기', NA, NA, 'A_M1'),
#'   nrow = 10, ncol = 10, byrow = TRUE)
#' layout_jut
#' # Apply the matpos function
#' layout_jut%>% move_matpos(
#'   list("진로태도", 3, 5),
#'   list("진로동기", 8, 5),
#'   list("B02", 1, 3),
#'   list("B04", 1, 5),
#'   list("B10", 1, 7),
#'   list("B14", 1, 9),
#'   list("D_P3", 4, 10),
#'   list("D_P2", 6, 10),
#'   list("D_P1", 8, 10),
#'   list("A_M1", 10, 3),
#'   list("A_M2", 10, 4),
#'   list("A_M3", 10, 5),
#'   replace = FALSE
#' )
#'
#'
#' matrix(
#'   c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 'D_P3',
#'     NA, NA, NA, NA, NA, NA, '진로태도', NA, NA, 'D_P2',
#'     NA, NA, NA, NA, NA, NA, NA, NA, NA, 'D_P1',
#'     'C_S5', NA, NA, NA, NA, NA, NA, NA, NA, 'B14',
#'     'C_S4', NA, NA, NA, NA, NA, NA, NA, NA, 'B10',
#'     'C_S3', NA, '자기효능감', NA, NA, NA, '진로준비', NA, NA, 'B04',
#'     'C_S2', NA, NA, NA, NA, NA, NA, NA, NA, 'B02',
#'     'C_S1', NA, NA, NA, NA, NA, NA, NA, NA, 'A_M3',
#'     NA, NA, NA, NA, NA, NA, NA, NA, NA, 'A_M2',
#'     NA, NA, NA, NA, NA, NA, '진로동기', NA, NA, 'A_M1'),
#'   nrow = 10, ncol = 10, byrow = TRUE)%>% move_matpos(
#'     list("진로태도", 3, 5),
#'     list("진로동기", 8, 5),
#'     list("B02", 1, 3),
#'     list("B04", 1, 5),
#'     list("B10", 1, 7),
#'     list("B14", 1, 9),
#'     list("D_P3", 4, 10),
#'     list("D_P2", 6, 10),
#'     list("D_P1", 8, 10),
#'     list("A_M1", 10, 3),
#'     list("A_M2", 10, 4),
#'     list("A_M3", 10, 5),
#'     replace = FALSE
#'   )
#'
#' }
#'
#'
move_matpos <- function(data, ..., imp = NA, replace = FALSE) {
  positions <- list(...)

  for (pos in positions) {
    element <- pos[[1]]
    new_row <- pos[[2]]
    new_col <- pos[[3]]

    # Find the position of the element in the matrix
    old_pos <- which(data == element, arr.ind = TRUE)

    if (nrow(old_pos) == 0) {
      warning(paste("Element", element, "not found in the matrix"))
      next
    }

    old_row <- old_pos[1, 1]
    old_col <- old_pos[1, 2]

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
