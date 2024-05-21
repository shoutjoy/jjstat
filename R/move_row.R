#' Functions to move rows to a desired position
#'
#' @param data data.fram
#' @param insert select row to position
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example data
#' data <- data.frame(
#'   source = c("A05", "A08", "A04", "A06", "A09", "A11", "A13", "A02", "A03", "A01", "A14", "A15", "A07", "A12", "A10", "eigen_value", "Proportion_Var", "Cumulative_Var"),
#'   MR1 = c("", "", "", "", "", "", "", "0.362", "0.449", "0.457", "0.47", "0.538", "0.62", "0.792", "0.96", "3.363", "0.224", "0.224"),
#'   MR2 = c("", "0.382", "", "", "0.445", "0.724", "0.906", "0.392", "", "", "0.487", "", "", "", "", "2.095", "0.14", "0.364"),
#'   MR3 = c("", "", "0.402", "0.963", "", "", "", "", "", "", "", "", "", "", "", "1.691", "0.113", "0.477")
#' )
#'
#' #
#' new_position <- c(4, 2)  # Move row 4 to row 2
#' moved_data <- move_row(data, new_position)
#' print(moved_data)
#'
#' }
#'
#'
move_row <- function(data, insert) {
  # Extract rows to move and where they will be moved
  select_row <- insert[1]
  to <- insert[2]

  # Extract rows to move
  row_to_move <- data[select_row, ]

  # Move rows down one space from the position to be moved
  data <- rbind(data[1:(to-1), ], data[to:nrow(data), ])

  # Insert the row to be moved at the location where it will be moved
  data[to, ] <- row_to_move

  # Remove the original position of the moved row
  data <- data[-select_row, ]
  return(data)
}


