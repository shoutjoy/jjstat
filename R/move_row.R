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

move_row <- function(data, from, to) {
  # Extract the row to move
  row_to_move <- data[from, , drop = FALSE]

  if (from == 1) {
    # Remove the row to move
    data <- data[-from, , drop = FALSE]

    # Split the data into "above" and "below" based on the "to" position
    above <- data[1:(to - 1), , drop = FALSE]
    below <- data[to:nrow(data), , drop = FALSE]

    # Combine the above rows, the row to move, and the below rows
    if (to == 1) {
      # If "to" is the first row, concatenate the row to move, below, and above rows
      data <- rbind(row_to_move, below, above)
    } else if (to == nrow(data) + 1) {
      # If "to" is the position after the last row, concatenate the above and below rows, and then the row to move
      data <- rbind(above, below, row_to_move)
    } else {
      # If "to" is not the first or the position after the last row, concatenate the above rows, the row to move, and the below rows
      data <- rbind(above, row_to_move, below)
    }
  } else {
    # Remove the row to move
    data <- data[-from, , drop = FALSE]

    # Insert the row to be moved at the specified position
    if (to == 1) {
      # If "to" is the first row, concatenate the row to move, below, and above rows
      data <- rbind(row_to_move, data[1:(to - 1), , drop = FALSE], data[to:nrow(data), , drop = FALSE])
    } else {
      # If "to" is not the first row, concatenate the above rows, the row to move, and the below rows
      data <- rbind(data[1:(to - 1), , drop = FALSE], row_to_move, data[to:nrow(data), , drop = FALSE])
    }
  }

  # Reset row names
  rownames(data) <- NULL

  return(data)
}


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

mr <- function(data, from, to) {
  # Extract the row to move
  row_to_move <- data[from, , drop = FALSE]

  if (from == 1) {
    # Remove the row to move
    data <- data[-from, , drop = FALSE]

    # Split the data into "above" and "below" based on the "to" position
    above <- data[1:(to - 1), , drop = FALSE]
    below <- data[to:nrow(data), , drop = FALSE]

    # Combine the above rows, the row to move, and the below rows
    if (to == 1) {
      # If "to" is the first row, concatenate the row to move, below, and above rows
      data <- rbind(row_to_move, below, above)
    } else if (to == nrow(data) + 1) {
      # If "to" is the position after the last row, concatenate the above and below rows, and then the row to move
      data <- rbind(above, below, row_to_move)
    } else {
      # If "to" is not the first or the position after the last row, concatenate the above rows, the row to move, and the below rows
      data <- rbind(above, row_to_move, below)
    }
  } else {
    # Remove the row to move
    data <- data[-from, , drop = FALSE]

    # Insert the row to be moved at the specified position
    if (to == 1) {
      # If "to" is the first row, concatenate the row to move, below, and above rows
      data <- rbind(row_to_move, data[1:(to - 1), , drop = FALSE], data[to:nrow(data), , drop = FALSE])
    } else {
      # If "to" is not the first row, concatenate the above rows, the row to move, and the below rows
      data <- rbind(data[1:(to - 1), , drop = FALSE], row_to_move, data[to:nrow(data), , drop = FALSE])
    }
  }

  # Reset row names
  rownames(data) <- NULL

  return(data)
}



#' move_row rep function
#'
#' @param data data
#' @param ... movelist c(a,b),c(c,d)...
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data <- data.frame(
#'   source = c("A05", "A08", "A04", "A06", "A09", "A11", "A13", "A02", "A03", "A01", "A14", "A15", "A07", "A12", "A10", "eigen_value", "Proportion_Var", "Cumulative_Var"),
#'   MR1 = c("", "", "", "", "", "", "", "0.362", "0.449", "0.457", "0.47", "0.538", "0.62", "0.792", "0.96", "3.363", "0.224", "0.224"),
#'   MR2 = c("", "0.382", "", "", "0.445", "0.724", "0.906", "0.392", "", "", "0.487", "", "", "", "", "2.095", "0.14", "0.364"),
#'   MR3 = c("", "", "0.402", "0.963", "", "", "", "", "", "", "", "", "", "", "", "1.691", "0.113", "0.477")
#' )
#' data
#' #'
#' move_row(data, 14,1)
#' move_row(data, 14,1)%>% move_row(1,2)
#' move_row(data, 14,1)%>% move_row(1,2)%>% move_row(2,3)
#' #'
#' data %>% move_rows(c(14, 1), c(1, 2))
#' data %>% move_rows(c(14, 1), c(1, 2), c(2, 3))
#' }
#'
move_rows <- function(data, ...) {
  moves=list(...)
  for (move in moves) {
    data <- move_row(data, move[1], move[2])
  }
  return(data)
}

