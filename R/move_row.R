#' Functions to move rows to a desired position
#'
#' @param data data.fram
#' @param from row from
#' @param to row to
#' @param history history
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #' #'
#' #' # Example data
#' car = data.frame(
#'   rownames = c(paste0("Row_",1:6)),
#'   mpg = c('21', '21', '22.8', '21.4', '18.7', '18.1'),
#'   cyl = c('6', '6', '4', '6', '8', '6'),
#'   disp = c('160', '160', '108', '258', '360', '225')
#' )
#'
#' car
#' car%>%move_row(1,6)
#' car%>%move_row(6,1)
#' car%>%move_row(6,2)
#' car%>%move_row(6,3)
#' car%>%move_row(1,3)
#' car%>%move_row(6,5)
#'
#' car
#' car%>%
#'   move_row(6,1)%>%
#'   move_row(1,3)%>%
#'   move_row(3,2)
#' #'
#' }
#'
#'
move_row <- function(data, from, to, history=FALSE) {
  # Extract the row to move
  row_to_move <- data[from, , drop = FALSE]

  # Remove the row from the original position
  data <- data[-from, , drop = FALSE]

  # Insert the row at the new position
  if (to <= 1) {
    data <- rbind(row_to_move, data)
  } else if (to >= nrow(data) + 1) {
    data <- rbind(data, row_to_move)
  } else {
    data <- rbind(data[1:(to - 1), , drop = FALSE],
                  row_to_move, data[to:nrow(data),
                                    , drop = FALSE])
  }
  # Reset row names
  rownames(data) <- NULL

  if(history){
    cat(" from_row:",paste0(from," ->  position to: ",to),"\n")
  }

  return(data)
}
#' Functions to move rows to a desired position
#'
#' @param data data.fram
#' @param from row from
#' @param to row to
#' @param history history
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' #' # Example data
#' car = data.frame(
#'   rownames = c(paste0("Row_",1:6)),
#'   mpg = c('21', '21', '22.8', '21.4', '18.7', '18.1'),
#'   cyl = c('6', '6', '4', '6', '8', '6'),
#'   disp = c('160', '160', '108', '258', '360', '225')
#' )
#' car
#' car %>% mr(1,6)
#' car %>% mr(6,1)
#' car %>% mr(6,2)
#' car %>% mr(6,3)
#' car %>% mr(1,3)
#' car %>% mr(6,5)
#'
#' #history
#' car %>%
#'   mr(6,1)%>%
#'   mr(1,3)%>%
#'   mr(3,2)
#' #'
#' }
#'
#'
mr <- function(data, from, to, history=FALSE) {
  # Extract the row to move
  row_to_move <- data[from, , drop = FALSE]

  # Remove the row from the original position
  data <- data[-from, , drop = FALSE]

  # Insert the row at the new position
  if (to <= 1) {
    data <- rbind(row_to_move, data)
  } else if (to >= nrow(data) + 1) {
    data <- rbind(data, row_to_move)
  } else {
    data <- rbind(data[1:(to - 1), , drop = FALSE],
                  row_to_move, data[to:nrow(data),
                                    , drop = FALSE])
  }
  # Reset row names
  rownames(data) <- NULL

  if(history){
    cat(" from_row:",paste0(from," ->  position to: ",to),"\n")
  }

  return(data)
}
#'


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
#' #'
#' car = data.frame(
#'   rownames = c(paste0("Row_",1:6)),
#'   mpg = c('21', '21', '22.8', '21.4', '18.7', '18.1'),
#'   cyl = c('6', '6', '4', '6', '8', '6'),
#'   disp = c('160', '160', '108', '258', '360', '225'))
#' cars
#'
#' #repeated
#' cars
#' car %>%
#'   move_row(6,1)%>%
#'   move_row(1,3)%>%
#'   move_row(3,2)
#'
#' #history
#' car %>%
#'   move_row(6,1, history=TRUE)%>%
#'   move_row(1,3, history=TRUE)%>%
#'   move_row(3,2, history=TRUE)%>%
#'   move_row(2,6, history=TRUE)
#'
#' car%>%move_rows(6,1) #errorcheck
#' car%>%move_rows(c(6,1), c(1,2),c(2,6))
#' car%>%move_rows(c(6,1), c(1,2),c(2,6), history=T)
#'
#' # simple input method
#' move_rows(car, 6, 1, 1, 2, 2, 6, history=TRUE)
#' car%>% move_rows( 6, 1, 1, 2, 2, 6, 6,3, history=TRUE)
#' # Testing the function with direct pair inputs
#' move_rows(car, 6, 1, 1, 2, 2, 6, history=TRUE)
#'
#' # Testing the function with list of pairs inputs
#' move_rows(car, c(6, 1), c(1, 2), c(2, 6), history=TRUE)
#'
#'
#' # Testing the function with direct pair inputs
#' move_rows2(car, 6, 1,  history=FALSE)
#' move_rows2(car, 6, 1,1,2)
#' move_rows2(car, 6, 1, 1, 2, 2, 6, history=TRUE)
#' car%>%move_rows2(6, 1, 1, 2, 2, 6, history=TRUE)
#' move_rows2(car, 6, 1, 1, 2, 2, 6, 6,3, history=TRUE)
#' car%>%move_rows2(c(6,1), c(1,2),c(2,6),c(6,5), history=T)
#' car%>%move_rows2(6, 1, 1, 2, 2, 6, 6,3, history=T)
#'
#'
#' # Testing the function with list of pairs inputs
#' move_rows2(car, c(6, 1), history=TRUE)
#' move_rows(car, c(6, 1),c(1,2), history=TRUE)
#' move_rows2(car, c(6, 1), c(1, 2), c(2, 6),c(6,3), history=TRUE)
#' #'
#' #'
#'
#'
#'
#' }
#'

move_rows <- function(data, ..., history=TRUE) {

  if(length(args0) == 0) {
    cat("\n To change the location of your data, enter a from line and a to line!
     move_rows(1, 3) -> from 1st row  to 3rd row position
     \n\n")
    return(data)
  }

  # Capture all the arguments passed
  args <- list(...)

  # Determine the input style: list of pairs or sequence of values
  if (is.list(args) && length(args[[1]]) == 2) {
    moves <- args
    #siigle mode
    if(history){

      for (move in moves) {
        data <- move_row(data, move[1], move[2], history=TRUE)
      }
    }else{
      for (move in moves) {
        data <- move_row(data, move[1], move[2])
      }
    }

    return(data)

  } else {
    # Otherwise, assume that arguments are provided as a sequence of pairs
    moves <- split(args, ceiling(seq_along(args) / 2))

    # Process each move
    if (history) {
      for (move in moves) {
        if (length(move[[2]]) > 1) {
          data <- move_row(data, move[[1]], move[[2]][1], history=TRUE)
        } else {
          data <- move_row(data, move[[1]], move[[2]], history=TRUE)
        }
      }
    } else {
      for (move in moves) {
        if (length(move[[2]]) > 1) {
          data <- move_row(data, move[[1]], move[[2]][1])
        } else {
          data <- move_row(data, move[[1]], move[[2]])
        }
      }
    }
  }
  #result
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
#' #'
#' car = data.frame(
#'   rownames = c(paste0("Row_",1:6)),
#'   mpg = c('21', '21', '22.8', '21.4', '18.7', '18.1'),
#'   cyl = c('6', '6', '4', '6', '8', '6'),
#'   disp = c('160', '160', '108', '258', '360', '225'))
#' cars
#'
#' #repeated
#' cars
#' car %>%
#'   move_row(6,1)%>%
#'   move_row(1,3)%>%
#'   move_row(3,2)
#'
#' #history
#' car %>%
#'   move_row(6,1, history=TRUE)%>%
#'   move_row(1,3, history=TRUE)%>%
#'   move_row(3,2, history=TRUE)%>%
#'   move_row(2,6, history=TRUE)
#'
#' car%>%move_rows(6,1) #errorcheck
#' car%>%move_rows(c(6,1), c(1,2),c(2,6))
#' car%>%move_rows(c(6,1), c(1,2),c(2,6), history=T)
#'
#' # simple input method
#' move_rows(car, 6, 1, 1, 2, 2, 6, history=TRUE)
#' car%>% move_rows( 6, 1, 1, 2, 2, 6, 6,3, history=TRUE)
#' # Testing the function with direct pair inputs
#' move_rows(car, 6, 1, 1, 2, 2, 6, history=TRUE)
#'
#' # Testing the function with list of pairs inputs
#' move_rows(car, c(6, 1), c(1, 2), c(2, 6), history=TRUE)
#'
#'
#' # Testing the function with direct pair inputs
#' move_rows2(car, 6, 1,  history=FALSE)
#' move_rows2(car, 6, 1,1,2)
#' move_rows2(car, 6, 1, 1, 2, 2, 6, history=TRUE)
#' car%>%move_rows2(6, 1, 1, 2, 2, 6, history=TRUE)
#' move_rows2(car, 6, 1, 1, 2, 2, 6, 6,3, history=TRUE)
#' car%>%move_rows2(c(6,1), c(1,2),c(2,6),c(6,5), history=T)
#' car%>%move_rows2(6, 1, 1, 2, 2, 6, 6,3, history=T)
#'
#'
#' # Testing the function with list of pairs inputs
#' move_rows2(car, c(6, 1), history=TRUE)
#' move_rows(car, c(6, 1),c(1,2), history=TRUE)
#' move_rows2(car, c(6, 1), c(1, 2), c(2, 6),c(6,3), history=TRUE)
#' #'
#' #'
#'
#'
#'
#' }
#'
mrs <- function(data, ..., history=TRUE) {
  # Capture all the arguments passed
  args <- list(...)

  # Determine the input style: list of pairs or sequence of values
  if (is.list(args) && length(args[[1]]) == 2) {
    moves <- args
    #siigle mode
    if(history){

      for (move in moves) {
        data <- move_row(data, move[1], move[2], history=TRUE)
      }
    }else{
      for (move in moves) {
        data <- move_row(data, move[1], move[2])
      }
    }

    return(data)

  } else {
    # Otherwise, assume that arguments are provided as a sequence of pairs
    moves <- split(args, ceiling(seq_along(args) / 2))

    # Process each move
    if (history) {
      for (move in moves) {
        if (length(move[[2]]) > 1) {
          data <- move_row(data, move[[1]], move[[2]][1], history=TRUE)
        } else {
          data <- move_row(data, move[[1]], move[[2]], history=TRUE)
        }
      }
    } else {
      for (move in moves) {
        if (length(move[[2]]) > 1) {
          data <- move_row(data, move[[1]], move[[2]][1])
        } else {
          data <- move_row(data, move[[1]], move[[2]])
        }
      }
    }
  }
  #result
  return(data)
}
