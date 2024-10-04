#' move_col
#'
#' @param data df
#' @param from from
#' @param to to
#' @param history histor show text
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 사용 예시
#' df <- data.frame(
#'   A = 1:5,
#'   B = 6:10,
#'   C = 11:15,
#'   D = 16:20
#' )
#'
#' # 2번째 열을 4번째 위치로 이동
#' result_df <- move_col(df, from = 2, to = 4, history = TRUE)
#' print(result_df)
#'
#' }
#'
move_col <- function(data, from, to, history=FALSE) {
  # Extract the column to move
  col_to_move <- data[[from]]
  col_name <- names(data)[from]

  # Remove the column from the original position
  data <- data[ , -from, drop = FALSE]

  # Insert the column at the new position
  if (to <= 1) {
    data <- cbind(col_to_move, data)
    names(data)[1] <- col_name
  } else if (to >= ncol(data) + 1) {
    data <- cbind(data, col_to_move)
    names(data)[ncol(data)] <- col_name
  } else {
    data <- cbind(data[ , 1:(to - 1), drop = FALSE], col_to_move, data[ , to:ncol(data), drop = FALSE])
    names(data)[to] <- col_name
  }

  if(history){
    cat(" from_col:", paste0(from, " ->  position to: ", to), "\n")
  }

  return(data)
}


#' move_cols replication
#'
#' @param data data frame
#' @param ...  from to, from to
#' @param history history show text
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 사용 예시
#' df <- data.frame(
#'   A = 1:5,
#'   B = 6:10,
#'   C = 11:15,
#'   D = 16:20
#' )
#'
#' # 여러 열을 이동: 2번째 열을 4번째로, 3번째 열을 1번째로 이동
#' result_df <- move_cols(df, 2, 4, 3, 1, history = TRUE)
#' print(result_df)
#'
#' }
#'
move_cols <- function(data, ..., history=TRUE) {
  # Capture all the arguments passed
  args <- list(...)

  if(length(args) == 0) {
    cat("\n To change the location of your columns, enter a from column and a to column!
     move_cols(1, 3) -> from 1st column to 3rd column position
     \n\n")
    return(data)
  }

  # Determine the input style: list of pairs or sequence of values
  if (is.list(args) && length(args[[1]]) == 2) {
    moves <- args
    if(history){
      for (move in moves) {
        data <- move_col(data, move[[1]], move[[2]], history=TRUE)
      }
    } else {
      for (move in moves) {
        data <- move_col(data, move[[1]], move[[2]])
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
          data <- move_col(data, move[[1]], move[[2]][1], history=TRUE)
        } else {
          data <- move_col(data, move[[1]], move[[2]], history=TRUE)
        }
      }
    } else {
      for (move in moves) {
        if (length(move[[2]]) > 1) {
          data <- move_col(data, move[[1]], move[[2]][1])
        } else {
          data <- move_col(data, move[[1]], move[[2]])
        }
      }
    }
  }
  # Return the modified data
  return(data)
}
