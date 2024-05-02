
#' lower pull matrix
#'
#' @param mat matrix data
#' @param fill matrix data fill upper 0 or if that's what you want , enter it
#' @param diag matrix data diag enter 0
#'
#' @return lowermatrix, upper 0
#' @export
#'
#'
lowerMat <- function(mat, fill = 0, diag = 0,
                     digits = 3) {
  num_rows <- nrow(mat)
  num_cols <- ncol(mat)

if(is.numeric(mat)){
  mat<- round(mat, digits = digits)
}


  if (num_rows != num_cols) {
    stop("Input matrix is not square.")
  }

  for (i in 1:num_rows) {
    for (j in 1:num_cols) {
      if (i == j) {
        mat[i, j] <- diag
      } else if (i < j) {
        mat[i, j] <- fill
      }
    }
  }

  return(mat)
}


#' lower pull matrix fill NA
#'
#' @param mat matrix data
#' @param fill matrix data NA or .
#' @param diag matrix  diagonal data NA or .
#'
#' @return lowermatrix, upper 0
#' @export
#'
#'
lowerMat_na <- function(mat, fill='', diag = 1,
                        numeric = FALSE, digits = 3,
                        justify ="right") {
  num_rows <- nrow(mat)
  num_cols <- ncol(mat)
  mat<- round(mat, digits = digits)

  if (num_rows != num_cols) {
    stop("Input matrix is not square.")
  }

  for (i in 1:num_rows) {
    for (j in 1:num_cols) {
      if (i == j) {
        mat[i, j] <- diag
      } else if (i < j) {
        mat[i, j] <- fill
      }
    }
  }

if(numeric){
  mat <- apply(mat, 2, function(x) as.numeric(gsub("'", "", x)))
}

mat <- format(mat, digist = digits, justify = justify)
  return(mat)
}



#' df or mat lower tri
#'
#' @param data data
#'
#' @return
#' @export
#'

lowerdf <- function(data) {
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (j > i) {
        data[i, j] <- ""
      }
    }
  }
  return(data)
}
