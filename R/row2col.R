#' rownames to column
#'
#' @param data data.fram
#' @param colname new column default rownames
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mtcars %>%row2col("car")
#' }
#'
row2col <- function(data, colname="rownames") {
  #
  first_col =  rownames(data)
  res = dplyr::bind_cols(rowname = first_col, data)

  colnames(res)[1] <- colname
  # shift rownames thes rownames remove
  rownames(res)<-NULL
  res
}

#' rownames to columns
#'
#' @param data data.fram
#' @param colname new column default rownames
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mtcars %>%row2cols("car")
#' }
#'
row2cols <- function(data, colname="rows") {
  # data <- as.data.frame(data)

  col1 = rownames(data)
  data <- cbind(paths = col1, data)
  colnames(data)[1] <- colname
  rownames(data) <- NULL
  return(data)
}
