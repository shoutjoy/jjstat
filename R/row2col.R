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
