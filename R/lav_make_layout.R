#' Create a Custom Layout Matrix
#'
#' @param nc Integer. Number of columns in the layout matrix. Default is 10.
#' @param nr Integer or NULL. Number of rows in the layout matrix. If NULL, the number of rows will be automatically determined. Default is NULL.
#'
#' @return A matrix representing the layout, optionally edited interactively using the `edit` function.
#' @export
#'
#' @examples
#' \dontrun{
#' make_layout(nc = 7)
#' make_layout(nc = 7, nr = 6)
#' make_layout(3, 6)
#' }
make_layout <- function(nc = 10, nr = NULL) {
  if (is.null(nr)) {
    layout <- matrix(rep("", nc^2), ncol = nc)
  } else {
    layout <- matrix(rep("", nc * nr), ncol = nc, nrow = nr)
  }

  rownames(layout) <- paste0("row", 1:nrow(layout))
  layout <- edit(layout)
  return(layout)
}
