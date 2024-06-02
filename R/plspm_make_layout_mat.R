#' plspm_make_layout_mat
#'
#' @param ... variable
#'
#' @return mat
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' plspm_make_layout_mat(x1 = c(1, 1),
#'                       x2 = c(2, 1),
#'                       x3 = c(2, 2),
#'                       x4 = c(3, 2),
#'                       x5 = c(3, 5))
#' }
#'
#'
plspm_make_layout_mat <- function(...) {
  layout <- list(...)
  xmax <- max(sapply(layout, function(x) x[1]))
  ymax <- max(sapply(layout, function(x) x[2]))
  out <- matrix(NA, xmax, ymax)
  for (i in seq_len(length(layout))) {
    j <- layout[[i]]
    jname <- names(layout)[i]
    out[j[1], j[2]] <- jname
  }
  out
}
