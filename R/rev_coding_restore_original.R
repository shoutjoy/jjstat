#' Remove reverse-coded variables, keeping only original variables
#'
#' @param data A data.frame of class 'revcoded_df'
#'
#' @return A data.frame without reverse-coded variables
#' @export
#'
#' @examples
#' df <- data.frame(A = 1:5, B = 5:1)
#' res <- rev_coding_rep(df, 6, A, B)
#' rev_coding_restore_original(res)
rev_coding_restore_original <- function(data) {
  stopifnot(inherits(data, "revcoded_df"))

  reverse_vars <- names(data)[sapply(data, function(x) inherits(x, "reverse_var"))]

  data[reverse_vars] <- NULL

  class(data) <- setdiff(class(data), "revcoded_df")
  return(data)
}
