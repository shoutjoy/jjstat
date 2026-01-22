#' Extract only reverse-coded variables from a data.frame
#'
#' @param data A data.frame of class 'revcoded_df'
#' @param drop_attr Logical. If TRUE, removes attributes and classes (default = FALSE)
#'
#' @return A data.frame containing only reverse-coded variables
#' @export
#'
#' @examples
#' df <- data.frame(A = 1:5, B = 5:1)
#' res <- rev_coding_rep(df, 6, A, B)
#' rev_coding_extract_only(res)
rev_coding_extract_only <- function(data, drop_attr = FALSE) {
  stopifnot(inherits(data, "revcoded_df"))

  reverse_vars <- names(data)[sapply(data, function(x) inherits(x, "reverse_var"))]

  out <- data[reverse_vars]

  if (drop_attr) {
    out <- lapply(out, function(x) {
      class(x) <- setdiff(class(x), "reverse_var")
      attributes(x)[c("reverse", "original_var", "max_scale")] <- NULL
      x
    })
    out <- as.data.frame(out)
  }

  return(out)
}
