#' Create a mapping table between original and reverse-coded variables
#'
#' @param data A data.frame of class 'revcoded_df'
#'
#' @return A data.frame with original and reverse-coded variable name pairs
#' @export
#'
#' @examples
#' df <- data.frame(A = 1:5, B = 5:1)
#' res <- rev_coding_rep(df, 6, A, B)
#' rev_coding_map(res)
rev_coding_map <- function(data) {
  stopifnot(inherits(data, "revcoded_df"))

  reverse_vars <- names(data)[sapply(data, function(x) inherits(x, "reverse_var"))]

  mapping <- data.frame(
    original = sapply(data[reverse_vars], function(x) attr(x, "original_var")),
    reverse  = reverse_vars,
    stringsAsFactors = FALSE
  )

  return(mapping)
}
