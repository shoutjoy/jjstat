#' Validate reverse coding scale (n): scale 유효성 검사
#'
#' Checks if the provided max scale (n) is valid for each variable.
#' A valid n is usually: min(x) + max(x)
#'
#' @param data A data.frame
#' @param vars Character vector or symbols: variable names
#' @param n Vector of expected scale maxima (same length as vars or length 1)
#' @param strict Logical. If TRUE, gives warning when mismatch found
#'
#' @return A data.frame with columns: var, min, max, expected_n, provided_n, match
#' @export
#'
#' @examples
#' df <- data.frame(A = 1:5, B = 5:1)
#' rev_coding_validate_scale(df, c("A", "B"), n = 6)
#' rev_coding_validate_scale(df, c("A", "B"), n = c(6, 6))
rev_coding_validate_scale <- function(data, vars, n, strict = FALSE) {
  if (!is.character(vars)) {
    vars <- sapply(substitute(list(vars))[-1], deparse)
  }

  if (length(n) == 1) {
    n <- rep(n, length(vars))
  }

  results <- data.frame(
    var = vars,
    min = sapply(vars, function(v) min(data[[v]], na.rm = TRUE)),
    max = sapply(vars, function(v) max(data[[v]], na.rm = TRUE)),
    provided_n = n
  )

  results$expected_n <- results$min + results$max
  results$match <- results$expected_n == results$provided_n

  if (strict && any(!results$match)) {
    warning("Some n values do not match min + max of variables.")
  }

  return(results)
}
