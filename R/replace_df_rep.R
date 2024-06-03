#' replace_df_repeated adation
#'
#' @param data data
#' @param ... pattern and imputation
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Use case 1: Passing pattern-replacement string pairs directly as variable arguments
#' data <- data.frame(
#'   col1 = c("aa", "bb", "cc"),
#'   col2 = c("aa", "bb", "dd")
#' )
#'
#' print("Before Replacement:")
#' print(data)
#'
#' result1 <- replace_df_rep(data, "aa", "a1", "bb", "b1")
#' print("After Replacement (direct arguments):")
#' print(result1)
#'
#' # Use case 2: Passing as a single vector
#' changes <- c("aa", "a1", "bb", "b1")
#' result2 <- replace_df_rep(data, changes)
#' print("After Replacement (vector):")
#' print(result2)
#' #'
#'
#' }
#'
#'
#'
replace_df_rep <- function(data, ...) {
  args <- list(...)

  if (length(args) == 1 && is.vector(args[[1]])) {
    changes <- args[[1]]
  } else {
    changes <- unlist(args)
  }

  if (length(changes) %% 2 != 0) {
    stop("Arguments should be in pairs of pattern and replacement")
  }

  for (i in seq(1, length(changes), by = 2)) {
    pattern <- changes[i]
    imp <- changes[i + 1]
    data <- replace_df(data, pattern = pattern, imp = imp)
  }

  return(data)
}
