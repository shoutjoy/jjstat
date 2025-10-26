#' colnames_by_dict: Rename only column names using a dictionary
#'
#' This function renames the column names of a data frame using a dictionary vector.
#'
#' @param df A data.frame or tibble whose column names you want to rename.
#' @param dict A character vector of key-value pairs.
#'   It can be either:
#'   - Named vector: c("old1" = "new1", "old2" = "new2")
#'   - Unnamed pairwise vector: c("old1", "new1", "old2", "new2")
#'
#' @return A data.frame with renamed column names.
#'
#' @export
#'
#' @examples
#' var_named <- c(
#'   "access","연구접근성",
#'   "barrier","진로장벽",
#'   "efficacy","연구효능감",
#'   "interest","진로흥미",
#'   "outcome","결과기대"
#' )
#'
#' df <- data.frame(access = 1:2, barrier = 3:4, outcome = 5:6)
#' colnames_by_dict(df, var_named)
colnames_by_dict <- function(df, dict) {
  if (is.null(names(dict))) {
    dict <- stats::setNames(dict[seq(2, length(dict), by = 2)],
                            dict[seq(1, length(dict), by = 2)])
  }

  colnames(df) <- ifelse(colnames(df) %in% names(dict),
                         dict[colnames(df)],
                         colnames(df))

  return(df)
}






#' rename_by_dict: Rename both column names and cell values using a dictionary
#'
#' This function renames column names and character cell values in a data frame
#' using a named character vector as dictionary. Especially useful in pipes.
#'
#' @param df A data.frame or tibble.
#' @param dict A character vector of key-value pairs (e.g., c("eng1" = "kor1", ...)) or unnamed like c("eng1", "kor1", ...)
#'
#' @return A data.frame with renamed column names and character cell values.
#' @export
#'
#' @examples
#' df <- data.frame(latent = c("access", "barrier"), access = 1:2, barrier = 3:4)
#' var_named <- c("access", "연구접근성", "barrier", "진로장벽")
#' rename_by_dict(df, var_named)
rename_by_dict <- function(df, dict) {
  if (is.null(names(dict))) {
    dict <- stats::setNames(dict[seq(2, length(dict), by = 2)],
                            dict[seq(1, length(dict), by = 2)])
  }

  # 1. colnames
  colnames(df) <- ifelse(colnames(df) %in% names(dict),
                         dict[colnames(df)],
                         colnames(df))

  # 2. cell values (only for character/factor columns)
  for (col in names(df)) {
    if (is.character(df[[col]]) || is.factor(df[[col]])) {
      df[[col]] <- as.character(df[[col]])
      df[[col]] <- ifelse(df[[col]] %in% names(dict),
                          dict[df[[col]]],
                          df[[col]])
    }
  }

  return(df)
}

