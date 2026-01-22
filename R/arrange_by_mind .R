#' Arrange dataframe by user-defined priority on rhs column
#'
#' This function arranges a data frame according to a user-defined
#' priority of values in the \code{rhs} column. If no priority is supplied,
#' the data frame is sorted alphabetically by \code{rhs}.
#'
#' @param df A data frame containing at least a column named \code{rhs}.
#' @param priority An optional character vector specifying the preferred
#'   order of values in the \code{rhs} column. If \code{NULL}, rows are
#'   sorted alphabetically by \code{rhs}.
#'
#' @return A data frame sorted by user-defined priority and/or
#'   alphabetically by \code{rhs}.
#'
#' @details
#' When \code{priority} is provided, values of \code{rhs} that appear in
#' \code{priority} are ordered first according to that order. Remaining
#' values are placed afterward and sorted alphabetically.
#' If \code{priority} is \code{NULL}, the function simply sorts the data
#' frame alphabetically by \code{rhs}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   lhs = c("intention", "access", "efficacy"),
#'   rhs = c("barrier", "experience", "grade"),
#'   est = c(0.2, 0.4, -0.1)
#' )
#'
#' # Example 1: Alphabetical ordering only
#' arrange_by_mind(df)
#'
#' # Example 2: User-defined priority
#' arrange_by_mind(df,
#'   priority = c("experience", "grade")
#' )
#'
#' # Example 3: Partial priority specification
#' arrange_by_mind(df,
#'   priority = "experience"
#' )
#' }
arrange_by_mind <- function(df, priority = NULL) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  if (!"rhs" %in% names(df)) {
    stop("Data frame must contain a column named 'rhs'.")
  }

  # If no priority is supplied → alphabetical order only
  if (is.null(priority)) {
    return(dplyr::arrange(df, rhs))
  }

  # Apply user-defined priority
  df <- df |>
    dplyr::mutate(
      rhs_order = dplyr::case_when(
        rhs %in% priority ~ match(rhs, priority),
        TRUE ~ length(priority) + 1
      )
    ) |>
    dplyr::arrange(rhs_order, rhs) |>
    dplyr::select(-rhs_order)

  return(df)
}
