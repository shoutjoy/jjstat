#' filter and,  or function
#'
#' @param data data
#' @param op operatror or, and
#' @param ... condition
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mtcars %>% filter_rows(cyl = 6)
#' mtcars %>% filter_rows(vs = 1)
#' mtcars %>% filter_rows(cyl = 6, vs = 1, op ="and")
#' mtcars %>% filter_rows(op="and", cyl = 6, vs = 1)
#' mtcars %>% filter_rows("and", cyl = 6, vs = 1 )
#'
#' mtcars %>% filter_rows("or", cyl = 6, vs = 1 )
#' mtcars %>% filter_rows(op= "or", cyl = 6, vs = 1 )
#' mtcars %>% filter_rows(cyl = 6, vs = 1, op ="or")
#'
#' mtcars %>% filter_rows(cyl = 6, vs = 1, gear = 4, op = "and")
#' mtcars %>% filter_rows(cyl = 6, vs = 1, gear = 4, op = "or")
#' mtcars %>% filter_rows(cyl = 6, vs = 1, gear = 4, op = "or")
#'
#' mtcars %>% filter_rows(mpg > 25) #error
#' mtcars %>% filter(mpg > 25)
#' }
#'
filter_rows <- function(data, op = "or", ...) {
  # Convert conditions to lists
  conditions <- list(...)

  # Create a filter expression for a condition
  filter_expression <- sapply(names(conditions), function(col) {
    paste0("`", col, "` == ", conditions[[col]])
  })

  # Apply logical operations
  if (op == "and") {
    filter_expression <- paste(filter_expression, collapse = " & ")
  } else if (op == "or") {
    filter_expression <- paste(filter_expression, collapse = " | ")
  } else {
    stop("logical parameter should be either 'and' or 'or'")
  }

  # # Evaluating filter conditions and filtering data
  filtered_data <-  dplyr::filter(data,!!rlang::parse_expr(filter_expression))


  cat("\n",filter_expression,"\n\n")
  return(filtered_data)

}
