#' Quick View Utility for data.frame or tibble
#'
#' This function provides a convenient way to preview or fully display
#' a data.frame or tibble object in a pipe-friendly workflow.
#' By default, it returns the first \code{n} rows of the data.
#'
#' @param data A data.frame or tibble object.
#' @param n Integer. Number of rows to display when previewing. Default is 10.
#' @param type Character. One of \code{NULL}, \code{"head"}, or \code{"all"}.
#'   If NULL (default), the first \code{n} rows are returned.
#'   If \code{"all"}, the full data.frame is returned.
#'   If \code{"head"}, the first \code{n} rows are returned explicitly.
#'
#' @return A data.frame.
#'
#' @details
#' This function is designed as a lightweight alternative to \code{View()},
#' optimized for console-based inspection and pipe  workflows.
#' The default behavior (type = NULL) prioritizes safe data previewing.
#'
#' @export
#'
#' @examples
#' ## Example 1: Basic preview (default n = 10)
#' dall(mtcars)
#'
#' ## Example 2: Preview first 5 rows
#' dall(mtcars, n = 5)
#'
#' ## Example 3: Explicit head option
#' dall(mtcars, type = "head", n = 3)
#'
#' ## Example 4: Return full data
#' dall(mtcars, type = "all")
#'
#' ## Example 5: Pipe-friendly usage
#' library(dplyr)
#' mtcars %>%
#'   dplyr::select(mpg, cyl, hp) %>%
#'   dall(n = 6)
#'
dall <- function(data, n = 10, type = NULL){

  # 1. Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data.frame or tibble")
  }

  df <- as.data.frame(data)

  # 2. Default behavior: preview
  if (is.null(type)) {
    return(utils::head(df, n))
  }

  # 3. Explicit type handling
  if (!type %in% c("all", "head")) {
    stop("type must be NULL, 'all', or 'head'")
  }

  if (type == "all") {
    return(df)
  }

  utils::head(df, n)
}

#'
#' #' view data.frame
#' #'
#' #' @param data tibble
#' #' @param type all, head
#' #' @param n  rows n = 10 (default)
#' #'
#' #' @return data.frame
#' #' @export
#' #'
#'
#' dall= function(data, type ="all", n=10){
#'
#' all =  data %>% data.frame()
#' head = data %>% data.frame() %>% head(n)
#'
#' switch(type, all= all, head = head )
#'
#' }
#'

#'
#' #' Print all
#' #'
#' #' @param data tibble
#' #' @param n rows n = Inf
#' #'
#' #' @return view all
#' #' @export
#' #'
#'
#' pall= function(data, n=Inf){
#'   data %>%print(n=n)
#' }
