#' select_format
#'
#' @param data data
#' @param ... col selection
#' @param nsmall default 3
#' @param digits round 3
#'
#' @return data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#'
#' # Usage example
#' data2 <- data.frame(a = rnorm(5), b = rnorm(5), c = rnorm(5), d = rnorm(5), e = rnorm(5))
#' select_format(data2, 1, 2)
#' select_format(data2, 1:5)
#' select_format(data2, a)
#' select_format(data2, a:e)
#' select_format(data2, "a")
#' }
select_format <- function(data, ..., nsmall = 3, digits = 3) {
  library(dplyr)
  library(tidyr)
  selected_data <- data %>%
    dplyr::select(...)

  formatted_data <- selected_data %>%
    dplyr::mutate(across(everything(),
                  ~ as.character(format(., nsmall = nsmall, digits = digits))))

  data[names(formatted_data)] <- formatted_data

  return(data%>% tibble)
}


#' select_format
#'
#' @param data data
#' @param ... col selection
#' @param nsmall default 3
#' @param digits round 3
#'
#' @return data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#'
#'
#' # Usage example
#' data2 <- data.frame(a = rnorm(5), b = rnorm(5), c = rnorm(5), d = rnorm(5), e = rnorm(5))
#' format_col(data2, 1, 2)
#' format_col(data2, 1:5)
#' format_col(data2, a)
#' format_col(data2, a:e)
#' format_col(data2, "a")
#' }
format_col <- function(data, ..., nsmall = 3, digits = 3) {
  library(dplyr)
  library(tidyr)
  selected_data <- data %>%
    dplyr::select(...)

  formatted_data <- selected_data %>%
    dplyr::mutate(across(everything(),
                         ~ as.character(format(., nsmall = nsmall, digits = digits))))

  data[names(formatted_data)] <- formatted_data

  return(data%>% tibble)
}
