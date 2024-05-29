#' select rows filter function
#'
#' @param data data
#' @param ... row condition
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mtcars %>% row_filter(cyl == 6 )
#' mtcars %>% row_filter(vs == 1)
#' mtcars %>% row_filter(cyl == 6 | vs == 1)
#' mtcars %>% row_filter(cyl == 6 & vs == 1)
#' }
#'
#'
#'
row_filter <- function(data, ...) {
  # 조건을 리스트로 변환
  conditions <- rlang::enquos(...)

  # 조건에 따른 데이터 필터링
  filtered_data <- dplyr::filter(data, !!!conditions)

  return(filtered_data)
}
