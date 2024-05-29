#' Functions to remove selected rows
#'
#' @param data data
#' @param ...  condition filter
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mtcars %>% remove_filter(cyl == 6 )
#' mtcars %>% remove_filter(vs == 1)
#' mtcars %>% remove_filter(am==0)
#' mtcars %>% remove_filter(vs == 1)
#' mtcars %>% remove_filter(cyl == 6 | vs == 1)
#' mtcars %>% remove_filter(cyl == 6 & vs == 1)
#' mtcars %>% remove_filter(cyl == 6 | cyl == 4 | am == 0)
#' }
#'
remove_filter <- function(data, ...) {
  # 조건을 리스트로 변환
  conditions <- rlang::enquos(...)

  # 조건에 맞는 행 필터링
  filtered_rows <- dplyr::filter(data, !!!conditions)

  # 조건에 맞는 행을 제외한 데이터 반환
  remaining_data <- dplyr::anti_join(data, filtered_rows, by = names(data))

  return(remaining_data)
}
