#' remove rows
#'
#' @param data data
#' @param ...  rows
#' @param filter TRUE possible filter
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 1~3행 제거
#' remove_rows(mtcars,  1:30)
#' remove_rows(mtcars, 1,2,3,6,5,7,8,9,30)
#' remove_rows(mtcars, rm = c(1,3,4,5,6,7,11:30))
#' remove_rows(mtcars, cyl == 4, filter=T)
#'
#' # 1행과 3행 제거
#' print(remove_rows(mtcars, rm_row = c(1, 3)))
#'
#' # 필터 조건 적용 (cyl == 6)
#' remove_rows(mtcars,rm=NULL, cyl == 6) #error
#' remove_rows(mtcars, cyl == 6 |cyl ==4, filter=T)
#'
#' # 필터 조건 적용 (cyl == 6 또는 vs == 1)
#' remove_rows(mtcars, cyl == 6 | vs == 1, filter=T)
#'
#' # 필터 조건 적용 (cyl == 6 그리고 vs == 1)
#' remove_rows(mtcars, cyl == 6 | vs == 1, filter=T)
#'
#' # 필터 조건 적용 후 특정 행 제거 (필터가 우선 적용)
#' remove_rows(mtcars, 1:3, cyl == 6 | vs == 1)
#'
#' #'
#' }
#'
#'
remove_rows <- function(data, ..., filter=FALSE) {
  if(filter){
    # to list
    conditions <- rlang::enquos(...)
    # filter rows
    filtered_rows <- dplyr::filter(data, !!!conditions)
    remaining_data <- dplyr::anti_join(data, filtered_rows, by = names(data))

    message("Filter(s) applied. Rows removed based on filter(s).", "\n")
    return(remaining_data)

  }else{
    # Convert all arguments passed to ... to vectors
    rm_row <- c(...)
    # If rm_row is specified, remove that row
    if (length(rm_row) > 0) {
      return(data[-rm_row, ])
    }
    # Return original data if rm_row is not specified
    return(data)
  }
}
