
#' colnames_change
#'
#' @param df df
#' @param ... input list
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 사용 예시
#' jutLca3 %>%
#'   lca_class_freq() %>%
#'   data.frame() %>%
#'   colnameschange("Class", "N")
#' }
colnames_change <- function(df, ...) {
  # ...는 가변 인자로, 변경하고자 하는 새로운 열 이름들을 받음
  new_colnames <- c(...)

  # 열의 개수와 새로운 열 이름 개수가 일치하는지 확인
  if (length(new_colnames) != ncol(df)) {
    stop("열 이름의 개수와 데이터프레임의 열 개수가 일치하지 않습니다.")
  }

  # 데이터프레임의 열 이름을 변경
  colnames(df) <- new_colnames

  return(df)
}
