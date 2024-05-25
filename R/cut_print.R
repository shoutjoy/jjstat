#' Functions that cutoff a given value
#'
#' @param data data.frame
#' @param cut cutoff  default 0
#' @param digits round 3
#' @param rm_row remove row
#' @param rm_cut remove cutoff rows
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' mtcars%>% cut_print(cut = 280)
#' mtcars%>% cut_print(cut = 280, rm_row = T)
#' mtcars%>% cut_print(cut = 280, rm_row = T,rm_cut=T)
#' #'
#' }
cut_print <- function(data, cut = 0, digits = 3,
                      rm_row = FALSE, rm_cut = FALSE) {
  # Step 1: 행의 합이 0인 경우 제거 (numeric 변수만 대상)
  if (rm_row) {
    numeric_cols <- sapply(data, is.numeric)
    data = data[rowSums(data[, numeric_cols]) != 0, ]
  }
  # Step 2: 각 열의 값을 cut 값과 비교하여 조건에 맞는 값들만 남기고 나머지는 ""로 대체
  # data = Round(data, digits)
  for (i in 1:ncol(data)) {
    if (is.numeric(data[[i]])) {
      data[[i]] <- round(data[[i]], digits)
    }
  }
  # Step 3: cut에 대한  처리
  res = data %>%
    mutate(across(where(is.numeric), ~ ifelse(abs(.) <= cut, "", .))) %>%
    data.frame()
  # Step 4:
  if (rm_cut) {
    # cut 조건이 적용된 열의 값이 모두 ""인 행을 제거합니다.
    res = res %>%
      filter(rowSums(res == "") != ncol(res))
  }

  return(res)
}
