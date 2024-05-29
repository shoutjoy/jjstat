#' Functions that cutoff a given value
#'
#' @param data data.frame
#' @param cut cutoff  default 0
#' @param digits round 3
#' @param rm_row remove row
#' @param rm_cut remove cutoff rows
#' @param star over cut marked star pasted
#' @param each  each col using cut
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
                      rm_row = FALSE, rm_cut = FALSE,
                      star = FALSE, each = NULL) {
  # Step 1: 행의 합이 0인 경우 제거 (numeric 변수만 대상)
  if (rm_row) {
    numeric_cols <- sapply(data, is.numeric)
    data <- data[rowSums(data[, numeric_cols], na.rm = TRUE) != 0, ]
  }

  # Step 2: 각 열의 값을 소수점 반올림
  for (i in 1:ncol(data)) {
    if (is.numeric(data[[i]])) {
      data[[i]] <- round(data[[i]], digits)
    }
  }

  # Step 3: each 값에 대한 처리
  numeric_cols <- sapply(data, is.numeric)

  if (!is.null(each)) {
    extended_each <- ifelse(numeric_cols, each, NA)

    for (i in 1:ncol(data)) {
      if (is.numeric(data[[i]])) {
        data[[i]] <- ifelse(is.na(data[[i]]) | data[[i]] < extended_each[i], "", data[[i]])
      }
    }
  } else {
    # Step 4: cut에 대한 처리
    if (star) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]] <- ifelse(is.na(data[[i]]) | abs(data[[i]]) <= cut,
                              data[[i]], paste0(data[[i]], "*"))
        }
      }
    } else {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]] <- ifelse(is.na(data[[i]]) | abs(data[[i]]) <= cut, "", data[[i]])
        }
      }
    }
  }

  res <- data.frame(data)

  # Step 5: rm_cut 옵션에 따른 처리 (단, star가 TRUE일 때는 적용되지 않음)
  if (rm_cut && !star) {
    res <- res %>%
      filter(rowSums(res == "", na.rm = TRUE) != ncol(res))
  }

  return(res)
}

#' overall cut
#'
#' @param data data
#' @param cut cut =0.5
#' @param digits 3
#' @param imp "
#'
#' @return data
#' @export
#'
cut_print_all <- function(data, cut = 0, imp="", digits=3) {
  data = data.frame(data)
  data = round2(data, digits)
  # 각 열의 값을 cut 값과 비교하여 조건에 맞는 값들만 남기고 나머지는 ""로 대체합니다.
  res=  data%>%
    mutate(across(where(is.numeric), ~ ifelse(abs(.) <= cut, imp, .)))%>%
    data.frame()

  res
}
