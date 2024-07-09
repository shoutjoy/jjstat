
#' longdata transfomation
#'
#' @param data widedata
#' @param names_to name, speaker(kge)
#' @param values_to value, freq
#' @param cols colrange
#' @param fix  column fixed (thereofore not using cols )
#' @param rowname defailt accent
#'
#' @return longdata
#' @export
#'
#' @examples
#' \dontrun{
#'
#' mtcars[,1:4] %>%long_df("car", "value")
#' mtcars[1:3, 1:7] %>% long_df(fix= 1:3)
#' mtcars[1:3, 1:7] %>% long_df(fix= 2:7)
#' mtcars[1:3, 1:7] %>% long_df(cols= 3:7, rownames_to_column=F)
#' mtcars[1:3, 1:7] %>% long_df(cols= 3:7)
#' mtcars[1:3, 1:7] %>% long_df(cols= 5:7)
#'
#'
#'  mtcars[,1:6] %>%long_df("car", "value", rownames_to_column = F, cols = c(1,3:6))
#'
#'
#'
#' }
long_df <- function(data,
                    rowname = "rownames",
                    names_to = "names",
                    values_to = "values",
                    cols = NULL,
                    fix = NULL,
                    rownames_to_column = TRUE) {
  data = data.frame(data)
  # 데이터프레임으로 변환하고 행 이름을 추가
  if (rownames_to_column) {
    data <- data %>%
      tibble::rownames_to_column(var = rowname)
  }

  # 선택할 열 결정
  if (is.null(cols)) {
    cols_selection <- dplyr::select(data, where(is.numeric)) %>% colnames()
  } else {
    cols_selection <- cols
  }

  # 열 이름 추출
  colName <- names(data)

  # pivot_longer 적용
  if (is.null(fix)) {
    data_long <- data %>%
      tidyr::pivot_longer(cols = cols_selection,
                          names_to = names_to,
                          values_to = values_to)
  } else {
    # 고정 열을 지정하여 pivot_longer 적용
    data_long <- data %>%
      tidyr::pivot_longer(cols = -c(all_of(fix)),
                          names_to = names_to,
                          values_to = values_to)
  }

  return(data_long)
}





