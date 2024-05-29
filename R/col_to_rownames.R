#' column to rownames col2row
#'
#' @param data data.frame
#' @param col Columns that should be rows
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data_text = "Tobacco_Dependence (0,5] (5,10] (10,15] (15,20] (20,98]
#' No_Nicotine_Dependence 130 210 43 114 20
#' Nicotine_Dependence 119 267 91 254 67"
#' #method1
#' # nt = data_text%>% text2df(header=T)%>%
#' #         tibble::column_to_rownames("Tobacco_Dependence")
#' #method2
#' nt = data_text%>% text2df(header=T)%>% jjstat::col2row()
#' nt=Char2num(nt,1:5)
#' nt
#' nt %>%tibble()
#'
#' #NEW data
#' New = data.frame(
#'   rownames= c('No_Nicotine_Dependence', 'Nicotine_Dependence'),
#'
#'   "A" = c(130, 119),
#'   "B" = c(210, 267),
#'   "C" = c(43, 91),
#'   "D" = c(114, 254),
#'   "E" = c(20, 67)
#' );New
#' New%>%col2row()
#' New%>%col2row(col="B")
#' #mtcars
#' data_mtcars=mtcars%>%tibble::rownames_to_column()
#' data_mtcars%>%col2row()
#'
#'
#'
#' }
#'
#'
col2row <- function(data, col=1) {
  data = as.data.frame(data)

  # col이 숫자일 경우 열 번호로 사용, 문자일 경우 열 이름으로 사용
  if (is.numeric(col)) {
    column_name <- names(data)[col]
  } else if (is.character(col)) {
    column_name <- col
  } else {
    stop("col must be either a column number or column name")
  }

  # 행 이름으로 지정
  rownames(data) <- data[[column_name]]

  # 열 제거
  data <- data[, !(names(data) %in% column_name)]

  # 결과 반환
  return(data)
}
# col2row <- function(data, col=1) {
#   data = as.data.frame(data)
#   # 첫 번째 열을 행 이름으로 지정
#   rownames(data) <- data[[col]]
#
#   # 첫 번째 열 제거
#   data <- data[, -col]
#
#   # 결과 반환
#   return(data)
# }



#' column to rownames , col_to_rownames
#'
#' @param data data.frame
#' @param col Columns that should be rows
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' data_text = "Tobacco_Dependence (0,5] (5,10] (10,15] (15,20] (20,98]
#' No_Nicotine_Dependence 130 210 43 114 20
#' Nicotine_Dependence 119 267 91 254 67"
#' #method1
#' # nt = data_text%>% text2df(header=T)%>%
#' #         tibble::column_to_rownames("Tobacco_Dependence")
#' #method2
#' nt = data_text%>% text2df(header=T)%>% jjstat::col2row()
#' nt=Char2num(nt,1:5)
#' nt
#' nt %>%tibble()
#'
#' #NEW data
#' New = data.frame(
#'   rownames= c('No_Nicotine_Dependence', 'Nicotine_Dependence'),
#'
#'   "A" = c(130, 119),
#'   "B" = c(210, 267),
#'   "C" = c(43, 91),
#'   "D" = c(114, 254),
#'   "E" = c(20, 67)
#' );New
#' New%>%col_to_rownames()
#'
#' #mtcars
#' data_mtcars=mtcars%>%tibble::rownames_to_column()
#' data_mtcars%>%col_to_rownames()
#'
#'
#'
#' }
#'
#'
col_to_rownames <- function(data, col=1) {
  data = as.data.frame(data)
  # 첫 번째 열을 행 이름으로 지정
  rownames(data) <- data[[col]]

  # 첫 번째 열 제거
  data <- data[, -col]

  # 결과 반환
  return(data)
}
