#' 엑셀파일로 저장
#'
#' @param data df
#'
#' @return excel data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' Rsave(jen_binddata)
#' Rload(file="F:/Rwork/02_OutAna/Joeunnarae_yonsei_2024/jen_binddata.RData")
#' }
#'
save_xlsx <- function(data) {
  # 패키지 로드
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required but not installed. Please install it first.")
  }

  # 데이터 이름 가져오기
  data_name <- deparse(substitute(data))

  # 파일 경로 생성
  save_path <- file.path(getwd(), paste0(data_name, ".xlsx"))

  # 데이터 저장
  openxlsx::write.xlsx(data, file = save_path)

  # 메시지 출력
  message("Data saved to: ", save_path)
  message("To load this data, Use: ",
          data_name, " = read.xlsx(file=\"", save_path, "\")")
}


#' 엑셀파일로 저장
#'
#' @param data df
#'
#' @return excel data
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' Rsave(jen_binddata)
#' Rload(file="F:/Rwork/02_OutAna/Joeunnarae_yonsei_2024/jen_binddata.RData")
#' }
#'
Rsave_xlsx <- function(data) {
  # 패키지 로드
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required but not installed. Please install it first.")
  }

  # 데이터 이름 가져오기
  data_name <- deparse(substitute(data))

  # 파일 경로 생성
  save_path <- file.path(getwd(), paste0(data_name, ".xlsx"))

  # 데이터 저장
  openxlsx::write.xlsx(data, file = save_path)

  # 메시지 출력
  message("Data saved to: ", save_path)
  message("To load this data, Use: ",
          data_name, " = read.xlsx(file=\"", save_path, "\")")
}
