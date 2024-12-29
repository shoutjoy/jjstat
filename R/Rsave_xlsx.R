#' Save Data to Excel File
#'
#' This function saves a given data frame to an Excel file in the current working directory.
#' The file is named after the variable name of the input data.
#'
#' @param data A data frame to be saved as an Excel file.
#'
#' @return None. The function saves the data to a file and prints the save path and a reload command.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Save a data frame to an Excel file
#' df <- data.frame(
#'   Name = c("Alice", "Bob", "Charlie"),
#'   Age = c(25, 30, 35)
#' )
#' Rsave_xlsx(df)
#'
#' # The file "df.xlsx" will be saved in the current working directory.
#' # Reload the saved file with:
#' # df <- openxlsx::read.xlsx(file = "df.xlsx")
#' }
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
  message("To load this data, use: ",
          data_name, " <- openxlsx::read.xlsx(file=\"", save_path, "\")")
}




#' Save Data to Excel File
#'
#' This function saves a given data frame to an Excel file in the current working directory.
#' The file is named after the variable name of the input data.
#'
#' @param data A data frame to be saved as an Excel file.
#'
#' @return None. The function saves the data to a file and prints the save path and a reload command.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Save a data frame to an Excel file
#' df <- data.frame(
#'   Name = c("Alice", "Bob", "Charlie"),
#'   Age = c(25, 30, 35)
#' )
#' Rsave_xlsx(df)
#'
#' # The file "df.xlsx" will be saved in the current working directory.
#' # Reload the saved file with:
#' # df <- openxlsx::read.xlsx(file = "df.xlsx")
#' }
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
  message("To load this data, use: ",
          data_name, " <- openxlsx::read.xlsx(file=\"", save_path, "\")")
}
