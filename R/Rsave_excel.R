#' #' Save R Object to Excel with Import Message
#' #'
#' #' This function saves a data.frame or tibble object to an Excel file
#' #' and prints a reproducible import message for later use.
#' #'
#' #' @param data A data.frame or tibble to be saved.
#' #' @param file Character. Full path to the Excel file.
#' #' @param sheet Character. Sheet name to write.
#' #' @param overwrite Logical. Whether to overwrite existing file.
#' #' @param object_name Character. Name of the R object (auto-detected).
#' #'
#' #' @return Invisible NULL. This function is called for its side effects.
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' Rsave_excel(shindata)
#' #'
#' #' Rsave_excel(
#' #'   data  = shindata,
#' #'   file  = "E:/Rwork/02_OutAna/0_shihhk_prof/ShinData_analysis.xlsx",
#' #'   sheet = "Sheet1"
#' #' )
#' #' }
#' Rsave_excel <- function(data,
#'                         file = "E:/Rwork/02_OutAna/0_shihhk_prof/ShinData.xlsx",
#'                         sheet = "Sheet1",
#'                         overwrite = TRUE,
#'                         object_name = deparse(substitute(data))) {
#'
#'   ## -------------------------
#'   ## 1. 패키지 확인
#'   ## -------------------------
#'   if (!requireNamespace("openxlsx", quietly = TRUE)) {
#'     stop("openxlsx 패키지가 설치되어 있지 않습니다.")
#'   }
#'
#'   if (!requireNamespace("readxl", quietly = TRUE)) {
#'     stop("readxl 패키지가 설치되어 있지 않습니다.")
#'   }
#'
#'   ## -------------------------
#'   ## 2. 객체 검증
#'   ## -------------------------
#'   if (!is.data.frame(data)) {
#'     stop("data는 반드시 data.frame 또는 tibble이어야 합니다.")
#'   }
#'
#'   ## -------------------------
#'   ## 3. Excel 저장
#'   ## -------------------------
#'   openxlsx::write.xlsx(
#'     x         = data,
#'     file      = file,
#'     sheetName = sheet,
#'     overwrite = overwrite
#'   )
#'
#'   ## -------------------------
#'   ## 4. 메시지 출력
#'   ## -------------------------
#'   message("Excel 파일 저장 완료")
#'   message("저장 위치: ", normalizePath(file, winslash = "/", mustWork = FALSE))
#'
#'   message(
#'     "\n[Import 안내]\n",
#'     "library(readxl)\n",
#'     object_name, " <- readxl::read_excel(\n",
#'     "  \"", normalizePath(file, winslash = "/", mustWork = FALSE), "\",\n",
#'     "  sheet = \"", sheet, "\"\n",
#'     ")"
#'   )
#'
#'   invisible(NULL)
#' }
