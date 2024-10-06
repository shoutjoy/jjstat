#' Rload
#'
#' @param file position
#' @param verbose  default TRUR
#'
#' @return datafile
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' #'
#' #'
#' Rload("pwj2.RData", verbose = TRUE)
#' pwj2 %>%head()
#' Rload(file="F:/Rwork/02_OutAna/Parkwonjun_yonsei_2024/pwj2.RData")
#'}
#'
#'
Rload <- function(file, verbose = TRUE) {
  # 파일 이름에서 확장자(.RData)를 제거한 객체 이름 추출
  object_name <- sub("\\.RData$", "", basename(file))

  # 데이터를 로드
  load(file, envir = .GlobalEnv)

  # 로드된 객체 이름 확인
  loaded_objects <- ls(.GlobalEnv)

  # 객체 이름이 파일명과 같은 경우 찾기
  for (obj in loaded_objects) {
    if (!exists(object_name, envir = .GlobalEnv)) {
      assign(object_name, get(obj, envir = .GlobalEnv), envir = .GlobalEnv)
      if (verbose) {
        message("Loading object: str(", object_name, ")")
      }
    }
  }

  # 로드된 객체 구조를 message로 출력
  if (exists(object_name, envir = .GlobalEnv)) {
    message("Structure of the loaded object below to")
    cat("\n")
    message(paste0("str(",object_name,")"))
    cat("\n")
    object_str <- capture.output(str(get(object_name, envir = .GlobalEnv)))
    message(paste(object_str, collapse = "\n"))
  }
}
