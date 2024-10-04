
#' RData fast save
#'
#' @param df data to save
#' @param file filename
#'
#' @return data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Rsave(Kge_jbs)           # 기본 파일 이름: Kge_jbs.RData
#' Rsave(Kge_jbs, file = "my_custom_name")  # 사용자 지정 파일 이름: my_custom_name.RData
#' }

Rsave <- function(df, file = "") {
  # 데이터프레임 이름을 문자열로 추출
  df_name <- deparse(substitute(df))

  # 만약 file 인수가 제공되지 않았다면, 데이터프레임 이름으로 파일 이름을 설정
  if (file == "") {
    file_name <- paste0(df_name, ".RData")
  } else {
    file_name <- paste0(file, ".RData")
  }

  # 데이터 저장
  save(df, file = file_name)

  # 저장된 위치 확인
  file_path <- file.path(getwd(), file_name)

  # 메시지 출력
  cat("Data saved to:", file_path, "\n")
  cat("To load this data, Use: Rload(file=\"", file_path, "\")\n", sep="","\n",
      "Use: load(file=\"", file_path, "\")\n", sep=""
      )
}

# Rsave <- function(df, file = "") {
#   # 데이터프레임 이름을 문자열로 추출
#   df_name <- deparse(substitute(df))
#
#   # 만약 file 인수가 제공되지 않았다면, 데이터프레임 이름으로 파일 이름을 설정
#   if (file == "") {
#     file_name <- paste0(df_name, ".RData")
#   } else {
#     file_name <- paste0(file, ".RData")
#   }
#
#   # 데이터 저장
#   save(df, file = file_name)
#
#   # 저장된 위치 확인
#   file_path <- file.path(getwd(), file_name)
#
#   # 메시지 출력
#   cat("Data saved to:", file_path, "\n")
#   cat("To load this data, use: load(file=\"", file_name, "\")\n", sep="")
# }
