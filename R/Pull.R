#' Pull select data.frame col to vector
#'
#' @param df df
#' @param col select col, "Freq"
#' @param name using names from rownames or col data"name"
#' @param sep paste sep = "_"
#'
#' @return vector
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 예시 데이터
#' df <- tibble::tibble(
#'   term = c("학교 수업에서", "혼자서 공부할 때", "활용 안함"),
#'   Freq = c(2058, 1248, 646)
#' )
#'
#' # 함수 사용 예시
#' Pull(df, col="Freq", name="term")
#' }
#'
#'
Pull <- function(df, col="Freq", name= "name", sep="_") {
  # col이 열 번호나 열 이름인지 확인하고 해당 열을 추출
  if (is.numeric(col)) {
    col_data <- df[[col]]
  } else if (is.character(col)) {
    col_data <- df[[col]]
  } else {
    stop("col은 열 번호 또는 열 이름이어야 합니다.")
  }

  # name 설정: name이 NULL일 경우 행 이름을 사용
  if (is.null(name)) {
    name_data <- rownames(df)
  } else {
    # name이 열 번호 또는 열 이름일 경우
    if (is.numeric(name)) {
      name_data <- df[[name]]
    } else if (is.character(name)) {
      name_data <- df[[name]]
    } else {
      stop("name은 열 번호 또는 열 이름이어야 합니다.")
    }
  }

  # 이름 벡터에서 빈칸을 sep으로 대체
  name_data <- gsub(" ", sep, name_data)

  # 이름을 names 속성으로 설정한 벡터 반환
  result <- setNames(col_data, name_data)
  return(result)
}
