#' 중복된 데이터를 합쳐주는 함수
#'
#' @param df df
#' @param rows rows =c(1,2,3) 지정된 행을 합침
#' @param name_col 이름 열 숫자로 입력 가능
#' @param value_col 빈도 열
#' @param prop_col 비율 열
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 예제 데이터
#' library(dplyr)
#'
#' df <- data.frame(
#'   term = c("지평선중", "한국게임과학고", "가천초", "감곡초", "감곡초",
#'            "강호항공고교", "강호항공고", "개야도초", "고", "고부초",
#'            "고산고", "고산고"),
#'   Freq = c(1, 1, 1, 2, 1, 1, 1, 1, 4, 1, 1, 2),
#'   prop.. = c(0.1237624, 0.1237624, 0.1237624, 0.2475248, 0.1237624,
#'              0.1237624, 0.1237624, 0.1237624, 0.4950495, 0.1237624,
#'              0.1237624, 0.2475248)
#' )
#'
#' # 함수 호출: 디폴트 값 사용 (열 번호)
#' result <- combine_rows(df)
#'
#' # 결과 출력
#' print(result)
#'
#' # 함수 호출: 열 이름 사용
#' result_named <- combine_rows(df, name_col = "term", value_col = "Freq", score_col = "prop..")
#' print(result_named)
#'
#' #'
#' }
combine_rows <- function(df, rows = NULL, name_col = 1, value_col = 2, prop_col = 3) {
  # 열 번호를 열 이름으로 변환
  if (is.numeric(name_col)) {
    name_col <- colnames(df)[name_col]
  }
  if (is.numeric(value_col)) {
    value_col <- colnames(df)[value_col]
  }
  if (is.numeric(prop_col)) {
    prop_col <- colnames(df)[prop_col]
  }

  # 입력된 rows에 따라 필터링
  if (!is.null(rows)) {
    df <- df[rows, ]
  }

  # 그룹화하여 빈도와 비율 합산
  combined_df <- df %>%
    group_by_at(name_col) %>%
    summarise(
      !!value_col := sum(!!sym(value_col)),
      !!prop_col := sum(!!sym(prop_col)),
      .groups = "drop"
    )

  # 정렬된 결과 반환
  return(as.data.frame(combined_df))
}
