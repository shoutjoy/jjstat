#' p value  to <.001
#'
#' @param df df data
#' @param ... col names and number
#' @param digits 3
#' @param remove default TRUE, col replace
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#'
#' # 예제 데이터
#' library(dplyr)
#'
#' df <- tibble(
#'   pairwise = c("30분이상_20-30분", "30분이상_10-20분", "30분이상_5-10분", "30분이상_5분이내", "20-30분_10-20분",
#'                "20-30분_5-10분", "20-30분_5분이내", "10-20분_5-10분", "10-20분_5분이내", "5-10분_5분이내"),
#'   chisq = c(198, 275, 349, 441, 9.27, 34.2, 87.2, 8.4, 44.6, 16.0),
#'   df = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   p.value = c(5.83e-45, 1.09e-61, 8.37e-78, 7.50e-98, 2.33e-3, 4.85e-9, 9.66e-21, 3.76e-3, 2.44e-11, 6.30e-5),
#'   adj.p = c(1.46e-44, 3
#'
#'
#'             # 함수 실행 예시
#'             # 열 번호로 처리
#'             result <- p_to_001(df, 4, 6, remove = TRUE)
#'
#'             # 열 이름으로 처리
#'             result2 <- p_to_001(df, "p.value", "adj.p", remove = TRUE)
#'
#'             print(result)  # 열 번호로 처리한 결과
#'             print(result2) # 열 이름으로 처리한 결과
#' #'
#' }
p_to_001 <- function(df, ..., digits = 3, remove = TRUE) {
  # 처리할 열 이름이나 번호를 가져옴
  cols <- unlist(list(...))

  # 열 번호를 열 이름으로 변환 (필요한 경우)
  cols <- sapply(cols, function(col) {
    if (is.numeric(col)) {
      colnames(df)[col]
    } else {
      col
    }
  })

  # 지정된 열에 대해 값 변환
  for (col in cols) {
    if (remove) {
      # 열을 직접 교체
      df[[col]] <- ifelse(as.numeric(df[[col]]) < 0.001, "< .001", round(as.numeric(df[[col]]), digits = digits))
    } else {
      # 새 열 생성: "_proces"를 추가한 이름으로 생성
      new_col <- paste0(col, "_proces")
      df[[new_col]] <- ifelse(as.numeric(df[[col]]) < 0.001, "< .001", round(as.numeric(df[[col]]), digits = digits))
    }
  }

  return(df)
}
