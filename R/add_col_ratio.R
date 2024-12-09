#' 그룹합에 비율을 만들어 주는 함수
#'
#' @param data data
#' @param col 기준이 되는 열
#' @param n_col 빈도를 나타내는 열
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 사용 예제
#' library(dplyr)
#'
#' # 주어진 데이터프레임
#' data <- data.frame(
#'   Q17 = c("", "고1 남학생", "고1 여학생", "고2 남학생", "고2 여학생",
#'           "고3 남학생", "고3 여학생", "중1 남학생", "중1 여학생",
#'           "중2 남학생", "중2 여학생", "중3 남학생", "중3 여학생",
#'           "초1 남학생", "초1 여학생", "초2 남학생", "초2 여학생",
#'           "초3 남학생", "초3 여학생", "초4 남학생", "초4 여학생",
#'           "초5 남학생", "초5 여학생", "초6 남학생", "초6 여학생"),
#'   Q16 = c("", rep("", 24)),
#'   n = c(1130, 413, 184, 329, 163, 216, 72, 354, 323, 370, 308,
#'         195, 169, 7, 4, 25, 23, 60, 53, 174, 162, 237, 216, 307, 293)
#' )
#'
#' # 함수 적용: 열 이름으로 지정
#' result1 <- add_col_ratio(data, col = "Q17", n_col = "n")
#'
#' # 함수 적용: 숫자 인덱스로 지정
#' result2 <- add_col_ratio(data, col = 1, n_col = 3)
#'
#' # 결과 확인
#' print(result1)
#' print(result2)
#'

#'
#' }
add_col_ratio <- function(data, col, n_col) {
  # 숫자 인덱스인 경우 열 이름으로 변환
  if (is.numeric(col)) col <- names(data)[col]
  if (is.numeric(n_col)) n_col <- names(data)[n_col]

  # 그룹화된 데이터의 합계 계산
  summed_data <- data %>%
    dplyr::group_by(!!sym(col)) %>%
    dplyr::summarise(total_n = sum(!!sym(n_col), na.rm = TRUE)) %>%
    dplyr::ungroup()

  # 비율 계산 및 원본 데이터에 병합
  data <- data %>%
    dplyr::left_join(summed_data, by = col) %>%
    dplyr::mutate(`prop(%)` = (!!sym(n_col) / total_n) * 100) %>%
    dplyr::select(-total_n) # 합계 열 제거

  return(data)
}
