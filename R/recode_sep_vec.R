#' recode_sep_vec 벡터데이터를 리코딩
#'
#' @param vector vector
#' @param ranges_list 변환리스트 list("0~5년" = 0:5)
#'
#' @return vector
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 예시 사용
#' vec <- c("53", "45", "38", "28", "41", "12", "0", "10", "30", "35")
#'
#' # 함수를 사용하여 벡터 재코딩
#' new_vec <- recode_sep_vec(vec, list("0~5년" = 0:5,
#'                                     "5~10년" = 6:10,
#'                                     "11~15년" = 11:15,
#'                                     "16~20년" = 16:20,
#'                                     "21~25년" = 21:25,
#'                                     "26~30년" = 26:30,
#'                                     "30년이상" = 31:60))
#'
#' # 결과 확인
#' print(new_vec)
#'
#' # # # 예시 사용
#' # vec <- c(53, 45, 45, 38, 36, 28, 38, 28, 41, 55, 39, 29, 31, 35, 37, 34, 34, 35, 58, 46)
#'
#' # # 함수를 사용하여 벡터 재코딩
#' new_vec <- recode_sep_vec(vec, list("20대" = 20:29, "30대" = 30:39, "40대" = 40:49, "50대" = 50:59))
#'
#' # # 결과 확인
#' # print(new_vec)
#' }
#'
#'
recode_sep_vec <- function(vector, ranges_list) {
  # vector를 숫자형으로 변환
  vector <- as.numeric(vector)

  # 결과를 저장할 새로운 벡터 생성 (NA로 초기화)
  new_vector <- rep(NA, length(vector))

  # ranges_list에 있는 각 범위별로 라벨링
  for (label in names(ranges_list)) {
    # 리스트 내 값을 벡터로 변환
    range_values <- unlist(ranges_list[[label]])

    # 범위에 따라 값 재코딩
    new_vector[vector %in% range_values] <- label
  }

  return(new_vector)
}
