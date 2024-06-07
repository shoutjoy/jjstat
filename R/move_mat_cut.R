#' move_mat_cut
#'
#' @param mat matrix
#' @param start 1
#' @param n width = 3
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 테스트용 행렬 데이터
#' mat <- matrix(c(NA, "진로동기", NA, "자기효능감", NA,
#'     "진로준비", NA, "진로태도", NA), nrow = 3, byrow = TRUE)
#'
#' # 함수 실행
#' move_mat_cut(mat, start = 1, n = 3)
#'
#' }
#'
#'
move_mat_cut <- function(mat, start = 1, n=3) {
  # 행렬의 행과 열 길이 얻기
  nrow_mat <- nrow(mat)
  ncol_mat <- ncol(mat)

  # 새로운 행렬 초기화
  new_mat <- matrix(NA, nrow = nrow_mat, ncol = ncol_mat)

  # 각 요소에 접근하여 텍스트 자르기
  for (i in 1:nrow_mat) {
    for (j in 1:ncol_mat) {
      if (!is.na(mat[i, j])) {
        new_mat[i, j] <- substr(mat[i, j], start, start + n - 1)
      }
    }
  }

  return(new_mat)
}

#' move_mat_cut
#'
#' @param mat matrix
#' @param start 1
#' @param n width = 3
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 테스트용 행렬 데이터
#' mat <- matrix(c(NA, "진로동기", NA, "자기효능감", NA,
#'     "진로준비", NA, "진로태도", NA), nrow = 3, byrow = TRUE)
#'
#' # 함수 실행
#' move_mat_cut(mat, start = 1, n = 3)
#'
#' }
#'
#'
move_mat_substr <- function(mat, start = 1, n=3) {
  # 행렬의 행과 열 길이 얻기
  nrow_mat <- nrow(mat)
  ncol_mat <- ncol(mat)

  # 새로운 행렬 초기화
  new_mat <- matrix(NA, nrow = nrow_mat, ncol = ncol_mat)

  # 각 요소에 접근하여 텍스트 자르기
  for (i in 1:nrow_mat) {
    for (j in 1:ncol_mat) {
      if (!is.na(mat[i, j])) {
        new_mat[i, j] <- substr(mat[i, j], start, start + n - 1)
      }
    }
  }

  return(new_mat)
}
