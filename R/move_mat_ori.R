#' move_mat_ori
#'
#' @param mat mat
#' @param sort  sort =TRUE
#'
#' @return coordnate
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' layout_jut <- matrix(
#'   c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 'D_P3',
#'     NA, NA, NA, NA, NA, NA, '진로태도', NA, NA, 'D_P2',
#'     NA, NA, NA, NA, NA, NA, NA, NA, NA, 'D_P1',
#'     'C_S5', NA, NA, NA, NA, NA, NA, NA, NA, 'B14',
#'     'C_S4', NA, NA, NA, NA, NA, NA, NA, NA, 'B10',
#'     'C_S3', NA, '자기효능감', NA, NA, NA, '진로준비', NA, NA, 'B04',
#'     'C_S2', NA, NA, NA, NA, NA, NA, NA, NA, 'B02',
#'     'C_S1', NA, NA, NA, NA, NA, NA, NA, NA, 'A_M3',
#'     NA, NA, NA, NA, NA, NA, NA, NA, NA, 'A_M2',
#'     NA, NA, NA, NA, NA, NA, '진로동기', NA, NA, 'A_M1'),
#'   nrow = 10, ncol = 10, byrow = TRUE)
#' move_mat_ori(layout_jut)
#' }
#'
#'
move_mat_ori <- function(mat, sort = TRUE) {
  # 결과를 저장할 데이터 프레임 생성
  result <- data.frame(element = character(), row_col = character(), stringsAsFactors = FALSE)

  # 행렬의 모든 요소를 순회하면서 NA가 아닌 값을 찾기
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (!is.na(mat[i, j])) {
        # NA가 아닌 값을 데이터 프레임에 추가
        result <- rbind(result, data.frame(element = mat[i, j], row_col = paste(i, j, sep = ", ")))
      }
    }
  }

  # result = result%>%
  #             tidyr::separate(row_col, c("row","col"), sep=",", remove=FALSE)
  # sort=TRUE 인 경우 element 기준으로 정렬
  if (sort) {
    result <- result[order(result$element), ]
  }

  # 결과 출력
  return(result)
}
