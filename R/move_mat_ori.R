#' move_mat_ori
#'
#' @param mat mat
#' @param sort  sort =TRUE
#' @param type  res, coord
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
#'
#' #coord
#'
#' move_mat_ori(layout_jut)%>%
#' mutate(Mat = paste0(element," = c(", row_col,")" ))%>%
#' pull(Mat)%>%
#'   paste(collapse=", ")%>%
#'   cat("\n")
#'
#'##treatment
#'  move_mat_ori(layout_jut, type="coord")
#' #'
#' plspm_make_layout_mat(
#'   A_M1 = c(10, 10), A_M2 = c(9, 10),
#'   A_M3 = c(8, 10), B02 = c(7, 10),
#'   B04 = c(6, 10), B10 = c(5, 10),
#'    B14 = c(4, 10), C_S1 = c(8, 1),
#'    C_S2 = c(7, 1), C_S3 = c(6, 1),
#'    C_S4 = c(5, 1), C_S5 = c(4, 1),
#'    D_P1 = c(3, 10), D_P2 = c(2, 10),
#'     D_P3 = c(1, 10), 자기효능감 = c(6, 3),
#'     진로동기 = c(10, 7), 진로준비 = c(6, 7),
#'      진로태도 = c(2, 7)
#' )
#'
#' }
#'
#'
move_mat_ori <- function(mat, sort = TRUE, type= "res") {
  # 결과를 저장할 데이터 프레임 생성
  result <- data.frame(element = character(), row_col = character(), stringsAsFactors = FALSE)

  # Traverse all elements of a matrix to find non-NA values
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (!is.na(mat[i, j])) {
        # Add non-NA values to the data frame
        result <- rbind(result, data.frame(element = mat[i, j], row_col = paste(i, j, sep = ", ")))
      }
    }
  }

  result_pull = result%>%
    mutate(Mat = paste0(element," = c(", row_col,")" ))%>%
    pull(Mat)%>%paste(collapse=", ")

  result1 = result%>%
    mutate(Mat = paste0(element," = c(", row_col,")" ))


  # sort by element if sort=TRUE
  if (sort) {
    result <- result[order(result$element), ]
  }


  # 결과 출력
  switch(type, res = result, res2 =result1,  coord = result_pull)
}
