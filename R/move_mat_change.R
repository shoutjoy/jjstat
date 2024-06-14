#' Functions that replace a single component
#'
#' @param mat matrix
#' @param pattern pattern
#' @param imp imputation
#'
#' @return matrix
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' plspm_boot_factor_layout(pls_ts)%>%
#'   move_mat_cut(1,3)%>%
#'   move_mat_change("int","i_I")%>%make_mat_text()
#'
#' layout_New = matrix(
#'   c( NA, 'IMA',  NA, NA, 'i_I',
#'      NA, 'QUA',NA,  'VAL', 'LOY',
#'      'EXP', NA, 'SAT',NA, NA),
#'   nrow = 3, ncol = 5, byrow = TRUE)
#' }
#'
#'
move_mat_change <- function(mat, pattern, imp) {
  mat[mat == pattern] <- imp
  return(mat)
}



#' move_mat_nc
#'
#' @param mat mat
#' @param row row
#' @param col cp;
#' @param name change name
#'
#' @return mat
#' @export
#'

move_mat_nc <- function(mat, row, col, name) {
  # 현재 위치에 있는 값을 찾아서 제거
  current_value <- mat[row, col]
  if (!is.na(current_value)) {
    mat[row, col] <- NA
  }

  # 새로운 위치에 값을 할당
  mat[row, col] <- name
  return(mat)
}
