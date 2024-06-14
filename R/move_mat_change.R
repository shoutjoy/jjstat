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
