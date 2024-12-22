
#' one select column to colnames
#'
#' @param df data frame
#' @param col column to use as new column names (default = 1)
#' @param remove logical. If TRUE, removes the selected column (default = TRUE).
#'
#' @return data frame with updated column names
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' df <- data.frame(
#'   요인 = c("에듀테크", "학습몰입", "지각된편리성", "지각된유용성", "태도", "지속사용"),
#'   SUX = c(NA, "0.948*", "0.933*", "0.947*", "0.902*", "0.865*"),
#'   FLOW = c(NA, NA, "0.951*", "0.967*", "0.929*", "0.876*"),
#'   PEOU = c(NA, NA, NA, "0.992*", "0.972*", "0.940*"),
#'   PU = c(NA, NA, NA, NA, "0.956*", "0.927*"),
#'   ATT = c(NA, NA, NA, NA, NA, "0.972*")
#' )
#'
#' # col_to_colnames 실행
#' new_df <- col_to_colnames(df, col = 1, remove = FALSE)
#' print(new_df)
#'
#' new_df2 <- col_to_colnames(df, col = 1, remove = TRUE)
#' print(new_df2)
#' }
#'
col_to_colnames <- function(df, col = 1, remove = TRUE) {
  # 선택한 열의 내용을 열 이름으로 설정
  new_colnames <- as.character(df[[col]])

  if (remove) {
    # 선택한 열 제거
    df <- df[,-col]
    colnames(df) <- new_colnames
  } else {
    # 선택한 열 유지
    colnames(df) <- c("-", new_colnames)
  }

  return(df)
}
