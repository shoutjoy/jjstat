#' one selcect column to colnames
#'
#' @param df df
#' @param col select col =1
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' # 함수 사용 예시
#' df <- data.frame(
#'   요인 = c("에듀테크", "학습몰입", "지각된편리성", "지각된유용성", "태도", "지속사용"),
#'   SUX = c(NA, "0.948*", "0.933*", "0.947*", "0.902*", "0.865*"),
#'   FLOW = c(NA, NA, "0.951*", "0.967*", "0.929*", "0.876*"),
#'   PEOU = c(NA, NA, NA, "0.992*", "0.972*", "0.940*"),
#'   PU = c(NA, NA, NA, NA, "0.956*", "0.927*"),
#'   ATT = c(NA, NA, NA, NA, NA, "0.972*")
#' )
#'
#' # col_to_colnames 함수 실행
#' new_df <- col_to_colnames(df, col = 1)
#' print(new_df)
#' }
#'
#'
col_to_colnames <- function(df, col = 1) {
  # 첫 번째 열의 내용을 열 이름으로 설정
  new_colnames <- as.character(df[[col]])

  # 첫 번째 열을 제외한 나머지 데이터
  df <- df[,-col]

  # 새로운 열 이름 설정
  colnames(df) <- new_colnames

  return(df)
}
