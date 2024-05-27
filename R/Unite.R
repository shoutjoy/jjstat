#' UInte data
#'
#' @param data data
#' @param col1 select col1
#' @param col2 select col2
#' @param Col col names
#' @param sep ""
#' @param remove true
#'
#' @return  data
#' @export
#'
#' @examples

#' \dontrun{
#' #'
#' # 예시 데이터 생성
#' data <- data.frame(
#'   block = c("IMAG", "IMAG", "IMAG", "IMAG"),
#'   name = c("imag1", "imag2", "imag3", "imag4"),
#'   Original = c("0.7093265 ***", "0.8773078 ***", "0.8417485 ***", "0.5691934 ***"),
#'   sig = c("***", "***", "***", "***"),
#'   Mean.Boot = c(0.7088215, 0.8750787, 0.8401757, 0.5647333),
#'   Std.Error = c(0.05949724, 0.02402834, 0.02970117, 0.07686419),
#'   perc.025 = c(0.5774529, 0.8231642, 0.7784983, 0.3876417),
#'   perc.975 = c(0.8105249, 0.9146114, 0.8863100, 0.6961572)
#' )
#'
#' # 예시 호출
#'
#' # Unite using column numbers
#' result1 <- Unite(data, 3, 4)
#' print(result1)
#'
#' # Unite using column numbers with a new column name
#' result2 <- Unite(data, 3, 4, "New")
#' print(result2)
#'
#' # Unite using column names
#' result3 <- Unite(data, "Original", "sig")
#' print(result3)
#'
#' # Unite using column names with a new column name
#' result4 <- Unite(data, "Original", "sig", "New")
#' print(result4)
#'
#'
#' }
#'
Unite <- function(data, col1, col2, Col=NULL, sep="", remove=TRUE) {
  # Check if col1 and col2 are numeric
  if (is.numeric(col1) && is.numeric(col2)) {
    col1 <- names(data)[col1]
    col2 <- names(data)[col2]
  }

  if (is.null(Col)) {
    # Use tidyr::unite to combine the columns
    data <- tidyr::unite(data, col={{col1}}, all_of(c(col1, col2)), sep=sep, remove=remove)
  } else {
    # Use tidyr::unite to combine the columns
    data <- tidyr::unite(data, col={{Col}}, all_of(c(col1, col2)), sep=sep, remove=remove)
  }
  return(data)
}
