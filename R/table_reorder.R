#' Reorder rows of data
#'
#' @param data data.fram
#' @param reorder  reorder=c(select row order)
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example data
#' data0 <- data.frame(
#'   source = c("A05", "A08", "A04", "A06",
#'   "A09", "A11", "A13", "A02", "A03", "A01",
#'   "A14", "A15", "A07", "A12", "A10",
#'   "eigen_value", "Proportion_Var", "Cumulative_Var"),
#'   MR1 = c("", "", "", "", "", "", "", "0.362",
#'   "0.449", "0.457", "0.47", "0.538", "0.62",
#'   "0.792", "0.96", "3.363", "0.224", "0.224"),
#'   MR2 = c("", "0.382", "", "", "0.445", "0.724",
#'   "0.906", "0.392", "", "", "0.487", "", "", "",
#'    "", "2.095", "0.14", "0.364"),
#'   MR3 = c("", "", "0.402", "0.963", "", "", "",
#'   "", "", "", "", "", "", "", "", "1.691",
#'   "0.113", "0.477")
#' )
#'
#' # 함수 테스트
#' new_order <- c(2, 3, 1, 4:16)
#' # 1번 columns backwards, and the rest in order
#' table_reorder(data0, new_order)
#' }
#'
table_reorder <- function(data, reorder = 1:nrow(data)) {
  # Assign a unique number to the current sort state
  current_order <- 1:nrow(data)

  # Rearrange rows of data in a selected order
  reordered_data <- data[current_order[reorder], ]

  return(reordered_data)
}
