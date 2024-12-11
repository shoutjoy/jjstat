#' str_cut function
#'
#' A function that modifies the values of a specified column in a data frame by cutting the string based on a specific separator (sep).
#'
#' @param df A data frame.
#' @param col The name of the column to modify in the data frame. Default is "term".
#' @param sep The separator used to cut the string. Default is "\\(".
#'
#' @return A modified data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' data <- data.frame(
#'   term = c("AI 학습 진단 및 문제은행 (예: AI 펭톡, EBS 단추)",
#'            "기타(직접입력)",
#'            "메타버스/게임화 도구 (예: Zep, 띵커벨, 퀴즈앤, Kahoot! 등)")
#' )
#'
#' # Using the function
#' result <- str_cut(data, "term")
#' print(result)
#' }

str_cut <- function(df, col = "term", sep = "\\(") {
  # Check if col exists in the data frame
  if (!col %in% names(df)) {
    stop("The specified col does not exist in the data frame.")
  }

  # Extract the part before the separator
  df[[col]] <- sub(paste0(sep, ".*$"), "", df[[col]])

  return(df)
}
