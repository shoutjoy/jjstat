#' Create a Frequency Table from Delimited Multiple Responses
#'
#' This function creates a frequency table from a column containing delimited multiple responses.
#' It accepts either a data frame with a specified column or a simple character vector.
#'
#' @param df A data frame or a character vector. If a vector is provided, it is converted to a one-column data frame named \code{response}.
#' @param col Column name or index to be processed when \code{df} is a data frame. Defaults to \code{1}.
#' @param sep A regular expression string for splitting responses. Defaults to \code{",\\s*"} (comma followed by optional spaces).
#'
#' @return A data frame with two columns: \code{item} and \code{n}, where \code{item} is the response item and \code{n} is the count.
#' @export
#'
#' @examples
#' # Example 1: Using a character vector
#' responses <- c("apple, banana", "banana, apple", "banana", "orange, apple")
#' Freq_table_multi(responses)
#'
#' # Example 2: Using a data frame column with different separator
#' df <- data.frame(Q1 = c("cat / dog", "dog", "cat / bird", "bird / dog"))
#' Freq_table_multi(df, col = "Q1", sep = "\\s*/\\s*")
Freq_table_multi <- function(df, col = 1, sep = ",\\s*") {
  library(tidyverse)

  # 벡터가 들어올 경우 데이터프레임으로 변환
  if (is.atomic(df)) {
    df <- data.frame(response = df)
    col <- "response"
  }

  # 열 번호를 변수명으로 변환
  if (is.numeric(col)) col <- names(df)[col]

  # 항목 분리 및 빈도 계산
  df_long <- df %>%
    separate_rows(!!sym(col), sep = sep) %>%
    mutate(!!col := str_trim(!!sym(col))) %>%
    filter(!is.na(!!sym(col)) & !!sym(col) != "") %>%
    count(!!sym(col), name = "n", sort = TRUE) %>%
    rename(item = !!sym(col))

  return(df_long)
}
