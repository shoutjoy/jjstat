#' Table and Summary Function for Data Frames
#'
#' This function processes a data frame to clean missing rows, calculates the frequency
#' and proportion of terms, and provides different outputs based on the selected type.
#'
#' @param df A data frame to process.
#' @param type A character string indicating the type of result to return. Options are:
#'   \itemize{
#'     \item \code{"res"}: Returns the frequency table (default).
#'     \item \code{"freq_table"}: Returns the frequency table.
#'     \item \code{"missing"}: Returns the list of missing rows.
#'     \item \code{"missing_list"}: Returns the list of missing rows.
#'     \item \code{"summary"}: Returns a summary of the frequency table.
#'     \item \code{"data"}: Returns the cleaned data frame.
#'   }
#' @param digits An integer specifying the number of decimal places to round proportions. Default is 4.
#' @param msg A logical value. If \code{TRUE}, displays messages about the process. Default is \code{TRUE}.
#'
#' @return Depending on the \code{type} parameter:
#'   \itemize{
#'     \item A frequency table (\code{"res"}, \code{"freq_table"}, or \code{"summary"}).
#'     \item A list of indices of missing rows (\code{"missing"} or \code{"missing_list"}).
#'     \item The cleaned data frame (\code{"data"}).
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Survey Data with Repeated Questions
#' # Survey data includes repeated questions with different phrasing
#' survey_data <- data.frame(
#'   question1 = c("Yes", "No", "Yes", "", "No"),
#'   question2 = c("No", "Yes", "Yes", "", ""),
#'   question3 = c("Yes", "", "No", "Yes", "No")
#' )
#'
#' # Process the survey data to identify duplicate responses
#' result <- table_df_multi(survey_data, type = "res", digits = 2, msg = TRUE)
#'
#' # Output the frequency table
#' print(result)
#'
#' # Additional example to find missing rows
#' missing_info <- table_df_multi(survey_data, type = "missing", msg = FALSE)
#' print(missing_info)
#' }
table_df_multi <- function(df, type = "res", digits = 4, msg = TRUE) {

  # 결측치 행 처리 (빈 문자열만 있는 행을 삭제)
  total_rows <- nrow(df)
  num_cols <- ncol(df)                          # 열의 개수 계산
  missing_rows <- rowSums(df == "") == num_cols  # 각 행의 빈 셀 개수가 열의 개수와 같은지 확인
  missing_count <- sum(missing_rows)             # 결측치 행의 개수
  df_clean <- df[!missing_rows, ]                # 결측치 행 삭제

  # 빈도와 비율 계산
  df_long <- as.data.frame(table(unlist(df_clean)))  # 데이터 프레임을 벡터로 변환 후 빈도 계산
  colnames(df_long) <- c("term", "freq")             # term과 freq로 열 이름 설정

  # 첫 행 제거
  df_long <- df_long[-1, ]

  df_long$prop <- round(df_long$freq / sum(df_long$freq), digits) * 100  # 비율 계산
  colnames(df_long) <- c("term", "Freq", "prop(%)")

  # 결과
  freq_table <- df_long

  if (msg) {
    message("전체 행의 개수: ", total_rows)
    message("결측치 행의 개수: ", missing_count)
    message("데이터개수: ", total_rows - missing_count)
  }

  missing_row_names <- which(missing_rows)

  res <- list(
    freq_table = df_long,
    missing_rows = missing_row_names
  )

  missing_list <- missing_row_names

  switch(
    type,
    res = freq_table,
    freq_table = freq_table,
    missing = missing_list,
    missing_list = missing_list,
    summary = freq_table,
    data = df_clean
  )
}

#' Table and Summary Function for Data Frames
#'
#' This function processes a data frame to clean missing rows, calculates the frequency
#' and proportion of terms, and provides different outputs based on the selected type.
#'
#' @param df A data frame to process.
#' @param type A character string indicating the type of result to return. Options are:
#'   \itemize{
#'     \item \code{"res"}: Returns the frequency table (default).
#'     \item \code{"freq_table"}: Returns the frequency table.
#'     \item \code{"missing"}: Returns the list of missing rows.
#'     \item \code{"missing_list"}: Returns the list of missing rows.
#'     \item \code{"summary"}: Returns a summary of the frequency table.
#'     \item \code{"data"}: Returns the cleaned data frame.
#'   }
#' @param digits An integer specifying the number of decimal places to round proportions. Default is 4.
#' @param msg A logical value. If \code{TRUE}, displays messages about the process. Default is \code{TRUE}.
#'
#' @return Depending on the \code{type} parameter:
#'   \itemize{
#'     \item A frequency table (\code{"res"}, \code{"freq_table"}, or \code{"summary"}).
#'     \item A list of indices of missing rows (\code{"missing"} or \code{"missing_list"}).
#'     \item The cleaned data frame (\code{"data"}).
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Survey Data with Repeated Questions
#' # Survey data includes repeated questions with different phrasing
#' survey_data <- data.frame(
#'   question1 = c("Yes", "No", "Yes", "", "No"),
#'   question2 = c("No", "Yes", "Yes", "", ""),
#'   question3 = c("Yes", "", "No", "Yes", "No")
#' )
#'
#' # Process the survey data to identify duplicate responses
#' result <- Freq_table_df_multi(survey_data, type = "res", digits = 2, msg = TRUE)
#'
#' # Output the frequency table
#' print(result)
#'
#' # Additional example to find missing rows
#' missing_info <- Freq_table_df_multi(survey_data, type = "missing", msg = FALSE)
#' print(missing_info)
#' }
Freq_table_df_multi <- function(df, type = "res", digits = 4, msg = TRUE) {

  # 결측치 행 처리 (빈 문자열만 있는 행을 삭제)
  total_rows <- nrow(df)
  num_cols <- ncol(df)                          # 열의 개수 계산
  missing_rows <- rowSums(df == "") == num_cols  # 각 행의 빈 셀 개수가 열의 개수와 같은지 확인
  missing_count <- sum(missing_rows)             # 결측치 행의 개수
  df_clean <- df[!missing_rows, ]                # 결측치 행 삭제

  # 빈도와 비율 계산
  df_long <- as.data.frame(table(unlist(df_clean)))  # 데이터 프레임을 벡터로 변환 후 빈도 계산
  colnames(df_long) <- c("term", "freq")             # term과 freq로 열 이름 설정

  # 첫 행 제거
  df_long <- df_long[-1, ]

  df_long$prop <- round(df_long$freq / sum(df_long$freq), digits) * 100  # 비율 계산
  colnames(df_long) <- c("term", "Freq", "prop(%)")

  # 결과
  freq_table <- df_long

  if (msg) {
    message("전체 행의 개수: ", total_rows)
    message("결측치 행의 개수: ", missing_count)
    message("데이터개수: ", total_rows - missing_count)
  }

  missing_row_names <- which(missing_rows)

  res <- list(
    freq_table = df_long,
    missing_rows = missing_row_names
  )

  missing_list <- missing_row_names

  switch(
    type,
    res = freq_table,
    freq_table = freq_table,
    missing = missing_list,
    missing_list = missing_list,
    summary = freq_table,
    data = df_clean
  )
}




# table_df_multi <- function(df, type="res", digits=4) {
#
#   # 결측치 행 처리 (빈 문자열만 있는 행을 삭제)
#   total_rows <- nrow(df)
#   num_cols <- ncol(df)                          # 열의 개수 계산
#   missing_rows <- rowSums(df == "") == num_cols  # 각 행의 빈 셀 개수가 열의 개수와 같은지 확인
#   missing_count <- sum(missing_rows)             # 결측치 행의 개수
#   # missing_count
#   df_clean <- df[!missing_rows, ]                # 결측치 행 삭제
#
#
#   # 빈도와 비율 계산
#   df_long <- as.data.frame(table(unlist(df_clean)))  # 데이터 프레임을 벡터로 변환 후 빈도 계산
#   colnames(df_long) <- c("term", "freq")             # term과 freq로 열 이름 설정
#
#
#   #첫행 제거
#   df_long = df_long %>% remove_rows(1)
#
#   df_long$prop <- round(df_long$freq / sum(df_long$freq), digits)*100    # 비율 계산
#
#   colnames(df_long)=c("term", "Freq", "prop(%)")
#
#   #결과
#   freq_table = df_long
#   # # 결과 출력
#   message("전체 행의 개수: ", total_rows)
#   message("결측치 행의 개수: ", missing_count)
#   message("데이터개수: ", total_rows - missing_count)
#   # message("결측치 행 목록: ", paste(missing_row_names, collapse=", "))
#
#
#
#   missing_row_names <- which(missing_rows)
#
#   res = (list(freq_table = df_long,
#               missing_rows = missing_row_names))
#   missing_list =       missing_rows = missing_row_names
#
#
#
#   switch(type, res= freq_table,
#          freq_table= freq_table,
#          missing = missing_list,
#          missing_list = missing_list,
#          summary = freq_table,
#          data = df_clean)
# }
