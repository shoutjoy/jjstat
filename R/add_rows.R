
#' Insert data into the required act in the dataframe
#'
#' @param df data.frame
#' @param ... data,  c(100, 200), c(200, 300),
#' @param row row position
#' @param char transpose character column
#'
#' @return dagta
#' @export
#'
#'
#' @examples
#' \dontrun{
#' #'
#' #'
#'
#'
#' df <- data.frame(A = 1:5, B = 6:10)
#' df
#' # 2번째 행부터 새 데이터(벡터 c(1,2,3,4)와 c(2,3,4,5))를 삽입
#'
#' add_rows(df, c(100, 200), c(200, 300), row = 2)
#'
#' df <- data.frame(
#'   측정변수 = c("진로목표(L01)", "준비여부(L02)", "직업결정(L03)", "부모추천(L04)", "학업성적(L05)"),
#'   Class.1 = c(0.2178, 0.3533, 0.1552, 0.2708, 0.1605),
#'   Class.2 = c(0.9888, 0.9769, 0.9910, 1.0000, 0.9804),
#'   Class.3 = c(0.9122, 0.8704, 0.9667, 0.0000, 0.6761)
#' )
#'
#' # 1행에 새 데이터를 삽입
#' new_df <- add_rows(df, c("class name", "불명확 진로인식형", "적극적 진로준비형", "조건분 진로지향형"), row = 1)
#'
#' print(new_df)
#'
#'
#' df <- data.frame(
#'   측정변수 = c("진로목표(L01)", "준비여부(L02)", "직업결정(L03)", "부모추천(L04)", "학업성적(L05)"),
#'   Class.1 = c(0.2178, 0.3533, 0.1552, 0.2708, 0.1605),
#'   Class.2 = c(0.9888, 0.9769, 0.9910, 1.0000, 0.9804),
#'   Class.3 = c(0.9122, 0.8704, 0.9667, 0.0000, 0.6761)
#' )
#'
#' # 첫 번째 행에 새 데이터를 삽입, char = TRUE로 문자형 변환
#' new_df_1 <- add_rows(df, c("class name", "불명확 진로인식형", "적극적 진로준비형", "조건분 진로지향형"), row = 1, char = TRUE)
#'
#' print(new_df_1)
#'
#' # 마지막 행에 새 데이터를 삽입, char = FALSE로 기존 타입 유지
#' new_df_2 <- add_rows(df, c("class name", "불명확 진로인식형", "적극적 진로준비형", "조건분 진로지향형"), row = 7, char = FALSE)
#'
#' print(new_df_2)
#'
#'
#' }
#'
add_rows <- function(df, ..., row = NULL, char = FALSE) {
  # 가변인자 벡터를 리스트로 받음
  new_rows <- list(...)

  # 데이터프레임을 문자형으로 변환 (char = TRUE인 경우에만)
  if (char) {
    df[] <- lapply(df, as.character)
  }

  # 각 입력 벡터가 df의 컬럼 개수와 일치하는지 확인
  for (row_data in new_rows) {
    if (length(row_data) != ncol(df)) {
      stop("Each input vector must have the same number of elements as the number of columns in the data frame.")
    }
  }

  # row가 지정되지 않았을 때 마지막 행 이후에 추가
  if (is.null(row)) {
    row <- nrow(df) + 1
  }

  # 반복해서 행을 삽입
  for (i in seq_along(new_rows)) {
    # 현재 삽입할 데이터
    new_row_data <- new_rows[[i]]

    # 벡터를 데이터프레임으로 변환하고 열 이름을 df의 열 이름과 일치시킴
    new_row_df <- as.data.frame(t(new_row_data), stringsAsFactors = FALSE)
    colnames(new_row_df) <- colnames(df)

    # 첫 번째 행에 삽입할 때 처리
    if (row == 1) {
      df <- rbind(new_row_df, df)
    }
    # 마지막 행에 삽입할 때 처리
    else if (row > nrow(df)) {
      df <- rbind(df, new_row_df)
    }
    # 중간 행에 삽입할 때 처리
    else {
      df <- rbind(df[1:(row - 1), ], new_row_df, df[row:nrow(df), ])
    }

    # 다음 행에 추가해야 하므로 row를 1 증가시킴
    row <- row + 1
  }

  return(df)
}
