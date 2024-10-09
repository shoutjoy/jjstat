
#' 여러행으로 중복응답한 자료 빈도분석
#'
#' @param df 여려행이 있는 데이터
#' @param type res = freq_table=summary, missing = missing_list, data(remove missing)
#' @param digits 4 default
#'
#' @return data
#' @export
#'
#' @examples
#' \donrrun{

#' }
#'
#'
#'
table_df_multi <- function(df, type="res", digits=4) {

  # 결측치 행 처리 (빈 문자열만 있는 행을 삭제)
  total_rows <- nrow(df)
  num_cols <- ncol(df)                          # 열의 개수 계산
  missing_rows <- rowSums(df == "") == num_cols  # 각 행의 빈 셀 개수가 열의 개수와 같은지 확인
  missing_count <- sum(missing_rows)             # 결측치 행의 개수
  # missing_count
  df_clean <- df[!missing_rows, ]                # 결측치 행 삭제


  # 빈도와 비율 계산
  df_long <- as.data.frame(table(unlist(df_clean)))  # 데이터 프레임을 벡터로 변환 후 빈도 계산
  colnames(df_long) <- c("term", "freq")             # term과 freq로 열 이름 설정


  #첫행 제거
  df_long = df_long %>% remove_rows(1)

  df_long$prop <- round(df_long$freq / sum(df_long$freq), digits)*100    # 비율 계산

  colnames(df_long)=c("term", "Freq", "prop(%)")

  #결과
  freq_table = df_long
  # # 결과 출력
  message("전체 행의 개수: ", total_rows)
  message("결측치 행의 개수: ", missing_count)
  message("데이터개수: ", total_rows - missing_count)
  # message("결측치 행 목록: ", paste(missing_row_names, collapse=", "))



  missing_row_names <- which(missing_rows)

  res = (list(freq_table = df_long,
              missing_rows = missing_row_names))
  missing_list =       missing_rows = missing_row_names



  switch(type, res= freq_table,
         freq_table= freq_table,
         missing = missing_list,
         missing_list = missing_list,
         summary = freq_table,
         data = df_clean)
}
