#' Freq_table_colnames caange korean names
#'
#' @param df Freq_table
#' @param input new name default "구분"
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #' table_df_multit(eduteck2 %>% select(contains("Q5_"), -Q5_4_TEXT)) %>%
#' arrange(desc(Freq)) %>%Freq_table_colnames()
#' }
#'
Freq_table_colnames <- function(df, input = "구분") {
  # 존재하는 컬럼만 변경하도록 조건 처리
  res <- df %>%
    dplyr::rename(
      !!sym(input) := if("term" %in% colnames(df)) "term" else NULL,
      부분합 = if("partial_sum" %in% colnames(df)) "partial_sum" else NULL,
      빈도 = if("Freq" %in% colnames(df)) "Freq" else NULL,
      `비율(%)` = if("prop(%)" %in% colnames(df)) "prop(%)" else NULL
    ) %>%
    # 불필요한 NULL 열을 제거
    dplyr::select(where(~ !is.null(.)))

  return(res)
}
