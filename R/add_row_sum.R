#' add_row_sum, row sum
#'
#' @param df df
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' mtcars$cyl %>% Freq_table(prop = TRUE) %>% addrow_sum()
#' }
add_row_sum <- function(df) {
  # Freq와 prop(%)의 합을 계산
  total_freq <- sum(df$Freq, na.rm = TRUE)
  total_prop <- sum(df$`prop(%)`, na.rm = TRUE)

  # 합계를 나타내는 행을 추가
  total_row <- tibble(term = "합계", Freq = total_freq, `prop(%)` = total_prop)

  # 원래 데이터에 합계 행을 추가하여 반환
  df_with_sum <- bind_rows(df, total_row)

  return(df_with_sum)
}
