#' na_remover row unite
#'
#' @param df_var df vector
#' @param pattern  ""
#' @param type data
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
na_remover <- function(df_var, pattern="", type="data") {

  # "" 값을 NA로 변환
  df_var[df_var == pattern] <- NA

  # NA 값 제거 및 NA 처리된 인덱스 추출
  cleaned_data <- na.omit(df_var)
  na_action_indices <- attr(cleaned_data, "na.action")

  # NA로 처리된 항목의 개수 계산
  na_count <- ifelse(is.null(na_action_indices), 0, length(na_action_indices))

  # switch로 type에 따라 다른 결과 반환
  switch(type,
         data = cleaned_data,
         na_count = na_count,
         na_row = na_action_indices
  )
}
