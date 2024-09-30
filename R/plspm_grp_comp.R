#' plspm_grp_comp:Creating multigroup list data into a data frame
#'
#' @param lst_data grp data
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' plspm_grp_comp(teacher_compare) %>%
#'   nice_table(exclude=c("df", "sig"))%>%
#'   replace_df_rep(var_named) %>%Round(2)%>%pall()
#' }
#'
plspm_grp_comp <- function(lst_data) {
  # 모든 리스트 내 데이터 프레임을 처리하는 함수
  process_df <- function(df, df_name) {
    # 행 이름을 첫 번째 열로 변환
    df <- cbind(relationship = rownames(df), df)
    # 데이터 프레임에 df_name 추가
    df$comparison <- df_name
    # 열 이름 통일
    colnames(df) <- c("Paths", "global", "grp1", "grp2", "diff_abs", "t", "df", "p_value", "sig", "comp")
    df<- df%>%dplyr::select(
      comp, Paths, global, grp1, grp2, diff_abs, t, df, p_value, sig)
    return(df)
  }

  # 리스트의 각 데이터 프레임에 대해 함수 적용
  processed_list <- lapply(names(lst_data), function(name) {
    process_df(lst_data[[name]], name)
  })

  # 리스트의 모든 데이터 프레임을 병합
  combined_df <- do.call(rbind, processed_list)

  return(tibble(combined_df))
}
