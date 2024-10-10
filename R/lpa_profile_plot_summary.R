#' lpa_profile_plot summary data
#'
#' @param resl result
#' @param digits default 3
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' lpa_profile_plot(edlcarename, n_profiles = 4, model_name = "EII", gtype="trans",
#'                  fct_reorder= f_order,
#'                  size.x=16, alpha = 0.2)%>%
#'   lpa_profile_plot_summary() %>%
#'   nice_table()
#' #'
#' }
#'
lpa_profile_plot_summary <- function(resl, digits =3) {
  # resl$est의 데이터를 행과 열을 전환한 후 첫번째 행을 열이름으로 변경하고 첫번째 행을 profile로 변환
  est_transformed <- resl$est %>%
    t() %>%
    as.data.frame() %>%
    rowdata2col(1) %>%  # 별도 제작 함수 이대로 유지
    row2col("profile")  # 별도 제작 함수 이대로 유지

  # chr -> numeric으로 변환
  est_transformed <- est_transformed %>%
    mutate(across(-profile, as.numeric))

  # est 데이터에 "raw data" 라벨 추가
  est_transformed <- est_transformed %>%
    mutate(data_type = "raw data")

  # std 데이터에 "std data" 라벨 추가
  std_data <- resl$std %>%
    mutate(data_type = "std data")

  # profile 열을 소문자로 변경하여 est와 일치시킴
  std_data <- std_data %>%
    mutate(profile = tolower(profile))

  # 두 데이터를 bind_rows로 결합
  # raw and std ordered
  result <- bind_rows(est_transformed, std_data)

  # profile 열에서 빈칸 제거
  result <- result %>%
    mutate(profile = gsub(" ", "", profile))

  # data_type 열을 첫 번째 열로 이동
  result <- result %>%
    move_cols(ncol(.), 1)  # 별도 제작 함수 이대로 유지

  return(Round(result, digits))
}

