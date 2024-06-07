#' plspm_ind_effect_sig
#'
#' @param boot_data bootstrap data
#' @param from from
#' @param through  era
#' @param to  to
#' @param digits 3
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # 함수 호출 예시
#' plspm_ind_effect_sig(
#'   boot_data = satpls_boot,
#'   from = "IMAG",
#'   through = c("EXPE", "QUAL", "SAT"),
#'   to = "VAL"
#' )
#'
#' }
#'
#'
#'
plspm_ind_effect_sig <- function(boot_data, from, through, to, digits = 3) {
  # 1. 부트스트랩 데이터에서 경로 추출 및 열 추가
  path_data <- boot_data$boot$paths %>%
    tibble::rownames_to_column("relationships")

  # 경로 설정
  paths <- c(from, through, to)

  # 초기 변수 설정
  est_values <- numeric()
  se_values <- numeric()
  result_paths <- character()

  # 경로별로 추정치와 표준오차 계산
  for (i in seq_along(paths)[-length(paths)]) {
    start <- paths[i]
    end <- paths[i + 1]

    path_info <- path_data %>%
      dplyr::filter(relationships == paste(start, "->", end))

    est <- path_info$Original
    se <- path_info$`Std.Error`

    est_values <- c(est_values, est)
    se_values <- c(se_values, se)
    result_paths <- c(result_paths, paste(start, "->", end))
  }

  # 전체 경로 계산
  total_est <- prod(est_values)

  # 소벨 테스트를 위한 표준오차 계산
  sobel_se <- sqrt(sum((se_values / est_values)^2)) * total_est

  # t값과 p값 계산
  t_value <- total_est / sobel_se
  p_value <- 2 * (1 - pnorm(abs(t_value)))

  # 결과 생성
  result <- data.frame(
    paths = paste(paths, collapse = " -> "),
    Est = round(total_est, digits),
    SE = round(sobel_se, digits),
    Z = round(t_value, digits),
    p.value = round(p_value, digits),
    sig = ifelse(p_value < 0.05, "Yes", "No")
  )

  return(result)
}




