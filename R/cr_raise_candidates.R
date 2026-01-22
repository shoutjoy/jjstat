#' cr_raise_candidates: 잠재변수별 CR 향상 후보 문항 탐색
#'
#' lavaan 모형 결과를 기반으로 각 잠재변수별로 문항 제거 시 CR 변화량을 계산합니다.
#' 모든 잠재변수를 자동 탐색하거나, 특정 잠재변수를 지정할 수 있습니다.
#'
#' @param fit lavaan::cfa 또는 sem 결과 객체
#' @param latent 분석 대상 잠재변수 이름(문자열). NULL이면 전체 잠재변수 자동 분석.
#' @param cutoff CR 변화량(delta)의 최소 기준. 기본값은 -Inf.
#' @param print_result 논리값. TRUE면 변수별 요약 메시지 출력.
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' ## 예제: lavaan 내장 데이터와 CFA 모델 사용
#' library(lavaan)
#'
#' # 모델 정의
#' model <- '
#'   Visual  =~ x1 + x2 + x3
#'   Textual =~ x4 + x5 + x6
#'   Speed   =~ x7 + x8 + x9
#' '
#'
#' # CFA 적합
#' fit <- cfa(model, data = HolzingerSwineford1939, std.lv = TRUE)
#'
#' # CR 향상 후보 탐색 (전체 변수)
#' cr_raise_candidates(fit)
#'
#' # 특정 잠재변수만 분석
#' cr_raise_candidates(fit, latent = "Visual", cutoff = 0.01)
#' }
cr_raise_candidates <- function(fit,
                                latent = NULL,
                                cutoff = -Inf, print_result = TRUE) {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("purrr", quietly = TRUE) ||
      !requireNamespace("magrittr", quietly = TRUE)) {
    stop("dplyr, purrr, magrittr 패키지가 필요합니다. 설치 후 사용해주세요.")
  }

  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  latent_vars <- if (is.null(latent)) {
    unique(pe$lhs[pe$op == "=~"])
  } else {
    if (!all(latent %in% unique(pe$lhs[pe$op == "=~"]))) {
      stop("지정한 latent 변수가 모델에 존재하지 않습니다: ",
           paste(setdiff(latent, unique(pe$lhs[pe$op == "=~"])), collapse = ", "))
    }
    latent
  }

  results <- purrr::map_dfr(latent_vars, function(lat) {
    pe_lat <- pe %>%
      dplyr::filter(op == "=~", lhs == lat) %>%
      dplyr::select(Item = rhs, lambda = std.all)

    k <- nrow(pe_lat)
    if (k < 2) {
      return(dplyr::tibble(
        latent = lat,
        Item = character(),
        lambda = numeric(),
        cr_current = NA_real_,
        cr_if_dropped = NA_real_,
        delta = NA_real_
      ))
    }

    cr <- function(lambda) {
      sum_lambda <- sum(lambda, na.rm = TRUE)
      sum_sq <- sum(lambda^2, na.rm = TRUE)
      (sum_lambda^2) / (sum_lambda^2 + sum(1 - lambda^2, na.rm = TRUE))
    }

    cr_current <- cr(pe_lat$lambda)

    purrr::map_dfr(seq_len(k), function(i) {
      cr_drop <- if (k > 2) cr(pe_lat$lambda[-i]) else NA_real_
      dplyr::tibble(
        latent = lat,
        Item = pe_lat$Item[i],
        lambda = pe_lat$lambda[i],
        cr_current = cr_current,
        cr_if_dropped = cr_drop,
        delta = cr_drop - cr_current
      )
    })
  })

  results <- results %>%
    dplyr::arrange(latent, dplyr::desc(delta)) %>%
    dplyr::filter(delta >= cutoff)

  if (print_result) {
    msg <- results %>%
      dplyr::group_by(latent) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(msg = paste0(latent, ": ", n, "개 문항 후보")) %>%
      dplyr::pull(msg)

    message("잠재변수별 CR 향상 문항 후보 수:")
    message(paste(msg, collapse = "\n"))
  }

  return(results)
}
