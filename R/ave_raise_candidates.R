
#' ave_raise_candidates: 잠재변수별 AVE 향상 후보 문항 탐색
#'
#' lavaan 모형 결과를 기반으로 각 잠재변수별로 문항 제거 시
#' AVE(Average Variance Extracted)의 변화량을 계산합니다.
#' 모든 잠재변수를 자동으로 분석하거나, 특정 잠재변수만 선택할 수 있습니다.
#'
#' @param fit lavaan::cfa 또는 lavaan::sem 결과 객체
#' @param latent 분석 대상 잠재변수 이름(문자열 벡터).
#'        NULL이면 모델 내 모든 잠재변수를 자동 분석합니다.
#' @param cutoff AVE 변화량(delta)의 최소 기준. 기본값은 -Inf.
#' @param print_result 논리값. TRUE이면 잠재변수별 요약 메시지를 출력합니다.
#'
#' @return tibble
#' \describe{
#'   \item{latent}{잠재변수 이름}
#'   \item{Item}{문항 이름}
#'   \item{lambda}{표준화 요인적재량}
#'   \item{ave_current}{현재 AVE}
#'   \item{ave_if_dropped}{문항 제거 시 AVE}
#'   \item{delta}{AVE 변화량 (제거 후 - 제거 전)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' library(dplyr)
#' library(purrr)
#' library(magrittr)
#'
#' #----------------------------------------
#' # 1. CFA 모델 적합
#' #----------------------------------------
#' model <- '
#'   Visual  =~ x1 + x2 + x3
#'   Textual =~ x4 + x5 + x6
#'   Speed   =~ x7 + x8 + x9
#' '
#' fit <- cfa(model, data = HolzingerSwineford1939, std.lv = TRUE)
#'
#' #----------------------------------------
#' # 2. 모든 잠재변수 자동 분석
#' #----------------------------------------
#' ave_raise_candidates(fit)
#'
#' #----------------------------------------
#' # 3. 특정 잠재변수만 분석
#' #----------------------------------------
#' ave_raise_candidates(fit, latent = "Visual")
#'
#' #----------------------------------------
#' # 4. AVE가 0.02 이상 증가하는 경우만 필터링
#' #----------------------------------------
#' ave_raise_candidates(fit, cutoff = 0.02)
#'
#' #----------------------------------------
#' # 5. 여러 잠재변수 선택 분석
#' #----------------------------------------
#' ave_raise_candidates(fit, latent = c("Visual", "Textual"))
#' }
ave_raise_candidates <- function(fit,
                                 latent = NULL,
                                 cutoff = -Inf,
                                 print_result = TRUE) {

  # ---------------------------------------------------------------------------
  # [0] 의존 패키지 존재 여부 확인
  # - 본 함수는 dplyr/purrr/magrittr를 "직접 attach"하지 않고(requireNamespace)
  #   네임스페이스로만 사용합니다.
  # - 따라서 설치 여부만 확인하고, 없으면 즉시 중단합니다.
  # ---------------------------------------------------------------------------
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("purrr", quietly = TRUE) ||
      !requireNamespace("magrittr", quietly = TRUE)) {
    stop("dplyr, purrr, magrittr 패키지가 필요합니다.")
  }

  # ---------------------------------------------------------------------------
  # [1] lavaan 결과에서 모수추정치 테이블(parameterEstimates) 추출
  # - standardized = TRUE 로 표준화 추정치를 함께 가져오며,
  # - 본 함수는 std.all(완전 표준화 추정치)을 요인적재량(lambda)로 사용합니다.
  # - 즉, 각 잠재변수 =~ 문항 관계의 표준화 적재량을 기반으로 AVE를 계산합니다.
  # ---------------------------------------------------------------------------
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  # ---------------------------------------------------------------------------
  # [2] 분석할 잠재변수(latent_vars) 결정
  # - latent 인자가 NULL이면: 모델 내 모든 잠재변수(lhs, op="=~")를 자동 수집
  # - latent 인자가 주어지면: 모델에 실제 존재하는 잠재변수인지 검증 후 사용
  # ---------------------------------------------------------------------------
  latent_vars <- if (is.null(latent)) {

    # 모델 내 측정모형 관계(lhs =~ rhs)에서 lhs(잠재변수) 목록 추출
    unique(pe$lhs[pe$op == "=~"])

  } else {

    # 모델에 존재하는 잠재변수 목록
    valid_latent <- unique(pe$lhs[pe$op == "=~"])

    # 사용자가 넣은 latent 중, 모델에 없는 것이 있으면 에러
    if (!all(latent %in% valid_latent)) {
      stop("모형에 존재하지 않는 latent가 포함되어 있습니다: ",
           paste(setdiff(latent, valid_latent), collapse = ", "))
    }

    # 검증된 latent만 사용
    latent
  }

  # ---------------------------------------------------------------------------
  # [3] 잠재변수별로 AVE 변화량 계산
  # - purrr::map_dfr 로 latent_vars 각각에 대해 결과 tibble을 만든 뒤 행결합
  # - 각 잠재변수 lat에 대해:
  #   (a) 해당 잠재변수의 문항과 표준화 적재량(lambda)만 추출
  #   (b) 현재 AVE = 평균(lambda^2)로 계산
  #   (c) 각 문항 i를 제거했을 때 AVE(ave_drop) 계산
  #   (d) delta = ave_drop - ave_current 반환
  #
  # [주의]
  # - 여기서의 AVE는 "요인적재량 제곱의 평균"이라는 단순화된 형태입니다.
  #   (오차분산을 포함한 엄밀한 AVE 정의를 쓰는 경우와 다를 수 있음)
  # - 문항 수(k)가 2개면, 하나를 제거하면 1개가 남아 AVE 개념이 의미가 약해지므로
  #   ave_if_dropped를 NA로 처리합니다(현재 코드 정책).
  # ---------------------------------------------------------------------------
  results <- purrr::map_dfr(latent_vars, function(lat) {

    # -------------------------------------------------------------------------
    # [3-1] 현재 잠재변수(lat)의 측정 문항과 표준화 적재량 추출
    # - op == "=~" : 측정모형의 요인적재량 행만
    # - lhs == lat : 해당 잠재변수에 속한 문항만
    # - rhs        : 관측변수(문항) 이름
    # - std.all    : 완전 표준화 적재량(여기서는 lambda로 사용)
    # -------------------------------------------------------------------------
    pe_lat <- pe %>%
      dplyr::filter(op == "=~", lhs == lat) %>%
      dplyr::select(Item = rhs, lambda = std.all)

    # 문항 수
    k <- nrow(pe_lat)

    # -------------------------------------------------------------------------
    # [3-2] 문항 수가 2개 미만이면 제거 후보를 계산하기 어려움
    # - k < 2: 사실상 비교/제거 시나리오를 만들 수 없으므로 빈 결과 형태로 반환
    # -------------------------------------------------------------------------
    if (k < 2) {
      return(dplyr::tibble(
        latent = lat,
        Item = character(),
        lambda = numeric(),
        ave_current = NA_real_,
        ave_if_dropped = NA_real_,
        delta = NA_real_
      ))
    }

    # -------------------------------------------------------------------------
    # [3-3] 현재 AVE 계산
    # - ave_current = mean(lambda^2)
    # - na.rm=TRUE: (드물지만) 표준화 적재량이 NA인 경우를 방어
    # -------------------------------------------------------------------------
    ave_current <- mean(pe_lat$lambda^2, na.rm = TRUE)

    # -------------------------------------------------------------------------
    # [3-4] 각 문항 i를 제거했을 때의 AVE 및 변화량(delta) 계산
    # - i를 1..k 순회하며:
    #   * ave_drop: i번째 문항을 제외한 lambda^2 평균
    #   * delta   : ave_drop - ave_current
    #
    # - 단, k==2 인 경우 하나 제거하면 문항 1개만 남으므로
    #   ave_drop를 NA로 두어 과해석을 방지(현재 코드 정책).
    # -------------------------------------------------------------------------
    purrr::map_dfr(seq_len(k), function(i) {

      # i번째 문항 제거 후 AVE
      ave_drop <- if (k > 2) {
        mean(pe_lat$lambda[-i]^2, na.rm = TRUE)
      } else {
        NA_real_
      }

      # 결과 1행 tibble로 반환(문항별)
      dplyr::tibble(
        latent = lat,
        Item = pe_lat$Item[i],
        lambda = pe_lat$lambda[i],
        ave_current = ave_current,
        ave_if_dropped = ave_drop,
        delta = ave_drop - ave_current
      )
    })
  })

  # ---------------------------------------------------------------------------
  # [4] 전체 결과 후처리
  # - latent별로 delta 내림차순 정렬(AVE가 많이 증가하는 문항이 위로)
  # - cutoff 이상인 후보만 필터링
  # ---------------------------------------------------------------------------
  results <- results %>%
    dplyr::arrange(latent, dplyr::desc(delta)) %>%
    dplyr::filter(delta >= cutoff)

  # ---------------------------------------------------------------------------
  # [5] 요약 메시지 출력(선택)
  # - latent별로 후보 문항 개수를 집계해서 message로 출력
  # - 사용자가 함수 결과(tibble)와 별개로 빠르게 개수만 확인할 수 있게 함
  # ---------------------------------------------------------------------------
  if (print_result) {

    # latent별 후보 개수 산출 후 "latent: n개 문항 후보" 형태로 메시지 구성
    msg <- results %>%
      dplyr::group_by(latent) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(msg = paste0(latent, ": ", n, "개 문항 후보")) %>%
      dplyr::pull(msg)

    # 메시지 출력
    message("잠재변수별 AVE 향상 문항 후보 수:")
    message(paste(msg, collapse = "\n"))
  }

  # ---------------------------------------------------------------------------
  # [6] 최종 결과 반환
  # - latent/Item별 lambda, 현재 AVE, 제거 시 AVE, delta를 담은 tibble
  # ---------------------------------------------------------------------------
  return(results)
}

#'
#' #' ave_raise_candidates: 잠재변수별 AVE 향상 후보 문항 탐색
#' #'
#' #' lavaan 모형 결과를 기반으로 각 잠재변수별로 문항 제거 시
#' #' AVE(Average Variance Extracted)의 변화량을 계산합니다.
#' #' 모든 잠재변수를 자동으로 분석하거나, 특정 잠재변수만 선택할 수 있습니다.
#' #'
#' #' @param fit lavaan::cfa 또는 lavaan::sem 결과 객체
#' #' @param latent 분석 대상 잠재변수 이름(문자열 벡터).
#' #'        NULL이면 모델 내 모든 잠재변수를 자동 분석합니다.
#' #' @param cutoff AVE 변화량(delta)의 최소 기준. 기본값은 -Inf.
#' #' @param print_result 논리값. TRUE이면 잠재변수별 요약 메시지를 출력합니다.
#' #'
#' #' @return tibble
#' #' \describe{
#' #'   \item{latent}{잠재변수 이름}
#' #'   \item{Item}{문항 이름}
#' #'   \item{lambda}{표준화 요인적재량}
#' #'   \item{ave_current}{현재 AVE}
#' #'   \item{ave_if_dropped}{문항 제거 시 AVE}
#' #'   \item{delta}{AVE 변화량 (제거 후 - 제거 전)}
#' #' }
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' library(lavaan)
#' #' library(dplyr)
#' #' library(purrr)
#' #' library(magrittr)
#' #'
#' #' #----------------------------------------
#' #' # 1. CFA 모델 적합
#' #' #----------------------------------------
#' #' model <- '
#' #'   Visual  =~ x1 + x2 + x3
#' #'   Textual =~ x4 + x5 + x6
#' #'   Speed   =~ x7 + x8 + x9
#' #' '
#' #' fit <- cfa(model, data = HolzingerSwineford1939, std.lv = TRUE)
#' #'
#' #' #----------------------------------------
#' #' # 2. 모든 잠재변수 자동 분석
#' #' #----------------------------------------
#' #' ave_raise_candidates(fit)
#' #'
#' #' #----------------------------------------
#' #' # 3. 특정 잠재변수만 분석
#' #' #----------------------------------------
#' #' ave_raise_candidates(fit, latent = "Visual")
#' #'
#' #' #----------------------------------------
#' #' # 4. AVE가 0.02 이상 증가하는 경우만 필터링
#' #' #----------------------------------------
#' #' ave_raise_candidates(fit, cutoff = 0.02)
#' #'
#' #' #----------------------------------------
#' #' # 5. 여러 잠재변수 선택 분석
#' #' #----------------------------------------
#' #' ave_raise_candidates(fit, latent = c("Visual", "Textual"))
#' #' }
#' ave_raise_candidates <- function(fit,
#'                                  latent = NULL,
#'                                  cutoff = -Inf,
#'                                  print_result = TRUE) {
#'
#'   if (!requireNamespace("dplyr", quietly = TRUE) ||
#'       !requireNamespace("purrr", quietly = TRUE) ||
#'       !requireNamespace("magrittr", quietly = TRUE)) {
#'     stop("dplyr, purrr, magrittr 패키지가 필요합니다.")
#'   }
#'
#'   pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
#'
#'   # 분석할 잠재변수 결정
#'   latent_vars <- if (is.null(latent)) {
#'     unique(pe$lhs[pe$op == "=~"])
#'   } else {
#'     valid_latent <- unique(pe$lhs[pe$op == "=~"])
#'     if (!all(latent %in% valid_latent)) {
#'       stop("모형에 존재하지 않는 latent가 포함되어 있습니다: ",
#'            paste(setdiff(latent, valid_latent), collapse = ", "))
#'     }
#'     latent
#'   }
#'
#'   results <- purrr::map_dfr(latent_vars, function(lat) {
#'
#'     pe_lat <- pe %>%
#'       dplyr::filter(op == "=~", lhs == lat) %>%
#'       dplyr::select(Item = rhs, lambda = std.all)
#'
#'     k <- nrow(pe_lat)
#'
#'     if (k < 2) {
#'       return(dplyr::tibble(
#'         latent = lat,
#'         Item = character(),
#'         lambda = numeric(),
#'         ave_current = NA_real_,
#'         ave_if_dropped = NA_real_,
#'         delta = NA_real_
#'       ))
#'     }
#'
#'     ave_current <- mean(pe_lat$lambda^2, na.rm = TRUE)
#'
#'     purrr::map_dfr(seq_len(k), function(i) {
#'       ave_drop <- if (k > 2) {
#'         mean(pe_lat$lambda[-i]^2, na.rm = TRUE)
#'       } else {
#'         NA_real_
#'       }
#'
#'       dplyr::tibble(
#'         latent = lat,
#'         Item = pe_lat$Item[i],
#'         lambda = pe_lat$lambda[i],
#'         ave_current = ave_current,
#'         ave_if_dropped = ave_drop,
#'         delta = ave_drop - ave_current
#'       )
#'     })
#'   })
#'
#'   results <- results %>%
#'     dplyr::arrange(latent, dplyr::desc(delta)) %>%
#'     dplyr::filter(delta >= cutoff)
#'
#'   if (print_result) {
#'     msg <- results %>%
#'       dplyr::group_by(latent) %>%
#'       dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#'       dplyr::mutate(msg = paste0(latent, ": ", n, "개 문항 후보")) %>%
#'       dplyr::pull(msg)
#'
#'     message("잠재변수별 AVE 향상 문항 후보 수:")
#'     message(paste(msg, collapse = "\n"))
#'   }
#'
#'   return(results)
#' }
