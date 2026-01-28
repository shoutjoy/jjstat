
#' Compute Composite Reliability (CR) and AVE from standardized loadings
#'
#' 표준화 요인부하량(lambda)을 이용하여
#' 합성신뢰도(CR)와 평균분산추출지수(AVE)를 계산한다.
#'
#' @param lambda Numeric vector.
#'   표준화 요인부하량(standardized factor loadings).
#'
#' @param na.rm Logical.
#'   결측값 제거 여부. 기본값은 TRUE.
#'
#' @param check Logical.
#'   TRUE일 경우 입력값의 범위(|lambda| ≤ 1)를 점검하고 경고를 출력한다.
#'
#' @return A named numeric vector with elements:
#'   \itemize{
#'     \item CR: Composite Reliability
#'     \item AVE: Average Variance Extracted
#'   }
#'
#' @references
#' Fornell, C., & Larcker, D. F. (1981).
#' Evaluating structural equation models with unobservable variables
#' and measurement error. Journal of Marketing Research, 18(1), 39–50.
#'
#' Hair, J. F., Hult, G. T. M., Ringle, C. M., & Sarstedt, M. (2019).
#' A Primer on Partial Least Squares Structural Equation Modeling (PLS-SEM).
#'
#' @export
#'
#' @examples
#' lambda <- c(0.72, 0.81, 0.76)
#' cr_ave_from_lambda(lambda)
#'
#' lambda_bad <- c(1.05, 0.80)
#' cr_ave_from_lambda(lambda_bad, check = TRUE)
#'
cr_ave_from_lambda <- function(lambda,
                               na.rm = TRUE,
                               check = TRUE) {

  # -------------------------
  # 1. 입력값 점검
  # -------------------------
  if (!is.numeric(lambda)) {
    stop("lambda must be a numeric vector.")
  }

  if (na.rm) {
    lambda <- lambda[!is.na(lambda)]
  }

  if (length(lambda) < 2) {
    return(c(CR = NA_real_, AVE = NA_real_))
  }

  if (check && any(abs(lambda) > 1)) {
    warning("Some loadings exceed |1|. Check whether loadings are standardized.")
  }

  # -------------------------
  # 2. 오차분산 계산
  # -------------------------
  theta <- 1 - lambda^2

  # -------------------------
  # 3. CR, AVE 계산
  # -------------------------
  CR  <- (sum(lambda)^2) / ((sum(lambda)^2) + sum(theta))
  AVE <- sum(lambda^2) / (sum(lambda^2) + sum(theta))

  # -------------------------
  # 4. 결과 반환
  # -------------------------
  c(CR = CR, AVE = AVE)
}
