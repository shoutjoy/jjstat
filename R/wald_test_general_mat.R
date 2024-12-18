#' Wald Test for Hypotheses Specified as Matrices
#'
#' This function performs a Wald test for general linear hypotheses on a fitted model.
#' The hypothesis is specified as a numeric vector or matrix. The function computes
#' the Wald statistic and associated p-value based on the input hypothesis matrix.
#'
#' @param model A fitted model object, such as an object from `lm` or `glm`.
#' @param hypothesis_matrix A numeric vector or matrix representing the hypothesis to test.
#'   For example, `c(1, -1, 1)` represents the hypothesis `x1 - x2 + x3 = 0`.
#' @param rhs A numeric value for the right-hand side of the hypothesis equation. Default is `0`.
#' @param test A character string specifying the type of test to perform. Options are `"F"`
#'   (default) or `"Chisq"`.
#' @param vcov_func An optional function for specifying a custom covariance matrix. Default is `NULL`.
#'
#' @return A tibble with the following columns:
#'   - `var_comparison`: A standardized version of the hypothesis in text form.
#'   - `W`: The Wald statistic.
#'   - `df`: Degrees of freedom for the test.
#'   - `p_value`: The p-value associated with the test.
#'   - `method`: The type of test performed (`"F"` or `"Chisq"`).
#'   - `mat`: A vectorized representation of the hypothesis matrix.
#'
#' @examples
#' # Example: Linear model with Wald test
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), y = rnorm(100))
#' model <- lm(y ~ x1 + x2 + x3, data = data)
#'
#' # Perform Wald test with matrix-based hypothesis
#' result <- wald_test_general_mat(model, c(1, -1, 1), test = "F")
#' print(result)
#'
#' # Perform Wald test with a custom covariance matrix
#' custom_vcov <- function(model) {
#'   vcov_matrix <- vcov(model)
#'   # Example: Inflate variance for demonstration
#'   vcov_matrix * 1.5
#' }
#' result_custom <- wald_test_general_mat(model, c(1, -1, 1), test = "F", vcov_func = custom_vcov)
#' print(result_custom)
#'
#' @export
wald_test_general_mat <- function(model, hypothesis_matrix, rhs = 0,
                                  test = c("F", "Chisq"), vcov_func = NULL) {
  # 모델의 계수와 공분산 행렬 가져오기
  beta <- coef(model)[-1]  # Intercept 제거
  vcov_matrix <- if (is.null(vcov_func)) {
    vcov(model)[-1, -1]  # Intercept 제거
  } else {
    vcov_func(model)[-1, -1]  # Intercept 제거
  }

  # Intercept 제거된 열 이름만 포함하도록 처리
  terms <- setdiff(names(coef(model)), "(Intercept)")

  # 가설 매트릭스 확장 (단순 입력을 처리)
  if (is.vector(hypothesis_matrix)) {
    hypothesis_matrix <- matrix(hypothesis_matrix, nrow = 1, ncol = length(terms), dimnames = list(NULL, terms))
  } else {
    hypothesis_matrix <- hypothesis_matrix[, terms, drop = FALSE]
  }

  # 선형 조합 계산: H * beta
  hypothesis_value <- hypothesis_matrix %*% beta
  # 차이 계산
  diff <- hypothesis_value - rhs
  # Wald 통계량 계산
  wald_stat <- t(diff) %*% solve(hypothesis_matrix %*% vcov_matrix %*% t(hypothesis_matrix)) %*% diff

  # 자유도 및 p값 계산
  df <- nrow(hypothesis_matrix)  # 가설 행렬의 행 수
  test <- match.arg(test)  # "Chisq" 또는 "F" 검정 선택
  if (test == "Chisq") {
    p_value <- as.numeric(pchisq(wald_stat, df, lower.tail = FALSE))
  } else if (test == "F") {
    residual_df <- df.residual(model)
    wald_stat_f <- wald_stat / df
    p_value <- as.numeric(pf(wald_stat_f, df1 = df, df2 = residual_df, lower.tail = FALSE))
  } else {
    stop("Invalid test type. Use 'Chisq' or 'F'.")
  }

  # 가설 매트릭스를 `x1 - x2 + x3 = 0` 형식으로 변환
  var_comparison_vector <- apply(hypothesis_matrix, 1, function(row) {
    terms <- colnames(hypothesis_matrix)
    expression <- ""
    for (i in seq_along(row)) {
      coef <- row[i]
      term <- terms[i]
      if (coef == 0) next  # 0인 항목은 생략
      if (expression == "") {  # 첫 번째 항목
        if (coef == 1) {
          expression <- term
        } else if (coef == -1) {
          expression <- paste0("-", term)
        } else {
          expression <- paste0(coef, "*", term)
        }
      } else {  # 두 번째 이후 항목
        if (coef == 1) {
          expression <- paste0(expression, " + ", term)
        } else if (coef == -1) {
          expression <- paste0(expression, " - ", term)
        } else if (coef > 0) {
          expression <- paste0(expression, " + ", coef, "*", term)
        } else {
          expression <- paste0(expression, " - ", abs(coef), "*", term)
        }
      }
    }
    paste0(expression, " = 0")  # 끝에 = 0 추가
  })

  # 행렬을 벡터화하여 문자열 형태로 저장
  mat_as_vector <- apply(hypothesis_matrix, 1, function(row) {
    paste(paste0(colnames(hypothesis_matrix), ":", row), collapse = ", ")
  })

  # 결과 출력
  result <- tibble::tibble(
    var_comparison = var_comparison_vector,  # 가설 매트릭스를 표준 문자열로 저장
    W = as.numeric(wald_stat),
    df = df,
    p_value = p_value,
    method = test,
    mat = mat_as_vector  # 벡터화된 가설 매트릭스 저장
  )
  return(result)
}
