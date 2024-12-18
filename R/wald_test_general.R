#' General Wald Test for Linear Hypotheses
#'
#' This function performs a Wald test for general linear hypotheses on a fitted model.
#' The hypothesis can be specified as a string (e.g., `"x1 - x2 + x3 = 0"`) or as
#' a numeric vector/matrix (e.g., `c(1, -1, 1)`). The function provides an option
#' for detailed output using the `full` parameter.
#'
#' @param model A fitted model object, such as an object from `lm` or `glm`.
#' @param hypothesis_matrix A hypothesis specified as either a character string (e.g., `"x1 - x2 + x3 = 0"`)
#'   or a numeric vector/matrix (e.g., `c(1, -1, 1)`).
#' @param rhs Right-hand side of the hypothesis equation. Default is `0`.
#' @param test A character string specifying the type of test to perform. Options are `"F"` (default) or `"Chisq"`.
#' @param vcov_func An optional function for specifying a custom covariance matrix. Default is `NULL`.
#' @param full Logical. If `TRUE`, returns a tibble with full results. If `FALSE`, prints summary information
#'   and returns a simplified tibble. Default is `FALSE`.
#'
#' @return A tibble with results. If `full = TRUE`, the tibble includes:
#'   - `var_comparison`: The hypothesis expressed as a standardized string.
#'   - `W`: The Wald statistic.
#'   - `df`: Degrees of freedom for the test.
#'   - `p_value`: The p-value associated with the test.
#'   - `method`: The type of test performed (`"F"` or `"Chisq"`).
#'   - `mat`: A vectorized representation of the hypothesis matrix.
#' If `full = FALSE`, the tibble includes only:
#'   - `var_comparison`: The hypothesis expressed as a standardized string.
#'   - `W`: The Wald statistic.
#'   - `df`: Degrees of freedom for the test.
#'   - `p_value`: The p-value associated with the test.
#'
#' @examples
#' # Example: Linear model with Wald test
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), y = rnorm(100))
#' model <- lm(y ~ x1 + x2 + x3, data = data)
#'
#' # Perform Wald test with text-based hypothesis
#' result1 <- wald_test_general(model, "x1 - x2 + x3 = 0", test = "F", full = TRUE)
#' print(result1)
#'
#' # Perform Wald test with matrix-based hypothesis
#' result2 <- wald_test_general(model, c(1, -1, 1), test = "F", full = TRUE)
#' print(result2)
#'
#' # Perform Wald test with custom covariance matrix
#' custom_vcov <- function(model) {
#'   vcov_matrix <- vcov(model)
#'   # Example: Inflate variance for demonstration
#'   vcov_matrix * 1.5
#' }
#' result_custom <- wald_test_general(model, c(1, -1, 1), test = "F", vcov_func = custom_vcov, full = TRUE)
#' print(result_custom)
#'
#' @export
wald_test_general <- function(model, hypothesis_matrix, rhs = 0, test = c("F", "Chisq"), vcov_func = NULL, full = FALSE) {
  # 가설 표준화 함수: "="을 기준으로 좌우를 처리
  standardize_hypothesis <- function(hypothesis) {
    if (grepl("=", hypothesis)) {
      # "="을 기준으로 나누기
      parts <- strsplit(hypothesis, "=")[[1]]
      left <- trimws(parts[1])  # 좌측 공백 제거
      right <- trimws(parts[2])  # 우측 공백 제거
      # 오른쪽 값이 0인 경우 그대로 반환
      if (right == "0") {
        return(hypothesis)
      }
      # 오른쪽에 있는 항목은 부호를 반대로 설정
      new_hypothesis <- paste(left, "-", right, "= 0")
      return(gsub("\\s+", " ", new_hypothesis))  # 공백 정리
    }
    return(hypothesis)
  }

  # 변수와 계수 추출 함수
  parse_hypothesis <- function(model, hypothesis) {
    terms <- setdiff(names(coef(model)), "(Intercept)")
    matrix_terms <- sapply(terms, function(term) {
      match <- regmatches(hypothesis, regexec(paste0("([-+]?[0-9]*\\.?[0-9]*)\\s*\\*?\\s*", term), hypothesis))
      if (length(match[[1]]) > 1) {
        coef <- match[[1]][2]
        if (coef == "" || coef == "+") return(1)
        if (coef == "-") return(-1)
        return(as.numeric(coef))
      } else {
        return(0)
      }
    })
    matrix(matrix_terms, nrow = 1, dimnames = list(NULL, terms))
  }

  # 모델의 계수와 공분산 행렬 가져오기
  beta <- coef(model)[-1]  # Intercept 제거
  vcov_matrix <- if (is.null(vcov_func)) {
    vcov(model)[-1, -1]  # Intercept 제거
  } else {
    vcov_func(model)[-1, -1]  # Intercept 제거
  }

  # 입력 유형에 따라 처리
  if (is.character(hypothesis_matrix)) {
    # 문자열 입력 처리
    standardized_hypothesis <- standardize_hypothesis(hypothesis_matrix)
    hypothesis_matrix <- parse_hypothesis(model, standardized_hypothesis)
    var_comparison <- standardized_hypothesis
  } else if (is.vector(hypothesis_matrix) || is.matrix(hypothesis_matrix)) {
    # 벡터/행렬 입력 처리
    terms <- setdiff(names(coef(model)), "(Intercept)")
    if (is.vector(hypothesis_matrix)) {
      hypothesis_matrix <- matrix(hypothesis_matrix, nrow = 1, ncol = length(terms), dimnames = list(NULL, terms))
    } else {
      hypothesis_matrix <- hypothesis_matrix[, terms, drop = FALSE]
    }
    var_comparison <- apply(hypothesis_matrix, 1, function(row) {
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
  } else {
    stop("Invalid input for hypothesis_matrix. Must be a character or numeric vector/matrix.")
  }

  # 선형 조합 계산: H * beta
  hypothesis_value <- hypothesis_matrix %*% beta
  diff <- hypothesis_value - rhs
  # Wald 통계량 계산
  wald_stat <- t(diff) %*% solve(hypothesis_matrix %*% vcov_matrix %*% t(hypothesis_matrix)) %*% diff

  # 자유도 및 p값 계산
  df <- nrow(hypothesis_matrix)
  test <- match.arg(test)
  if (test == "Chisq") {
    p_value <- as.numeric(pchisq(wald_stat, df, lower.tail = FALSE))
  } else if (test == "F") {
    residual_df <- df.residual(model)
    wald_stat_f <- wald_stat / df
    p_value <- as.numeric(pf(wald_stat_f, df1 = df, df2 = residual_df, lower.tail = FALSE))
  } else {
    stop("Invalid test type. Use 'Chisq' or 'F'.")
  }

  # 벡터화된 가설 매트릭스 생성
  mat_as_vector <- paste(paste0(colnames(hypothesis_matrix), ": ", hypothesis_matrix[1, ]), collapse = ", ")

  if (full) {
    # 결과 출력
    result <- tibble::tibble(
      var_comparison = var_comparison,  # 표준화된 가설 표시
      W = as.numeric(wald_stat),
      df = df,
      p_value = p_value,
      method = test,
      mat = mat_as_vector  # 벡터화된 가설 매트릭스 저장
    )
  } else {
    # 간단 출력
    cat("\n", paste0("Test Method:", test, ", hypothesis Mat [", mat_as_vector, "]"), "\n\n")
    result <- tibble::tibble(
      var_comparison = var_comparison,  # 표준화된 가설 표시
      W = as.numeric(wald_stat),
      df = df,
      p_value = p_value
    )
  }
  return(result)
}
