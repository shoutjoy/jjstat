#' Wald Test for Hypotheses Specified as Strings
#'
#' This function performs a Wald test for general linear hypotheses on a fitted model.
#' The hypothesis is specified as a string (e.g., `"x1 - x2 + x3 = 0"`) and is converted
#' into a matrix internally for computation. This allows for flexible specification of
#' hypotheses in an intuitive text-based format.
#'
#' @param model A fitted model object, such as an object from `lm` or `glm`.
#' @param hypothesis A character string representing the hypothesis to test.
#'   For example, `"x1 - x2 + x3 = 0"`. The right-hand side (`rhs`) is
#'   automatically incorporated into the hypothesis.
#' @param rhs A numeric value for the right-hand side of the hypothesis equation.
#'   Default is `0`.
#' @param test A character string specifying the type of test to perform. Options
#'   are `"F"` (default) or `"Chisq"`.
#' @param vcov_func An optional function for specifying a custom covariance matrix.
#'   Default is `NULL`, in which case the model's default covariance matrix is used.
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
#' # Perform Wald test with text-based hypothesis
#' result <- wald_test_general_str(model, "x1 - x2 + x3 = 0", test = "F")
#' print(result)
#'
#' @export
wald_test_general_str <- function(model, hypothesis, rhs = 0, test = c("F", "Chisq"), vcov_func = NULL) {
  # 가설 표준화 함수: "="을 기준으로 좌우를 처리
  standardize_hypothesis <- function(hypothesis) {
    if (base::grepl("=", hypothesis)) {
      # "="을 기준으로 나누기
      parts <- base::strsplit(hypothesis, "=")[[1]]
      left <- base::trimws(parts[1])  # 좌측 공백 제거
      right <- base::trimws(parts[2])  # 우측 공백 제거
      # 오른쪽 값이 0인 경우 그대로 반환
      if (right == "0") {
        return(hypothesis)
      }
      # 오른쪽에 있는 항목은 부호를 반대로 설정
      new_hypothesis <- base::paste(left, "-", right, "= 0")
      return(base::gsub("\\s+", " ", new_hypothesis))  # 공백 정리
    }
    return(hypothesis)
  }

  # 변수와 계수 추출 함수
  parse_hypothesis <- function(model, hypothesis) {
    terms <- base::setdiff(base::names(stats::coef(model)), "(Intercept)")
    matrix_terms <- base::sapply(terms, function(term) {
      match <- base::regmatches(hypothesis, base::regexec(base::paste0("([-+]?[0-9]*\\.?[0-9]*)\\s*\\*?\\s*", term), hypothesis))
      if (base::length(match[[1]]) > 1) {
        coef <- match[[1]][2]
        if (coef == "" || coef == "+") return(1)
        if (coef == "-") return(-1)
        return(base::as.numeric(coef))
      } else {
        return(0)
      }
    })
    base::matrix(matrix_terms, nrow = 1, dimnames = list(NULL, terms))
  }

  # 모델의 계수와 공분산 행렬 가져오기
  beta <- stats::coef(model)[-1]
  vcov_matrix <- if (base::is.null(vcov_func)) {
    stats::vcov(model)[-1, -1]
  } else {
    vcov_func(model)[-1, -1]
  }

  # 가설 표준화
  standardized_hypothesis <- standardize_hypothesis(hypothesis)

  # 가설 매트릭스 생성
  hypothesis_matrix <- parse_hypothesis(model, standardized_hypothesis)

  # 선형 조합 계산: H * beta
  hypothesis_value <- hypothesis_matrix %*% beta
  diff <- hypothesis_value - rhs
  wald_stat <- base::t(diff) %*% base::solve(hypothesis_matrix %*% vcov_matrix %*% base::t(hypothesis_matrix)) %*% diff

  # 자유도 및 p값 계산
  df <- base::nrow(hypothesis_matrix)
  test <- base::match.arg(test)
  if (test == "Chisq") {
    p_value <- base::as.numeric(stats::pchisq(wald_stat, df, lower.tail = FALSE))
  } else if (test == "F") {
    residual_df <- stats::df.residual(model)
    wald_stat_f <- wald_stat / df
    p_value <- base::as.numeric(stats::pf(wald_stat_f, df1 = df, df2 = residual_df, lower.tail = FALSE))
  } else {
    base::stop("Invalid test type. Use 'Chisq' or 'F'.")
  }

  # 벡터화된 가설 매트릭스 생성
  mat_as_vector <- base::paste(base::paste0(base::colnames(hypothesis_matrix), ":", hypothesis_matrix[1, ]), collapse = ", ")

  # 결과 출력
  result <- tibble::tibble(
    var_comparison = standardized_hypothesis,  # 표준화된 가설 표시
    W = base::as.numeric(wald_stat),
    df = df,
    p_value = p_value,
    method = test,
    mat = mat_as_vector  # 벡터화된 가설 매트릭스 저장
  )
  return(result)
}

#' #' Wald Test for Hypotheses Specified as Strings
#' #'
#' #' This function performs a Wald test for general linear hypotheses on a fitted model.
#' #' The hypothesis is specified as a string (e.g., `"x1 - x2 + x3 = 0"`) and is converted
#' #' into a matrix internally for computation. This allows for flexible specification of
#' #' hypotheses in an intuitive text-based format.
#' #'
#' #' @param model A fitted model object, such as an object from `lm` or `glm`.
#' #' @param hypothesis A character string representing the hypothesis to test.
#' #'   For example, `"x1 - x2 + x3 = 0"`. The right-hand side (`rhs`) is
#' #'   automatically incorporated into the hypothesis.
#' #' @param rhs A numeric value for the right-hand side of the hypothesis equation.
#' #'   Default is `0`.
#' #' @param test A character string specifying the type of test to perform. Options
#' #'   are `"F"` (default) or `"Chisq"`.
#' #' @param vcov_func An optional function for specifying a custom covariance matrix.
#' #'   Default is `NULL`, in which case the model's default covariance matrix is used.
#' #'
#' #' @return A tibble with the following columns:
#' #'   - `var_comparison`: A standardized version of the hypothesis in text form.
#' #'   - `W`: The Wald statistic.
#' #'   - `df`: Degrees of freedom for the test.
#' #'   - `p_value`: The p-value associated with the test.
#' #'   - `method`: The type of test performed (`"F"` or `"Chisq"`).
#' #'   - `mat`: A vectorized representation of the hypothesis matrix.
#' #'
#' #' @examples
#' #' # Example: Linear model with Wald test
#' #' set.seed(123)
#' #' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), y = rnorm(100))
#' #' model <- lm(y ~ x1 + x2 + x3, data = data)
#' #'
#' #' # Perform Wald test with text-based hypothesis
#' #' result <- wald_test_general_str(model, "x1 - x2 + x3 = 0", test = "F")
#' #' print(result)
#' #'
#' #' @export
#' wald_test_general_str <- function(model, hypothesis, rhs = 0, test = c("F", "Chisq"), vcov_func = NULL) {
#'   # 가설 표준화 함수: "="을 기준으로 좌우를 처리
#'   standardize_hypothesis <- function(hypothesis) {
#'     if (base::grepl("=", hypothesis)) {
#'       # "="을 기준으로 나누기
#'       parts <- base::strsplit(hypothesis, "=")[[1]]
#'       left <- base::trimws(parts[1])  # 좌측 공백 제거
#'       right <- base::trimws(parts[2])  # 우측 공백 제거
#'       # 오른쪽 값이 0인 경우 그대로 반환
#'       if (right == "0") {
#'         return(hypothesis)
#' #' Wald Test for Hypotheses Specified as Strings
#' #'
#' #' This function performs a Wald test for general linear hypotheses on a fitted model.
#' #' The hypothesis is specified as a string (e.g., `"x1 - x2 + x3 = 0"`) and is converted
#' #' into a matrix internally for computation. This allows for flexible specification of
#' #' hypotheses in an intuitive text-based format.
#' #'
#' #' @param model A fitted model object, such as an object from `lm` or `glm`.
#' #' @param hypothesis A character string representing the hypothesis to test.
#' #'   For example, `"x1 - x2 + x3 = 0"`. The right-hand side (`rhs`) is
#' #'   automatically incorporated into the hypothesis.
#' #' @param rhs A numeric value for the right-hand side of the hypothesis equation.
#' #'   Default is `0`.
#' #' @param test A character string specifying the type of test to perform. Options
#' #'   are `"F"` (default) or `"Chisq"`.
#' #' @param vcov_func An optional function for specifying a custom covariance matrix.
#' #'   Default is `NULL`, in which case the model's default covariance matrix is used.
#' #'
#' #' @return A tibble with the following columns:
#' #'   - `var_comparison`: A standardized version of the hypothesis in text form.
#' #'   - `W`: The Wald statistic.
#' #'   - `df`: Degrees of freedom for the test.
#' #'   - `p_value`: The p-value associated with the test.
#' #'   - `method`: The type of test performed (`"F"` or `"Chisq"`).
#' #'   - `mat`: A vectorized representation of the hypothesis matrix.
#' #'
#' #' @examples
#' #' # Example: Linear model with Wald test
#' #' set.seed(123)
#' #' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), y = rnorm(100))
#' #' model <- lm(y ~ x1 + x2 + x3, data = data)
#' #'
#' #' # Perform Wald test with text-based hypothesis
#' #' result <- wald_test_general_str(model, "x1 - x2 + x3 = 0", test = "F")
#' #' print(result)
#' #'
#' #' @export
#' wald_test_general_str <- function(model, hypothesis, rhs = 0, test = c("F", "Chisq"), vcov_func = NULL) {
#'   # 가설 표준화 함수: "="을 기준으로 좌우를 처리
#'   standardize_hypothesis <- function(hypothesis) {
#'     if (base::grepl("=", hypothesis)) {
#'       # "="을 기준으로 나누기
#'       parts <- base::strsplit(hypothesis, "=")[[1]]
#'       left <- base::trimws(parts[1])  # 좌측 공백 제거
#'       right <- base::trimws(parts[2])  # 우측 공백 제거
#'       # 오른쪽 값이 0인 경우 그대로 반환
#'       if (right == "0") {
#'         return(hypothesis)
#'       }
#'       # 오른쪽에 있는 항목은 부호를 반대로 설정
#'       new_hypothesis <- base::paste(left, "-", right, "= 0")
#'       return(base::gsub("\\s+", " ", new_hypothesis))  # 공백 정리
#'     }
#'     return(hypothesis)
#'   }
#'
#'   # 변수와 계수 추출 함수
#'   parse_hypothesis <- function(model, hypothesis) {
#'     terms <- base::setdiff(base::names(stats::coef(model)), "(Intercept)")
#'     matrix_terms <- base::sapply(terms, function(term) {
#'       match <- base::regmatches(hypothesis, base::regexec(base::paste0("([-+]?[0-9]*\\.?[0-9]*)\\s*\\*?\\s*", term), hypothesis))
#'       if (base::length(match[[1]]) > 1) {
#'         coef <- match[[1]][2]
#'         if (coef == "" || coef == "+") return(1)
#'         if (coef == "-") return(-1)
#'         return(base::as.numeric(coef))
#'       } else {
#'         return(0)
#'       }
#'     })
#'     base::matrix(matrix_terms, nrow = 1, dimnames = list(NULL, terms))
#'   }
#'
#'   # 모델의 계수와 공분산 행렬 가져오기
#'   beta <- stats::coef(model)[-1]
#'   vcov_matrix <- if (base::is.null(vcov_func)) {
#'     stats::vcov(model)[-1, -1]
#'   } else {
#'     vcov_func(model)[-1, -1]
#'   }
#'
#'   # 가설 표준화
#'   standardized_hypothesis <- standardize_hypothesis(hypothesis)
#'
#'   # 가설 매트릭스 생성
#'   hypothesis_matrix <- parse_hypothesis(model, standardized_hypothesis)
#'
#'   # 선형 조합 계산: H * beta
#'   hypothesis_value <- hypothesis_matrix %*% beta
#'   diff <- hypothesis_value - rhs
#'   wald_stat <- base::t(diff) %*% base::solve(hypothesis_matrix %*% vcov_matrix %*% base::t(hypothesis_matrix)) %*% diff
#'
#'   # 자유도 및 p값 계산
#'   df <- base::nrow(hypothesis_matrix)
#'   test <- base::match.arg(test)
#'   if (test == "Chisq") {
#'     p_value <- base::as.numeric(stats::pchisq(wald_stat, df, lower.tail = FALSE))
#'   } else if (test == "F") {
#'     residual_df <- stats::df.residual(model)
#'     wald_stat_f <- wald_stat / df
#'     p_value <- base::as.numeric(stats::pf(wald_stat_f, df1 = df, df2 = residual_df, lower.tail = FALSE))
#'   } else {
#'     base::stop("Invalid test type. Use 'Chisq' or 'F'.")
#'   }
#'
#'   # 벡터화된 가설 매트릭스 생성
#'   mat_as_vector <- base::paste(base::paste0(base::colnames(hypothesis_matrix), ":", hypothesis_matrix[1, ]), collapse = ", ")
#'
#'   # 결과 출력
#'   result <- tibble::tibble(
#'     var_comparison = standardized_hypothesis,  # 표준화된 가설 표시
#'     W = base::as.numeric(wald_stat),
#'     df = df,
#'     p_value = p_value,
#'     method = test,
#'     mat = mat_as_vector  # 벡터화된 가설 매트릭스 저장
#'   )
#'   return(result)
#' }
#'
