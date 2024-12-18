
#' Perform Wald Tests for Multiple Hypotheses
#'
#' This function allows multiple hypotheses to be tested on a fitted model. It internally
#' uses `wald_test_general` to perform each test and combines the results into a single
#' tibble.
#'
#' @param model A fitted model object, such as an object from `lm` or `glm`.
#' @param ... Hypotheses to test. Each hypothesis can be specified as a character string
#'   (e.g., `"x1 - x2 + x3 = 0"`) or a numeric vector/matrix (e.g., `c(1, -1, 1)`).
#' @param rhs Right-hand side of the hypothesis equations. Default is `0`.
#' @param test A character string specifying the type of test to perform. Options are `"F"`
#'   (default) or `"Chisq"`.
#' @param vcov_func An optional function for specifying a custom covariance matrix. Default is `NULL`.
#' @param full Logical. If `TRUE`, returns a tibble with full results. Default is `FALSE`.
#'
#' @return A tibble with results for all tested hypotheses. Each row corresponds to one hypothesis.
#'
#' @examples
#' # Example: Multiple hypotheses with a linear model
#' set.seed(123)
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), y = rnorm(100))
#' model <- lm(y ~ x1 + x2 + x3, data = data)
#'
#' # Perform multiple Wald tests
#' result <- wald_test_general_df(model, "x1 - x2 + x3 = 0", "x1 - x2 = 0", c(1, -1, 1), test = "F", full = TRUE)
#' print(result)
#'
#' @export
wald_test_general_df <- function(model, ..., rhs = 0, test = c("F", "Chisq"), vcov_func = NULL, full = FALSE) {
  # Gather hypotheses
  hypotheses <- list(...)

  # Ensure hypotheses are not empty
  if (length(hypotheses) == 0) {
    stop("At least one hypothesis must be provided.")
  }

  # Iterate over hypotheses and perform Wald tests
  results <- lapply(hypotheses, function(hypothesis) {
    wald_test_general(
      model = model,
      hypothesis_matrix = hypothesis,
      rhs = rhs,
      test = test,
      vcov_func = vcov_func,
      full = full
    )
  })

  # Combine results into a single tibble
  combined_results <- dplyr::bind_rows(results)

  return(combined_results%>% p_mark_sig("p_value"))
}
