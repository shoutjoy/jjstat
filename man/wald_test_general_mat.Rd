% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wald_test_general_mat.R
\name{wald_test_general_mat}
\alias{wald_test_general_mat}
\title{Wald Test for Hypotheses Specified as Matrices}
\usage{
wald_test_general_mat(
  model,
  hypothesis_matrix,
  rhs = 0,
  test = c("F", "Chisq"),
  vcov_func = NULL
)
}
\arguments{
\item{model}{A fitted model object, such as an object from `lm` or `glm`.}

\item{hypothesis_matrix}{A numeric vector or matrix representing the hypothesis to test.
For example, `c(1, -1, 1)` represents the hypothesis `x1 - x2 + x3 = 0`.}

\item{rhs}{A numeric value for the right-hand side of the hypothesis equation. Default is `0`.}

\item{test}{A character string specifying the type of test to perform. Options are `"F"`
(default) or `"Chisq"`.}

\item{vcov_func}{An optional function for specifying a custom covariance matrix. Default is `NULL`.}
}
\value{
A tibble with the following columns:
  - `var_comparison`: A standardized version of the hypothesis in text form.
  - `W`: The Wald statistic.
  - `df`: Degrees of freedom for the test.
  - `p_value`: The p-value associated with the test.
  - `method`: The type of test performed (`"F"` or `"Chisq"`).
  - `mat`: A vectorized representation of the hypothesis matrix.
}
\description{
This function performs a Wald test for general linear hypotheses on a fitted model.
The hypothesis is specified as a numeric vector or matrix. The function computes
the Wald statistic and associated p-value based on the input hypothesis matrix.
}
\examples{
# Example: Linear model with Wald test
set.seed(123)
data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), y = rnorm(100))
model <- lm(y ~ x1 + x2 + x3, data = data)

# Perform Wald test with matrix-based hypothesis
result <- wald_test_general_mat(model, c(1, -1, 1), test = "F")
print(result)

# Perform Wald test with a custom covariance matrix
custom_vcov <- function(model) {
  vcov_matrix <- vcov(model)
  # Example: Inflate variance for demonstration
  vcov_matrix * 1.5
}
result_custom <- wald_test_general_mat(model, c(1, -1, 1), test = "F", vcov_func = custom_vcov)
print(result_custom)

}
