#' Model-based Cronbach's Alpha from Correlation Matrix
#'
#' This function computes Cronbach's alpha using a correlation matrix.
#' The correlation matrix can be directly provided or extracted from a
#' fitted lavaan object (e.g., CFA or SEM results).
#'
#' @param x A correlation matrix or a fitted lavaan object.
#' @param type Character. If \code{x} is a lavaan object, choose which
#'   correlation matrix to use:
#'   \describe{
#'     \item{"ov"}{Observed variable correlation matrix (default).}
#'     \item{"lv"}{Latent variable correlation matrix.}
#'   }
#' @param digits Integer. Number of digits to round the result.
#'
#' @return A numeric value of Cronbach's alpha.
#'
#' @details
#' The formula used is:
#' alpha = {k over (k - 1)} ( 1 - {trace(C) over sum(C)} )
#'
#' where k is the number of items and C is a correlation matrix.
#'
#' This alpha should be interpreted as a model-implied internal consistency
#' rather than a raw data-based reliability coefficient.
#'
#' @export
#'
#' @examples
#' ## Example 1: Using a correlation matrix
#' R <- matrix(c(
#'   1.00, 0.45, 0.38,
#'   0.45, 1.00, 0.41,
#'   0.38, 0.41, 1.00
#' ), nrow = 3, byrow = TRUE)
#' cronbach_alpha_model(R)
#'
#' ## Example 2: Using a CFA lavaan object
#' \dontrun{
#' library(lavaan)
#'
#' model <- '
#'   F =~ x1 + x2 + x3
#' '
#' fit <- cfa(model, data = mydata)
#'
#' cronbach_alpha_model(fit)
#' }
#'
cronbach_alpha <- function(x,
                                 type = c("ov","lv"),
                                 digits = 3) {

  type <- match.arg(type)

  # -------------------------
  # 1. 상관행렬 추출
  # -------------------------
  if (inherits(x, "lavaan")) {

    C <- if (type == "ov") {
      lavaan::lavInspect(x, "cor.ov")
    } else {
      lavaan::lavInspect(x, "cor.lv")
    }

  } else if (is.matrix(x)) {

    C <- x

  } else {
    stop("Input must be a correlation matrix or a lavaan object.")
  }

  # -------------------------
  # 2. 유효성 점검
  # -------------------------
  k <- ncol(C)
  if (k < 2) return(NA_real_)

  if (any(abs(diag(C) - 1) > 1e-6)) {
    warning("Diagonal elements are not all 1. Input may not be a correlation matrix.")
  }

  # -------------------------
  # 3. Cronbach's alpha 계산
  # -------------------------
  alpha <- (k / (k - 1)) * (1 - sum(diag(C)) / sum(C))

  round(alpha, digits)
}



#' Cronbach's Alpha from Model-Implied Correlation Matrix
#'
#' This function computes Cronbach's alpha using a correlation matrix.
#' It is intended for use with model-implied correlation matrices
#' (e.g., from CFA or SEM), not raw data.
#'
#' @param C A correlation matrix.
#'
#' @return A numeric value of Cronbach's alpha.
#'
#' @details
#' The formula used is:
#' alpha = {k over (k - 1)} ( 1 - {sum(diag(C)) over sum(C)} )
#'
#' where k is the number of items and C is a correlation matrix.
#'
#' This coefficient represents a model-based internal consistency estimate.
#'
#' @export
#'
#' @examples
#' ## Example 1: Simple correlation matrix
#' R <- matrix(c(
#'   1.00, 0.40, 0.35,
#'   0.40, 1.00, 0.42,
#'   0.35, 0.42, 1.00
#' ), nrow = 3, byrow = TRUE)
#'
#' cronbach_from_cor(R)
#'
#' ## Example 2: Model-implied correlation matrix
#' \dontrun{
#' library(lavaan)
#'
#' fit <- cfa(model, data = dat)
#' C <- lavInspect(fit, "cor.ov")
#' cronbach_from_cor(C)
#' }
#'
cronbach_from_cor <- function(C) {
  k <- ncol(C)
  if (k < 2) return(NA_real_)
  (k / (k - 1)) * (1 - sum(diag(C)) / sum(C))
}
