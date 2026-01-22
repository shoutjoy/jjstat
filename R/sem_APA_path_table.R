#' Create an APA-style path coefficient table from a lavaan SEM object
#'
#' This function extracts structural path coefficients from a fitted
#' \code{lavaan} SEM object and returns an APA-style summary table
#' including unstandardized estimates, standard errors, standardized
#' coefficients, p-values, and significance symbols.
#'
#' @param fit A fitted \code{lavaan} SEM object (e.g., from \code{sem()} or \code{cfa()}).
#' @param only_sig Logical. If \code{TRUE}, only statistically significant
#'   paths (p < .05) are returned. Default is \code{FALSE}.
#'
#' @return A data.frame containing:
#' \describe{
#'   \item{Path}{Structural path in the form "Outcome <- Predictor"}
#'   \item{B}{Unstandardized regression coefficient}
#'   \item{SE}{Standard error of the coefficient}
#'   \item{Beta}{Standardized regression coefficient (std.all)}
#'   \item{p}{Formatted p-value}
#'   \item{sig}{Significance symbol (* p < .05, ** p < .01, *** p < .001)}
#' }
#'
#' @details
#' Only regression paths (operator \code{"~"}) are included.
#' Covariances, variances, and intercepts are excluded.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #--------------------------------------------------
#' # Example 1: Simple mediation model
#' #--------------------------------------------------
#' library(lavaan)
#'
#' model_med <- '
#'   M ~ a*X
#'   Y ~ b*M + c*X
#' '
#'
#' fit_med <- sem(model_med, data = mydata)
#'
#' # All structural paths
#' sem_APA_path_table(fit_med)
#'
#' # Only significant paths
#' sem_APA_path_table(fit_med, only_sig = TRUE)
#'
#' #--------------------------------------------------
#' # Example 2: Multiple predictors
#' #--------------------------------------------------
#' model_multi <- '
#'   Y ~ X1 + X2 + X3
#' '
#'
#' fit_multi <- sem(model_multi, data = mydata)
#'
#' sem_APA_path_table(fit_multi)
#'
#' #--------------------------------------------------
#' # Example 3: Full SEM with latent variables
#' #--------------------------------------------------
#' model_sem <- '
#'   F1 =~ x1 + x2 + x3
#'   F2 =~ y1 + y2 + y3
#'
#'   F2 ~ F1
#' '
#'
#' fit_sem_full <- sem(model_sem, data = mydata)
#'
#' sem_APA_path_table(fit_sem_full)
#' sem_APA_path_table(fit_sem_full, only_sig = TRUE)
#' }
sem_APA_path_table <- function(fit, only_sig = FALSE) {

  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  path_tbl <- subset(pe, op == "~")

  # Significance symbols
  sig_symbol <- ifelse(path_tbl$pvalue < .001, "***",
                       ifelse(path_tbl$pvalue < .01, "**",
                              ifelse(path_tbl$pvalue < .05, "*", "")))

  out <- data.frame(
    Path = paste(path_tbl$lhs, "<-", path_tbl$rhs),
    B = round(path_tbl$est, 3),
    SE = round(path_tbl$se, 3),
    Beta = round(path_tbl$std.all, 3),
    p = ifelse(path_tbl$pvalue < .001, "< .001",
               round(path_tbl$pvalue, 3)),
    sig = sig_symbol,
    stringsAsFactors = FALSE
  )

  if (only_sig) {
    out <- out[out$sig != "", ]
  }

  rownames(out) <- NULL
  out
}
