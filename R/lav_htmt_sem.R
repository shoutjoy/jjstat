#' HTMT for Single-level SEM/CFA (lavaan)
#'
#' This function computes the Heterotrait–Monotrait ratio (HTMT)
#' for single-level CFA/SEM models fitted with \code{lavaan}.
#' The output is a lower-triangular HTMT matrix (upper triangle and diagonal are NA).
#'
#' @param fit A \code{lavaan} cfa/sem fitted object.
#' @param digits Integer. Number of digits to round the HTMT values. Default is 3.
#'
#' @return A data.frame containing the HTMT matrix (lower triangle only).
#' @export
#'
#' @details
#' HTMT is computed using observed-variable correlations from
#' \code{lavaan::lavInspect(fit, "cor.ov")}.
#' Only latent variables with at least two indicators are included.
#'
#' The HTMT for latent variables i and j is computed as:
#' HTMT_ij = mean( |r_xy| ) / { mean( |r_xx| ) * mean( |r_yy| ) } over 2
#' where r_xy are heterotrait correlations (indicators across constructs),
#' and r_xx, r_yy are monotrait correlations (within-construct indicators).
#'
#' @references
#' Henseler, J., Ringle, C. M., & Sarstedt, M. (2015).
#' A new criterion for assessing discriminant validity in variance-based
#' structural equation modeling. Journal of the Academy of Marketing Science,
#' 43(1), 115–135.
#'
#' @examples
#' \dontrun{
#' HS.model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
#'
#' htmt_sem(fit)
#' htmt_sem(fit, digits = 4)
#' }
htmt_sem <- function(fit, digits = 3) {

  ## -------------------------
  ## 0. input checks
  ## -------------------------
  if (!inherits(fit, "lavaan")) {
    stop("fit must be a lavaan object (from lavaan::cfa/sem).")
  }

  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits) || digits < 0) {
    stop("digits must be a single non-negative numeric value.")
  }
  digits <- as.integer(digits)

  ## -------------------------
  ## 1. parameter table
  ## -------------------------
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  ## -------------------------
  ## 2. measurement model (latent - indicators)
  ## -------------------------
  meas <- pe |>
    dplyr::filter(op == "=~") |>
    dplyr::select(Latent = lhs, Item = rhs)

  if (nrow(meas) == 0) {
    stop("No measurement model found (op == '=~'). Please check the lavaan model syntax.")
  }

  lv_use <- meas |>
    dplyr::count(Latent, name = "k") |>
    dplyr::filter(k >= 2) |>
    dplyr::pull(Latent)

  if (length(lv_use) < 2) {
    stop("At least two latent variables with >= 2 indicators are required.")
  }

  meas <- meas |>
    dplyr::filter(Latent %in% lv_use)

  ## -------------------------
  ## 3. observed-variable correlation matrix
  ## -------------------------
  cor_ov <- lavaan::lavInspect(fit, "cor.ov")

  if (is.list(cor_ov)) {
    stop("lavInspect(fit, 'cor.ov') returned a list. This function is for single-level models only.")
  }

  if (!is.matrix(cor_ov)) {
    stop("lavInspect(fit, 'cor.ov') did not return a correlation matrix.")
  }

  ## keep only indicators that exist in cor_ov
  meas <- meas |>
    dplyr::filter(Item %in% colnames(cor_ov))

  lv_use2 <- meas |>
    dplyr::count(Latent, name = "k") |>
    dplyr::filter(k >= 2) |>
    dplyr::pull(Latent)

  if (length(lv_use2) < 2) {
    stop("After matching indicators to cor.ov, fewer than two latent variables remain with >= 2 indicators.")
  }
  lv_use <- lv_use2
  meas <- meas |>
    dplyr::filter(Latent %in% lv_use)

  ## -------------------------
  ## 4. HTMT computation (lower triangle)
  ## -------------------------
  HTMT <- matrix(
    NA_real_,
    nrow = length(lv_use),
    ncol = length(lv_use),
    dimnames = list(lv_use, lv_use)
  )

  for (i in seq_along(lv_use)) {
    for (j in seq_along(lv_use)) {

      if (i > j) {

        it_i <- meas |>
          dplyr::filter(Latent == lv_use[i]) |>
          dplyr::pull(Item) |>
          unique()

        it_j <- meas |>
          dplyr::filter(Latent == lv_use[j]) |>
          dplyr::pull(Item) |>
          unique()

        ## monotrait means (within-construct indicator correlations)
        mono_i_mat <- cor_ov[it_i, it_i, drop = FALSE]
        mono_j_mat <- cor_ov[it_j, it_j, drop = FALSE]

        mono_i <- mean(abs(mono_i_mat[upper.tri(mono_i_mat)]), na.rm = TRUE)
        mono_j <- mean(abs(mono_j_mat[upper.tri(mono_j_mat)]), na.rm = TRUE)

        ## heterotrait mean (across-construct indicator correlations)
        hetero <- mean(abs(cor_ov[it_i, it_j, drop = FALSE]), na.rm = TRUE)

        ## guard: avoid division by 0 or NA
        denom <- sqrt(mono_i * mono_j)

        if (is.na(denom) || denom == 0) {
          HTMT[i, j] <- NA_real_
        } else {
          HTMT[i, j] <- hetero / denom
        }
      }
    }
  }

  ## -------------------------
  ## 5. output
  ## -------------------------
  out <- as.data.frame(round(HTMT, digits))
  out <- cbind(Latent = rownames(out), out)
  rownames(out) <- NULL

  return(out)
}
