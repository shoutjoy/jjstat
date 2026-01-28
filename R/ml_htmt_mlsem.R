#' HTMT for Multilevel SEM
#'
#' This function computes the Heterotrait–Monotrait ratio (HTMT)
#' for multilevel CFA/SEM models estimated using lavaan.
#' HTMT is calculated separately for within-level or between-level
#' measurement models based on observed-variable correlations.
#'
#' @param fit A lavaan object fitted with a multilevel CFA/SEM model.
#' @param level Character. One of \code{"within"} or \code{"between"}.
#' @param digits Integer. Number of digits to round the HTMT values.
#'
#' @return A data.frame containing the HTMT matrix
#' (lower triangle only; diagonal and upper triangle are NA).
#'
#' @details
#' HTMT is defined as the ratio of the mean of heterotrait–heteromethod
#' correlations to the geometric mean of monotrait–heteromethod
#' correlations.
#'
#' Only latent variables with at least two indicators are included.
#' The function uses level-specific observed-variable correlation
#' matrices extracted via \code{lavInspect(fit, "cor.ov")}.
#'
#' @export
#'
#' @references
#' Henseler, J., Ringle, C. M., & Sarstedt, M. (2015).
#' A new criterion for assessing discriminant validity in
#' variance-based structural equation modeling.
#' Journal of the Academy of Marketing Science, 43(1), 115–135.
#'
#' @examples
#' \dontrun{
#' fit <- lavaan::sem(model, data = Demo.twolevel, cluster = "cluster")
#'
#' mlsem_htmt(fit, level = "within")
#' mlsem_htmt(fit, level = "between")
#' }
mlsem_htmt <- function(fit,
                       level = c("within", "between"),
                       digits = 3) {

  level <- match.arg(level)
  lv_idx <- ifelse(level == "within", 1, 2)

  ## -------------------------
  ## parameter table
  ## -------------------------
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  ## -------------------------
  ## measurement model
  ## -------------------------
  meas <- pe |>
    dplyr::filter(block == lv_idx, op == "=~") |>
    dplyr::select(Latent = lhs, Item = rhs)

  lv_use <- meas |>
    dplyr::count(Latent, name = "k") |>
    dplyr::filter(k >= 2) |>
    dplyr::pull(Latent)

  if (length(lv_use) < 2) {
    stop("At least two latent variables with >= 2 indicators are required.")
  }

  meas <- meas |> dplyr::filter(Latent %in% lv_use)

  ## -------------------------
  ## observed-variable correlations (by level)
  ## -------------------------
  cor_ov <- lavaan::lavInspect(fit, "cor.ov")

  if (is.list(cor_ov)) {
    cor_ov <- cor_ov[[lv_idx]]
  }

  ## -------------------------
  ## HTMT calculation
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

        it_i <- meas |> dplyr::filter(Latent == lv_use[i]) |> dplyr::pull(Item)
        it_j <- meas |> dplyr::filter(Latent == lv_use[j]) |> dplyr::pull(Item)

        mono_i <- mean(
          abs(cor_ov[it_i, it_i][upper.tri(cor_ov[it_i, it_i])]),
          na.rm = TRUE
        )

        mono_j <- mean(
          abs(cor_ov[it_j, it_j][upper.tri(cor_ov[it_j, it_j])]),
          na.rm = TRUE
        )

        hetero <- mean(
          abs(cor_ov[it_i, it_j]),
          na.rm = TRUE
        )

        HTMT[i, j] <- hetero / sqrt(mono_i * mono_j)
      }
    }
  }

  ## -------------------------
  ## output
  ## -------------------------
  out <- as.data.frame(round(HTMT, digits))
  out <- cbind(Latent = rownames(out), out)
  rownames(out) <- NULL

  return(out)
}




#' HTMT for Multilevel SEM
#'
#' This function computes the Heterotrait–Monotrait ratio (HTMT)
#' for multilevel CFA/SEM models estimated using lavaan.
#' HTMT is calculated separately for within-level or between-level
#' measurement models based on observed-variable correlations.
#'
#' @param fit A lavaan object fitted with a multilevel CFA/SEM model.
#' @param level Character. One of \code{"within"} or \code{"between"}.
#' @param digits Integer. Number of digits to round the HTMT values.
#'
#' @return A data.frame containing the HTMT matrix
#' (lower triangle only; diagonal and upper triangle are NA).
#'
#' @details
#' HTMT is defined as the ratio of the mean of heterotrait–heteromethod
#' correlations to the geometric mean of monotrait–heteromethod
#' correlations.
#'
#' Only latent variables with at least two indicators are included.
#' The function uses level-specific observed-variable correlation
#' matrices extracted via \code{lavInspect(fit, "cor.ov")}.
#'
#' @export
#'
#' @references
#' Henseler, J., Ringle, C. M., & Sarstedt, M. (2015).
#' A new criterion for assessing discriminant validity in
#' variance-based structural equation modeling.
#' Journal of the Academy of Marketing Science, 43(1), 115–135.
#'
#' @examples
#' \dontrun{
#' fit <- lavaan::sem(model, data = Demo.twolevel, cluster = "cluster")
#'
#' mlsem_htmt(fit, level = "within")
#' mlsem_htmt(fit, level = "between")
#' }
htmt_mlsem <- function(fit,
                       level = c("within", "between"),
                       digits = 3) {

  level <- match.arg(level)
  lv_idx <- ifelse(level == "within", 1, 2)

  ## -------------------------
  ## parameter table
  ## -------------------------
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  ## -------------------------
  ## measurement model
  ## -------------------------
  meas <- pe |>
    dplyr::filter(block == lv_idx, op == "=~") |>
    dplyr::select(Latent = lhs, Item = rhs)

  lv_use <- meas |>
    dplyr::count(Latent, name = "k") |>
    dplyr::filter(k >= 2) |>
    dplyr::pull(Latent)

  if (length(lv_use) < 2) {
    stop("At least two latent variables with >= 2 indicators are required.")
  }

  meas <- meas |> dplyr::filter(Latent %in% lv_use)

  ## -------------------------
  ## observed-variable correlations (by level)
  ## -------------------------
  cor_ov <- lavaan::lavInspect(fit, "cor.ov")

  if (is.list(cor_ov)) {
    cor_ov <- cor_ov[[lv_idx]]
  }

  ## -------------------------
  ## HTMT calculation
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

        it_i <- meas |> dplyr::filter(Latent == lv_use[i]) |> dplyr::pull(Item)
        it_j <- meas |> dplyr::filter(Latent == lv_use[j]) |> dplyr::pull(Item)

        mono_i <- mean(
          abs(cor_ov[it_i, it_i][upper.tri(cor_ov[it_i, it_i])]),
          na.rm = TRUE
        )

        mono_j <- mean(
          abs(cor_ov[it_j, it_j][upper.tri(cor_ov[it_j, it_j])]),
          na.rm = TRUE
        )

        hetero <- mean(
          abs(cor_ov[it_i, it_j]),
          na.rm = TRUE
        )

        HTMT[i, j] <- hetero / sqrt(mono_i * mono_j)
      }
    }
  }

  ## -------------------------
  ## output
  ## -------------------------
  out <- as.data.frame(round(HTMT, digits))
  out <- cbind(Latent = rownames(out), out)
  rownames(out) <- NULL

  return(out)
}
