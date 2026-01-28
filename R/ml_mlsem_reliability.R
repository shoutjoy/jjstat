#' Multilevel SEM Reliability (Cronbach's alpha, CR, AVE)
#'
#' This function computes reliability indices for multilevel SEM models
#' fitted with lavaan. Reliability is calculated separately for within-
#' and between-level measurement models.
#'
#' The function returns Cronbach's alpha (model-based, using observed
#' correlation matrix), Composite Reliability (CR), and Average Variance
#' Extracted (AVE) based on standardized factor loadings.
#'
#' @param fit A fitted lavaan multilevel SEM object.
#' @param level Character. One of \code{"within"} or \code{"between"}.
#' @param digits Integer. Number of digits to round the results.
#'
#' @return A data.frame with latent variables and their reliability indices:
#' \describe{
#'   \item{Latent}{Latent variable name}
#'   \item{Cronbach}{Cronbach's alpha}
#'   \item{CR}{Composite Reliability}
#'   \item{AVE}{Average Variance Extracted}
#' }
#'
#' @details
#' \itemize{
#'   \item Cronbach's alpha is computed using the model-implied observed
#'   correlation matrix (lavInspect(fit, "cor.ov")).
#'   \item CR and AVE are computed from standardized factor loadings.
#'   \item Latent variables with fewer than two indicators are excluded.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' model <- '
#' level: 1
#'   fw =~ y1 + y2 + y3
#'
#' level: 2
#'   fb =~ y1 + y2 + y3
#' '
#'
#' fit <- lavaan::sem(
#'   model,
#'   data    = Demo.twolevel,
#'   cluster = "cluster"
#' )
#'
#' mlsem_reliability(fit, level = "within")
#' mlsem_reliability(fit, level = "between", digits = 4)
#' }
mlsem_reliability <- function(fit,
                              level  = c("within","between"),
                              digits = 3) {

  ## -------------------------
  ## 0. Level setting
  ## -------------------------
  level  <- match.arg(level)
  lv_idx <- ifelse(level == "within", 1, 2)

  ## -------------------------
  ## 1. Parameter extraction
  ## -------------------------
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  ## -------------------------
  ## 2. Measurement model (factor loadings)
  ## -------------------------
  meas <- pe %>%
    dplyr::filter(block == lv_idx, op == "=~") %>%
    dplyr::select(
      Latent = lhs,
      Item   = rhs,
      lambda = std.all
    )

  ## 최소 2문항 이상 요인만 사용
  lv_use <- meas %>%
    dplyr::count(Latent, name = "k") %>%
    dplyr::filter(k >= 2) %>%
    dplyr::pull(Latent)

  meas <- meas %>%
    dplyr::filter(Latent %in% lv_use)

  ## -------------------------
  ## 3. Cronbach's alpha
  ##    (model-based, cor.ov)
  ## -------------------------
  cor_ov <- lavaan::lavInspect(fit, "cor.ov")
  if (is.list(cor_ov)) cor_ov <- cor_ov[[lv_idx]]

  alpha_fun <- function(items) {
    C <- cor_ov[items, items, drop = FALSE]
    k <- ncol(C)
    if (k < 2) return(NA_real_)
    (k / (k - 1)) * (1 - sum(diag(C)) / sum(C))
  }

  ## -------------------------
  ## 4. CR & AVE calculation
  ## -------------------------
  cr_ave_from_lambda <- function(lambda) {
    lambda2 <- lambda^2
    CR  <- (sum(lambda))^2 / ((sum(lambda))^2 + sum(1 - lambda2))
    AVE <- sum(lambda2) / length(lambda2)
    c(CR = CR, AVE = AVE)
  }

  ## -------------------------
  ## 5. Summary table
  ## -------------------------
  out <- meas %>%
    dplyr::group_by(Latent) %>%
    dplyr::summarise(
      Cronbach = alpha_fun(Item),
      CR_AVE   = list(cr_ave_from_lambda(lambda)),
      .groups  = "drop"
    ) %>%
    dplyr::mutate(
      CR  = vapply(CR_AVE, function(x) x["CR"],  numeric(1)),
      AVE = vapply(CR_AVE, function(x) x["AVE"], numeric(1))
    ) %>%
    dplyr::select(Latent, Cronbach, CR, AVE) %>%
    dplyr::mutate(
      dplyr::across(-Latent, ~ round(.x, digits))
    )

  return(out)
}
