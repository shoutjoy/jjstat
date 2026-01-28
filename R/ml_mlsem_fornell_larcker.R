
#' Fornell–Larcker Table with Significance (Multilevel SEM)
#'
#' @param fit lavaan multilevel SEM object
#' @param level "within" or "between"
#' @param digits numeric
#'
#' @export
#'
#'
mlsem_fornell_larcker <- function(fit,
                                  level = c("within","between"),
                                  digits = 3) {

  level <- match.arg(level)
  lv_idx <- ifelse(level == "within", 1, 2)

  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  ## -------------------------
  ## 측정모형
  ## -------------------------
  meas <- pe %>%
    dplyr::filter(block == lv_idx, op == "=~") %>%
    dplyr::select(Latent = lhs, Item = rhs, lambda = std.all)

  lv_use <- meas %>%
    dplyr::count(Latent, name = "k") %>%
    dplyr::filter(k >= 2) %>%
    dplyr::pull(Latent)

  meas <- meas %>% dplyr::filter(Latent %in% lv_use)

  ## -------------------------
  ## AVE & sqrt(AVE)
  ## -------------------------
  AVE_df <- meas %>%
    dplyr::group_by(Latent) %>%
    dplyr::summarise(
      AVE = {
        l <- lambda
        th <- 1 - l^2
        sum(l^2) / (sum(l^2) + sum(th))
      },
      .groups = "drop"
    )

  sqrt_AVE <- sqrt(AVE_df$AVE)
  names(sqrt_AVE) <- AVE_df$Latent

  ## -------------------------
  ## 잠재변수 상관행렬
  ## -------------------------
  lv_cor <- lavaan::lavInspect(fit, "cor.lv")
  if (is.list(lv_cor)) lv_cor <- lv_cor[[lv_idx]]

  lv_cor <- lv_cor[lv_use, lv_use, drop = FALSE]

  ## -------------------------
  ## Fornell–Larcker 하위삼각
  ## -------------------------
  FL <- matrix("", nrow = length(lv_use), ncol = length(lv_use),
               dimnames = list(lv_use, lv_use))

  for (i in seq_along(lv_use)) {
    for (j in seq_along(lv_use)) {
      if (i > j) {
        FL[i, j] <- round(lv_cor[i, j], digits)
      }
    }
  }

  FL_df <- as.data.frame(FL, stringsAsFactors = FALSE)

  ## ★ 핵심 수정: rownames → Latent 열
  FL_df <- FL_df %>%
    dplyr::mutate(
      Latent = rownames(FL_df),
      sqrt_AVE = round(sqrt_AVE[rownames(FL_df)], digits)
    )

  ## -------------------------
  ## sig 판단
  ## -------------------------
  max_cor <- apply(abs(lv_cor), 1, function(x) max(x[x < 1], na.rm = TRUE))
  sig <- ifelse(sqrt_AVE > max_cor, "*", "ns")

  FL_df$sig <- sig

  ## 열 순서 정리 (Latent → 상관 → sqrt_AVE → sig)
  FL_df <- FL_df %>%
    dplyr::select(Latent, dplyr::all_of(lv_use), sqrt_AVE, sig)

  rownames(FL_df) <- NULL

  FL_df
}
