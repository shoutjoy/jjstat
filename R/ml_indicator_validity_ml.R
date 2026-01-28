
#' Indicator validity table for (Multi-level) CFA
#'
#' @param fit lavaan SEM / ML-SEM object
#' @param level "within" or "between"
#' @param digits numeric
#'
#' @return data.frame
#' @export
indicator_validity_ml <- function(fit,
                                  level = c("within","between"),
                                  digits = 3) {

  level <- match.arg(level)
  lv_idx <- ifelse(level == "within", 1, 2)

  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  out <- pe %>%
    dplyr::filter(block == lv_idx, op == "=~") %>%
    dplyr::transmute(
      Latent = lhs,
      Item   = rhs,
      est    = round(est, digits),
      se     = round(se, digits),
      Z      = round(z, digits),
      p      = dplyr::if_else(
        pvalue < .001,
        "< .001",
        as.character(round(pvalue, digits))
      ),
      std    = round(std.all, digits),
      Validity = dplyr::case_when(
        std.all < 0.50 ~ "Reject",
        std.all < 0.70 ~ "Poor",
        TRUE           ~ "Good"
      )
    )

  out
}
