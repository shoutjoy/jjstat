
#' Item-level CR & AVE table
#'
#' @param fit lavaan (ML-)SEM object
#' @param level "within" or "between"
#' @param digits numeric
#'
#' @return data.frame
#' @export
item_CR_AVE <- function(fit,
                        level = c("within","between"),
                        digits = 3) {

  level <- match.arg(level)
  lv_idx <- ifelse(level == "within", 1, 2)

  ## -------------------------
  ## 1. Parameter estimates (items)
  ## -------------------------
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)

  item_est <- pe %>%
    dplyr::filter(block == lv_idx, op == "=~") %>%
    dplyr::transmute(
      Latent = lhs,
      Item   = rhs,
      Est    = est,
      SE     = se,
      std    = std.all,
      Z      = ifelse(se > 0,
                      paste0(round(est / se, digits), "***"),
                      "")
    )

  ## 기준문항 처리
  item_est$Est[item_est$SE == 0 | is.na(item_est$SE)] <- 1
  item_est$SE[item_est$SE == 0 | is.na(item_est$SE)] <- 0
  item_est$Z[item_est$SE == 0 | is.na(item_est$SE)]  <- ""

  ## -------------------------
  ## 2. CR & AVE (잠재변수 단위)
  ## -------------------------
  CR_AVE <- mlsem_reliability(
    fit   = fit,
    level = level,
    digits = digits
  )

  CR_AVE_use <- CR_AVE %>%
    dplyr::select(Latent, CR, AVE)

  ## -------------------------
  ## 3. Merge (item × latent)
  ## -------------------------
  out <- item_est %>%
    dplyr::left_join(CR_AVE_use, by = "Latent")

  ## -------------------------
  ## 4. Formatting
  ## -------------------------
  out <- out %>%
    dplyr::mutate(
      Est = round(Est, digits),
      SE  = ifelse(SE == 0, 0, round(SE, digits)),
      std = round(std, digits)
    )

  ## 중복 Latent / CR / AVE 정리
  out <- nice_table(out, exclude = c("Item","Est","SE","std","Z"))

  out
}

