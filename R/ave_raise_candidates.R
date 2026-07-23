#' ave_raise_candidates: Check AVE change by removing each indicator
#'
#' This function calculates how much the Average Variance Extracted (AVE) would change
#' if each indicator (item) is removed from a given latent variable. It also provides
#' recommendations for item removal based on AVE improvement.
#'
#' @param fit A fitted lavaan object.
#' @param latent A character string specifying the name of the latent variable to assess.
#'
#' @return A tibble containing:
#' \describe{
#'   \item{Item}{The name of the observed variable (indicator).}
#'   \item{lambda}{The standardized loading of the item.}
#'   \item{ave_current}{The current AVE with all items.}
#'   \item{ave_if_dropped}{The AVE if the current item is removed.}
#'   \item{delta}{Change in AVE (dropped - current).}
#'   \item{recommand}{Recommendation based on AVE change: "Recommended Removal", "Slight Improvement", or "Keep".}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' model <- 'F1 =~ x1 + x2 + x3 + x4 + x5'
#' fit <- cfa(model, data = HolzingerSwineford1939)
#' ave_raise_candidates(fit, "F1")
#' }
ave_raise_candidates <- function(fit, latent){
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE) |>
    dplyr::filter(op == "=~", lhs == latent) |>
    dplyr::select(Item = rhs, lambda = std.all)

  k <- nrow(pe)
  if (k < 2) {
    return(dplyr::tibble(
      Item = character(),
      lambda = numeric(),
      ave_current = NA_real_,
      ave_if_dropped = NA_real_,
      delta = NA_real_,
      recommand = character()
    ))
  }

  ave_current <- mean(pe$lambda^2, na.rm = TRUE)

  out <- purrr::map_dfr(seq_len(k), function(i){
    ave_drop <- if (k > 2) mean(pe$lambda[-i]^2, na.rm = TRUE) else NA_real_
    dplyr::tibble(
      Item = pe$Item[i],
      lambda = pe$lambda[i],
      ave_current = ave_current,
      ave_if_dropped = ave_drop,
      delta = ave_drop - ave_current
    )
  })

  out <- out |>
    dplyr::mutate(
      recommand = dplyr::case_when(
        delta > 0.005 ~ "Recommended Removal",
        delta > 0     ~ "Slight Improvement",
        TRUE ~ "Keep"
      )
    ) |>
    dplyr::arrange(dplyr::desc(delta))

  return(out)
}
