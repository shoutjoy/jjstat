#' Batch calculation of ICCs for multiple variables
#'
#' This function applies \code{icc_lme4()} to multiple outcome variables
#' and returns a combined table of ICC results.
#'
#' @param data data.frame. Dataset containing outcome variables and cluster variable.
#' @param vars character vector. Names of numeric outcome variables.
#' @param cluster character string. Name of the clustering variable.
#'
#' @return A tibble with ICC results for each variable.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Built-in dataset
#' data(mtcars)
#'
#' # Single ICC calculation
#' icc_lme4(
#'   y = mtcars$hp,
#'   cluster = mtcars$cyl
#' )
#'
#' # Batch ICC calculation
#' icc_batch(
#'   data = mtcars,
#'   vars = c("mpg", "hp", "wt"),
#'   cluster = "cyl"
#' )
#' }
icc_batch <- function(data, vars, cluster) {

  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tibble", quietly = TRUE)) {
    stop("Packages 'dplyr' and 'tibble' are required.")
  }

  res <- lapply(vars, function(v) {

    out <- icc_lme4(data[[v]], data[[cluster]])
    out$variable <- v

    out
  })

  dplyr::bind_rows(res) %>%
    dplyr::mutate(
      icc_interpretation = dplyr::case_when(
        ICC >= 0.10 ~ "강한 집단 효과(>= 0.10)",
        ICC >= 0.05 & ICC < 0.10 ~ "경계 수준, 다층 분석 권장(ICC >= 0.05 & ICC < 0.1)",
        ICC < 0.05 ~ "개인 수준 중심 변수(ICC < 0.05)"
      )
    ) %>%
    dplyr::select(
      variable,
      ICC,
      between_var,
      within_var,
      J,
      kbar,
      icc_interpretation
    ) %>%
    tibble::as_tibble()
}

