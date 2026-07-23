#
#' Check and resolve missing data-
#'
#' @param data data.frame
#' @param plot missing data plot
#' @description
#' If there are missing values, print how many there are
#'
#' @return Results of checking for missingness
#' @export
#'
#' @examples
#' \dontrun{
#'  sleep %>% str()
#' sleep %>% missCheck()
#' }
#'
#'
missCheck <- function(data, plot = FALSE) {
  data <- as.data.frame(data)
  missing_count <- colSums(is.na(data))

  if (isTRUE(plot)) {
    if (!requireNamespace("VIM", quietly = TRUE)) {
      stop(
        "결측치 그래프를 그리려면 `VIM` 패키지가 필요합니다.",
        call. = FALSE
      )
    }
    VIM::aggr(data, prop = FALSE, numbers = TRUE, plot = TRUE)
  }

  table_data <- data.frame(
    Variable = names(data),
    V_count = colSums(!is.na(data)),
    Count = unname(missing_count),
    check.names = FALSE
  )

  dplyr::filter(table_data, Count != 0)
}

