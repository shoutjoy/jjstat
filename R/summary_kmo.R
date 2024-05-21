#' Kaiser-Meyer-Olkin factor adequacy (KMO)
#'
#' @param data data, cor data, result data
#' @param test none, cor, df
#' @param cat TRUE
#'
#' @return result
#' @export
#'
#' @examples
#' \dontrun{
#' #'
#' ## Example usage
#' New = data.frame(
#'   A01 = c(4, 4, 5, 4, 5, 5, 5, 4, 4, 2, 4, 4, 5, 4, 4, 4, 4, 5, 1, 3),
#'   A02 = c(2, 3, 3, 5, 5, 4, 5, 5, 4, 4, 4, 4, 4, 3, 3, 5, 4, 4, 3, 1),
#'   A03 = c(3, 5, 4, 5, 4, 4, 4, 4, 4, 4, 5, 4, 5, 4, 4, 4, 4, 5, 3, 3),
#'   A04 = c(4, 4, 4, 4, 5, 4, 5, 4, 4, 3, 5, 4, 5, 5, 5, 4, 5, 5, 1, 5),
#'   A05 = c(3, 2, 3, 5, 4, 4, 4, 4, 4, 4, 4, 3, 5, 2, 3, 4, 5, 5, 4, 2)
#' )
#' New %>%summary_kmo("df")
#' New %>%summary_kmo("df", cat=FALSE)
#' New %>%cor()%>% summary_kmo("cor")
#' New %>%cor()%>%psych::KMO()%>% summary_kmo()
#' }
#'
summary_kmo <- function(data, test="none", cat=TRUE) {
  if (test == "df") {
    result <- data %>% cor() %>% psych::KMO() %>% suppressWarnings()
  } else if (test == "cor") {
    result <- data %>% psych::KMO() %>% suppressWarnings()
  } else {
    result <- suppressWarnings(data)
  }

  overall_msa <- result$MSA
  suitability <- ifelse(overall_msa >= 0.9, "요인분석에 매우 적합하다",
                        ifelse(overall_msa >= 0.7, "요인분석에 적합하다",
                               ifelse(overall_msa >= 0.5, "요인분석이 가능하다", "부적합하다")))

  kmo_summary <- paste0(
    "Kaiser-Meyer-Olkin factor adequacy (KMO) = ", round(overall_msa, 3), "\n\n",
    "KMO는 입력된 변수간 상관계수의 제곱들과 편상관계수를 모두 더한 값 중에서 상관계수의 제곱의 합이 차지하는 비율이며, ",
    "요인분석이 적합한가를 나타내는 기준이다 (Kaiser and Rice, 1974). KMO가 0.5 이상이면 요인분석이 가능, 0.7 이상이면 적합하고, ",
    "0.9 이상이면 매우 적합한 것으로 판단한다. 현재 KMO는 ", round(overall_msa, 3), "으로 ", suitability, "."
  )

  if (cat) {
    print(result)
    cat("\n",kmo_summary, "\n")
  } else {
    return(kmo_summary)
  }
}
