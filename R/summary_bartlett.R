
#' Factorization Bartlett test results
#'
#' @param data data. cor data. result data
#' @param test perform test df, cor, none
#' @param cat show method
#'
#' @return result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Henry Kaiser (1970) introduced an Measure of Sampling
#' # Adequacy  (MSA) of factor analytic data matrices.
#' # Kaiser and Rice (1974) then modified it.
#' #'
#'
#' New = data.frame(
#'   A01 = c(4, 4, 5, 4, 5, 5, 5, 4, 4, 2, 4, 4, 5, 4, 4, 4, 4, 5, 1, 3),
#'   A02 = c(2, 3, 3, 5, 5, 4, 5, 5, 4, 4, 4, 4, 4, 3, 3, 5, 4, 4, 3, 1),
#'   A03 = c(3, 5, 4, 5, 4, 4, 4, 4, 4, 4, 5, 4, 5, 4, 4, 4, 4, 5, 3, 3),
#'   A04 = c(4, 4, 4, 4, 5, 4, 5, 4, 4, 3, 5, 4, 5, 5, 5, 4, 5, 5, 1, 5),
#'   A05 = c(3, 2, 3, 5, 4, 4, 4, 4, 4, 4, 4, 3, 5, 2, 3, 4, 5, 5, 4, 2)
#' )
#' New %>%summary_bartlett("df")
#' New %>%cor()%>% summary_bartlett("cor")
#' New %>%cor()%>%psych::cortest.bartlett()%>% summary_bartlett()
#'
#' }
summary_bartlett <- function(data, test = "df", cat = TRUE) {

  if (test == "df") {
    bartlett_result = data %>%
      cor() %>%
      psych::cortest.bartlett() %>%
      suppressWarnings()
  } else if (test == "cor") {
    bartlett_result = data %>%
      psych::cortest.bartlett() %>%
      suppressWarnings()
  } else if (test == "none") {
    bartlett_result = data %>% suppressWarnings()
  }

  chisq_value <- bartlett_result$chisq
  p_value <- bartlett_result$p.value
  df <- bartlett_result$df

  result_string <- sprintf("Bartlett's test : chisq(%d) = %.4f, p = %.2e",
                           df, chisq_value, p_value)
  result_msg_string <- sprintf("chisq(%d) = %.4f, p = %.2e.",
                               df, chisq_value, p_value)

  if (cat) {
    cat("\n")
    cat(result_string)
    cat("\n")

    if (p_value < 0.05) {
      cat("\nBartlett's 구형성 검정은 변수간에 상관관계가 있는지 확인하는 검정(test)로 유의확률(p)이 0.05이하면 변수간에 연관성이 있으며 요인분석 모형이 적합하다고 할 수 있다. 현재 모형은 유의하여 변인의 선정이 요인분석에 적절하였다,",
          result_msg_string,"\n\n")
    } else {
      cat("\nBartlett's 구형성 검정은 변수간에 상관관계가 있는지 확인하는 검정(test)로 유의확률(p)이 0.05이하면 변수간에 연관성이 있으며 요인분석 모형이 적합하다고 할 수 있다. 현재 모형은 유의하지 않아 변인의 선정이 요인분석에 적절하지 않을 수 있다,",
          result_msg_string,
          "\n\n")
    }
  } else {
    return(result_string)
  }
}
