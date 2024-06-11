#' Using ttest with mean, sd, and n
#'
#' @param n1 sample1
#' @param m1 mean1
#' @param sd1 sd1
#' @param n2 n2
#' @param m2 m2
#' @param sd2 sd2
#' @param var.equal TRUE Student FALSE, whech
#' @param digits 3
#'
#' @return ttest result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # # Example usage
#' msdn_ttest(n1 = 603, m1 = 59.6, sd1 = 0.6,
#'            n2 = 10785, m2 = 46.7, sd2 = 0.3)
#'
#' #  mysummaryBy(mpg ~ vs, data = mtcars)
#'
#' # library(jjstat)
#' msdn_ttest(n1 = 18, m1 = 16.617, sd1 = 3.8607,
#'            n2 = 14, m2 = 24.557, sd2 = 5.3790,  var.equal=FALSE)
#' t.test(mpg ~ vs, data = mtcars, var.equal=F)%>%tidy()%>%dall
#'
#' msdn_ttest(n1 = 18, m1 = 16.617, sd1 = 3.8607,
#'            n2 = 14, m2 = 24.557, sd2 = 5.3790, var.equal=TRUE)
#' t.test(mpg ~ vs, data = mtcars, var.equal=T)%>%tidy()
#' }
#'
msdn_ttest <- function(n1, m1, sd1, n2, m2, sd2, var.equal=FALSE, digits =2) {

  if (var.equal) {
    # Calculate the pooled standard deviation
    sp <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

    # Calculate the t-statistic
    t_stat <- (m1 - m2) / (sp * sqrt(1/n1 + 1/n2))

    # Degrees of freedom
    df <- n1 + n2 - 2

    # Calculate confidence interval
    ci_low <- (m1 - m2) - qt(1 - 0.05/2, df) * sp * sqrt(1/n1 + 1/n2)
    ci_high <- (m1 - m2) + qt(1 - 0.05/2, df) * sp * sqrt(1/n1 + 1/n2)

    method <- "Student Two Sample t-test"
    # cat("Two Sample t-test")

  } else {
    # Calculate Welch's t-statistic
    t_stat <- (m1 - m2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))

    # Calculate the degrees of freedom for Welch's t-test
    df <- ((sd1^2 / n1) + (sd2^2 / n2))^2 /
      (((sd1^2 / n1)^2 / (n1 - 1)) + ((sd2^2 / n2)^2 / (n2 - 1)))

    # Calculate confidence interval
    ci_low <- (m1 - m2) - qt(1 - 0.05/2, df) * sqrt((sd1^2 / n1) + (sd2^2 / n2))
    ci_high <- (m1 - m2) + qt(1 - 0.05/2, df) * sqrt((sd1^2 / n1) + (sd2^2 / n2))

    method <- "Welch Two Sample t-test"
    # cat("Welch Two Sample t-test")
  }

  # Calculate the p-value
  p_value <- 2 * pt(-abs(t_stat), df)

  # Create a list to mimic t.test output
  result <- list(
    # estimate = c(m1, m2),
    est =m1-m2,
    mean_1 = m1,
    mean_2 = m2,
    t = t_stat,
    df = df,
    p.value = p_value,
    # conf.int = c(ci_low, ci_high),
    ci_low = round(ci_low, digits),
    ci_high = round(ci_high, digits),
    method = method,
    # stderr = if (var.equal) sp * sqrt(1/n1 + 1/n2) else sqrt((sd1^2 / n1) + (sd2^2 / n2))
    # null.value = 0,
    alternative = "two.sided"
    # data.name = "Summary statistics"
  )
  bind_cols(result)%>%Unite2("ci_low", "ci_high", "CI", sep=", ", left="[",right="]")

}
