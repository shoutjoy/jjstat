#' cramers_v: Correlation of categorical variables
#'
#' @param data matrix, table
#' @param type  cramer, adjust, data
#' @param digits 3
#'
#' @return correlation
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' matrix(c(36, 67, 11,
#' 40, 35, 44,
#' 41, 108, 1,
#' 56, 87, 54), nrow = 4, byrow = TRUE,
#' dimnames = list(c("공명음", "마찰음", "유기음_경음", "평파열음_평파찰음"),
#'                 c("H", "H(H)", "L"))) %>%
#'   cramers_v()
#'
#' # A tibble: 1 × 3
#' # Cramer_V  p.value sig
#' # <dbl>    <dbl> <chr>
#' #  0.441 1.01e-16 ***
#'
#' }
cramers_v <- function(data, type="cramer", digits=3) {
  # Calculate chi-square test for independence
  chi_square_test <- chisq.test(data)

  # Calculate Cramer's V
  n <- sum(data)
  num_rows <- nrow(data) - 1   #c-1
  num_cols <- ncol(data) - 1   #r-1

  phi <- chi_square_test$statistic / n
  v <- sqrt(phi / min(num_rows, num_cols))

  #adjusted cramer's V
  adjust_v = v /( sqrt(min(num_rows, num_cols)) )

  # Calculate p-value
  p_value <- pchisq(chi_square_test$statistic,
                    df = chi_square_test$parameter,
                    lower.tail = FALSE)

  #p value
  if(p_value<0.001){
    p_value = "< .001"
  }else{
    p_value = format(paste0("= ", round(p_value,3)), digits, scientific=TRUE)
  }


  # Calculate 95% confidence interval for Cramer's V
  alpha <- 0.05
  chi_critical <- qchisq(1 - alpha / 2, df = min(num_rows, num_cols))
  lower_ci <- max(0, v - sqrt(chi_critical / n))
  upper_ci <- min(1, v + sqrt(chi_critical / n))



  # result
  res = cbind.data.frame(Cramer_V = v, p.value = p_value)
  res1 = paste0("Cramer's V = ", round(v, digits))
  res2 = paste0("Cramer's V = ", round(v, digits), ", 95%CI[",
                round(lower_ci, digits),", ",
                round(upper_ci, digits),"]")

  res3 = paste0("Adusted Cramer's V = ", round(adjust_v, digits), ", p ",
                format(p_value, digits, scientific = TRUE))
  switch(type,
         data = res,
         data1 = res1,
         cramer = res2,
         adjust = res3
  )
}
