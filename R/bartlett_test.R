
#' bartlett_test
#'
#' @param data data.frame or mean. sd. n
#' @param formula dv ~ iv
#' @param Mean mean selection
#' @param sd sd selection
#' @param n n selection
#' @param var variable
#'
#' @return result
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' set.seed(20240418)
#' my_data <- data.frame(
#'   value = c(group1 <- rnorm(20, mean = 10, sd = 2),
#'             group2 <- rnorm(20, mean = 12, sd = 2),
#'             group3 <- rnorm(20, mean = 15, sd = 2)),
#'   group = rep(c("Group1", "Group2", "Group3"), each = 20)
#' )
#' my_data %>%mysummaryBy(value ~ group)%>%select(1:4) %>%bartlett_test()
#'
#' data = data.frame(row.names = c("1", "2", "3"),
#'                   var = c('Group1', 'Group2', 'Group3'),
#'                   Mean = c('10.0782306287367', '11.5824600813951', '14.764623393546'),
#'                   SD = c('1.88743751774179', '1.9951475292551', '2.07862332265816'),
#'                   N = c('20', '20', '20'))
#' data
#' bartlett_test(data)
#'
#' # original test
#' bartlett.test(value ~ group, data=my_data)
#' #now test
#' bartlett_test(value ~ group, data=my_data)
#'
#'
#' #origin
#' bartlett.test(mpg~cyl, data=mtcars)
#' #aggregated data
#' mtcars%>%mysummaryBy(mpg~cyl) %>%bartlett_test()
#' # Using function
#' bartlett_test(mpg~cyl, data=mtcars)
#' #'
#' }
bartlett_test <- function(data, formula=NULL,
                          Mean="Mean",
                          sd="SD",
                          n ="N",
                          var="variable"
) {

  if(is.null(formula)){
    # Extract mean, standard deviation, and sample size by group
    mean_values <- data[[Mean]]
    sd_values <- data[[sd]]
    n_values <- data[[n]]
    ivs = paste0(data[[var]], collapse=", ")
    # Number of groups
    k <- nrow(data)
    df = k - 1
    # Total number of samples
    N <- sum(n_values)
    # Calculating Pooled Variance
    sp_squared <- sum((n_values - 1) * sd_values^2) / (N - k)
    # Calculate the log sum of variance by group
    sum_log_variances <- sum((n_values - 1) * log(sd_values^2))
    # Calculate the C value
    C <- 1 + (1 / (3 * (k - 1))) * (sum(1 / (n_values - 1)) - 1 / (N - k))

    # Calculate the chi-square statistic
    chi_sq <- ((N - k) * log(sp_squared) - sum_log_variances) / C
    # Chi-square test
    p_value <- 1 - pchisq(chi_sq, df = k - 1)
  }else{
    b_res = bartlett.test(formula, data=data) %>% broom::tidy()
    chi_sq = b_res$statistic
    df = b_res$parameter
    k = df + 1
    p_value = b_res$p.value
    iv = as.character(formula(formula))[3]
    ivs = paste0(unique(data[[iv]]), collapse=", ")
  }
  # Output the results
  cat("\n\nBartlett Test of homogeneity of variances(For Equal Variances)\n")
  cat("=================================================================\n")
  cat("Chi-Squared Value:", chi_sq, "\n")
  cat("Degrees of Freedom:", df, "\n")
  cat("p-value:", round(p_value, 4), "\n")
  # 판단
  sig = ifelse(p_value > 0.05 ,
               "등분산을 만족하였다(",
               "등분산을 만족하지못하였다()")
  p_sig = ifelse(p_value < 0.001, "< .001", paste0("= ",
                                                   round(p_value,4)))

  Msg = paste0("\n Bartlett's test를 이용하여 ",
               ivs,"에 관한 등분산 검정 검정결과, ",
               sig, "(Chi-Squared(df = ",as.vector(df),") = ",
               round(as.vector(chi_sq),3), ", p ",
               p_sig, "). \n\n",
               "사후분석의 경우 : 등분산이면서 샘플수가 동일한 경우 Tukey, Dunett, Ducan을 사용하고, 등분산이면서 샘플수가 다른 경우는 Bonferroni, Scheffe등을 사용, 등분산이 아닌경우는 GAmes-Howell의 방법을 사용한다. \n\n" )

  # cat("\n",Msg,"\n\n")
  # 등분산 검정 결과 반환
  res = tibble(ivs= ivs, statistic = chi_sq,
               df = df, p_value = p_value)
  Res = list(res=res, msg= cat(Msg))
  return(Res)
}
