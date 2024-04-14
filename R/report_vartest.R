
#' var.test report
#'
#' @param var_test var.test result
#' @param trans tans true english to korean
#'
#' @return explain result apa
#' @export
#'
#' @examples
#' \dontrun{
#' var.test(mpg ~ vs, data = mtcars)%>%
#'      report_vartest()
#'
#' var.test(mpg ~ vs, data = mtcars)%>%
#'      report_vartest() %>%
#'      jjstat::kakaoi()
#'
#' var.test(mpg ~ vs, data = mtcars)%>%
#' report_vartest(trans=TRUE)
#'
#' }
report_vartest = function(var_test, trans = FALSE){
  var_test_resul0<-  broom::tidy(var_test)
  var_test_result<-  var_test

  dd = var_test_result$data.name%>% strsplit(" by ") %>% unlist()
  dv = dd[1]
  iv = dd[2]
  iv_name_1 =  attributes(var_test_result$parameter)[[1]][1]
  iv_name_2 =  attributes(var_test_result$parameter)[[1]][2]

  # Output the results of the homogeneity of variance test in tibble format
  var_test_result_tibble <- tibble::tibble(
    Var = iv,
    df = var_test_result$parameter,
    f_value = var_test_result$statistic,
    p_value = var_test_result$p.value
  )

  var_test_report <- paste0(
    "Equality of variances analysis (var.test), between the two groups '",iv_name_1, "' and '",iv_name_2,
    "' of the independent variable '",iv,"'",
    " with respect to the dependent variable '", dv, "' showed a ",
    ifelse(var_test_result$p.value < 0.05, "statistically significant. F(",
           "not statistically significant. F("),
    var_test_result$parameter[1], ", ",
    var_test_result$parameter[2], ") = ",
    round(var_test_result$statistic, 2), ",",
    " p = ",
    round(var_test_result$p.value, 2), ". ",
    ifelse(var_test_result$p.value < 0.05,
           "This indicates that the variances of the two groups are not equal..",
           "This indicates that the variances of the two groups are equal.")
  )
  if(trans){
    jjstat::kakaoi(var_test_report)
  }else{ var_test_report}
}

