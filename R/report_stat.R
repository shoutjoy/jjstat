#' report nomality , t.test , var.test
#'
#' @param data test result
#' @param type normality, t.test, var.test(leven)
#' @param digits  default 3
#' @param trans translate TRUE korean language
#' @param show  normal is source and output, data is output
#'
#' @return report data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' shapiro.test(final) |> report_stat()
#'
#' var.test(mpg ~ vs, data = mtcars)|>
#' report_stat("var.test")
#'
#' car::leveneTest(mpg~ factor(vs), mtcars)|>
#'  report_stat("leven.test")
#'
#' car::leveneTest(mpg~ factor(vs), mtcars)|>
#' report_stat("leven.test", trans=TRUE)
#'
#'
#' fligner.test(mpg ~ vs,data= mtcars) |>
#' report_stat("t.test")
#'
#' t.test(mpg ~ vs, data= mtcars, var.equal = FALSE)|>
#' report_stat("t.test")
#'
#' t.test(len ~ supp, data = ToothGrowth, var.equal=TRUE) |>
#' report_stat("t.test")
#'
#' shapiro.test(final) |> report_stat()|> jjstat::k("en","ko")
#' shapiro.test(final) |> report_stat(trans =TRUE)
#'
#' car::leveneTest(mpg~ factor(vs), mtcars)|>
#' report_stat("leven.test", trans=TRUE)
#'
#'
#' }
#'
report_stat <- function(data, type="normality", digits = 3,
                        trans=FALSE, show="nomal") {
  # Extract relevant information from the data
  data = broom::tidy(data)
  #   #
  if(type=="leven.test"){
    data<- data |> dplyr::mutate(method="Leven test")
    # data <- data |> transform(method="Leven test")
  }else{
    data
  }

  method <- data$method
  statistic <- data$statistic
  p_value <- data$p.value
  if(p_value < 0.001){p_value_res <- "< .001"
  }else{
    p_value_res <- paste0("= ", round(data$p.value, digits))}
  # Determine the significance based on p-value

  if(type=="t.test"){
    significance <- ifelse(p_value < 0.05, "It was statistically significant", "It was not statistically significant. t = ")

  }else if(type =="normality"){
    significance <- ifelse(p_value < 0.05, "Statistically significant, the null hypothesis was rejected and normality was not secured.",
                           "the normality was secured by rejecting the alternative hypothesis because it was not statistically significant. statistic = ")

  }else if(type =="var.test"| type=="leven.test"){
    significance <- ifelse(p_value < 0.05, "Statistically significant, the null hypothesis is rejected, so equidistribution can not be assumed.",
                           "It was not statistically significant, and the null hypothesis can be adopted to assume equal variance. F = ")
  }
  # Create the sentence based on the method and p-value
  result_sentence <- paste0( method," showed that ",
                             significance,
                             #  "(stat = ",
                             #  statistic,
                             round(statistic ,digits),
                             ", p ",
                             p_value_res, ")")
  #  p_value , ")")

  # Print the result
  if(trans){
    jjstat::kakaoi(result_sentence,"en","ko", show=show)
  }else{
    result_sentence
  }

}



