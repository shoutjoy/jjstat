#' anova report ,report_aov
#'
#' @param data aov data
#' @param digits default 3
#' @param trans TRUE english to korea
#'
#' @return report
#' @export
#'
#' @examples
#' \dontrun{
#' aov(len ~ supp*factor(dose), data= ToothGrowth)|>report_aov()
#' aov(len ~ supp*factor(dose), data= ToothGrowth)|>report_aov(trans=TRUE)
#'
#'
#' }
#'
report_aov <- function(data, digits = 2, trans=FALSE) {
  data0 = data
  dv = as.character(data$call[[2]][2])
  iv = as.character(data$call[[2]][3])
  Data = broom::tidy(data0)
  summary_data = summary(data0)

  # Extract relevant information from the data
  term <- Data$term
  df <- Data$df
  statistic <- Data$statistic

  p_value <- Data$p.value

  # Create the sentence based on the method and p-value
  result_sentence = list()
  for( i in 1: nrow(Data)-1){
    # Determine the significance based on p-value
    significance <- ifelse(p_value[i] < 0.05,
                           "It was statistically significant",
                           "It was not statistically significant")

    result_sentence[i] <- paste0("one-way analysis of variance result for dv[",
                                 dv,"]~[",term[i],"], ",
                                 significance, "(F (",
                                 df[i],
                                 ", ",
                                 df[nrow(Data)],
                                 ") = ",
                                 round(statistic[i], digits),
                                 ", p = ",
                                 round(p_value[i], digits),
                                 "). \n")
    #  significance = rbind(significance,)
  }
  # Print the result
  res = c(do.call(rbind, result_sentence))

  if(trans){
    cat( jjstat::k(res,"en","ko")[2] ,  "\n")
  }else{
    cat(res ,"\n")
  }
}
