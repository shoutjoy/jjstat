#' p_mark_sig is Function that displays significance in data
#'
#' @param data input data.frame or matrix
#' @param col 'col' specifies the name of the column representing the p value in the data. By default, it is set to p.value. For example, if it is written as p, pvalue, p_value, etc., you can change the name.
#' @param unite TRUE combine pvalue and sig

#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## data(ToothGrowth)
#' library(dplyr)
#' library(rstatix)
#'
#' ##method 1
#'  ToothGrowth %>%
#'   group_by(dose) %>%
#'   t_test(data =., len ~ supp) %>%
#'   p_mark_sig("p")
#'
#' ##method 2
#'  ToothGrowth %>%
#'  group_by(dose) %>%
#'  rstatix::t_test(data =., len ~ supp) %>%
#'   p_mark_sig("p", unite=TRUE)
#' }
#'
#'
p_mark_sig <-function(data,
                      col="p.value",
                      unite = FALSE){
  #
  library(tidyverse)

  p.value <- data %>% as.data.frame() %>%
    dplyr::select(all_of(col))
  # any_of() does not check for missing variables. This is especially useful for negative selection when you want to check if a variable has been removed.
  # all_of() is for strict selection. An error occurs if any of the variables in the character vector are missing.
  # all_of : correct, any_of: some
  ndata <- data %>%
    as.data.frame() %>%
    dplyr::mutate(sig = ifelse(p.value < 0.001, "***",
                               ifelse(p.value < 0.01, "**",
                                      ifelse(p.value < 0.05, "*",
                                             "ns"))))
  #Change variable to CHARACTER
  ndata$sig <- as.character(ndata$sig)
  res = ndata %>% tibble::tibble()



  if(unite){
    res = res %>% dplyr::rename(p.value = all_of(col))
    res = res %>% tidyr::unite(p.value, p.value, sig, sep="")
    res
  }else{
    res }
}

