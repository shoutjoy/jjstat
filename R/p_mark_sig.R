#' p_mark_sig is Function that displays significance in data
#'
#' @param data input data.frame or matrix
#' @param col 'col' specifies the name of the column representing the p value in the data. By default, it is set to p.value. For example, if it is written as p, pvalue, p_value, etc., you can change the name.
#' @param unite TRUE combine pvalue and sig
#' @param digits default 3
#' @param ns ns="ns"
#' @param unite_col unite_col is the variable we want to merge with sig
#' @param rownames_to_column drownames_to_column if you need
#' @param exclude Round exclude varible
#' @param remove remove var TRUE
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
#'   rstatix::t_test(data =., len ~ supp) %>%
#'   p_mark_sig("p")
#'
#'   ### NEW
#'   TukeyHSD(aov(weight ~ feed, chickwts))$feed %>% as.data.frame() %>%
#'   mutate(sig = add_sig(`p adj`))
#'
#'   TukeyHSD(aov(weight ~ feed, chickwts))$feed %>% p_mark_sig("p adj")
#'
#'
#' ##method 2
#'  ToothGrowth %>%
#'  group_by(dose) %>%
#'  rstatix::t_test(data =., len ~ supp) %>%
#'   p_mark_sig("p", unite=TRUE)
#'
#'
#'  ToothGrowth %>%
#'      group_by(dose) %>%
#'     rstatix::t_test(data =., len ~ supp) %>%
#'     p_mark_sig("p", unite = TRUE, unite_col = "statistic" ,ns="")
#'
#'  ToothGrowth %>%
#'    group_by(dose) %>%
#'    rstatix::t_test(data =., len ~ supp) %>%
#'    p_mark_sig("p", unite = TRUE, unite_col = "statistic" )

#' }
#'
#'
p_mark_sig <-function(data,
                      col = "p.value",
                      unite = FALSE,
                      unite_col = NULL,
                      digits = 3,
                      ns = "ns",
                      rownames_to_column = FALSE,
                      exclude= NULL,
                      remove=TRUE
){
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
                                             ns))))
  #Change variable to CHARACTER
  ndata$sig <- as.character(ndata$sig)



  if(rownames_to_column){

     res = ndata %>% data.frame() %>%
        tibble::rownames_to_column("vars") %>%
        tibble::tibble()
  }else{
    res = ndata %>% data.frame() %>%

      tibble::tibble()
       }


  if(unite){
  if(is.null(unite_col)){
    #Automatically combine p.value with sig
    res = res %>% dplyr::rename(p.value = all_of( col) ) %>%
      mutate_if(is.numeric, round, digits)

    res = res %>% tidyr::unite(p.value, p.value, sig, sep="")

  }else{
    # unite sig with a specified variable: estimate_data
    res = res %>% Round(digits = digits, exclude = exclude) %>%
      tidyr::unite(estimate_data, all_of(unite_col), sig, sep="", remove=remove)%>%
      rename(!!unite_col := estimate_data)
  }


    res= res%>% tibble::tibble()
  }else{
    res = res }

   options(pillar.sigfig = digits)

   return(res)

}
