
#' accent_table
#'
#' @param data data.frame
#' @param Var1 v1
#' @param Var2 v2
#' @param trans transpose
#'
#' @return cross table df
#' @export
#'
accent_table = function(data, Var1="a1", Var2="성조", trans = TRUE){


  res = data  %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = Var2, values_from = Freq) %>%
    rename(accent = Var1) %>% tibble::column_to_rownames("accent")

  if(trans){res= res%>% t()
  }else{res}
  res
}




#' hisq table observed/Expected table
#'
#' @param data data.frame
#' @param v1 v1
#' @param v2 v1
#' @param title title name
#' @param type outtype ct: crosstable, df: data.frame. 'margin','chisq_test','chitable','res1','res2'
#' @param digits round 3
#' @param trans transpose
#' @param simulate.p.value Chi-squared approximation may be incorrect, default= FALSE
#' @param warn warning msg
#' #'
#' @return output
#' @export
#'

chisq_table = function(data, v1, v2,
                       title="Table",
                       type= "res2",
                       digits=3,
                       trans=FALSE,
                       simulate.p.value = FALSE,
                       warn = FALSE){

  data =  data %>%
    dplyr::select(all_of(v1), all_of(v2)) %>%
    table()

  data_margin = data %>% addmargins() %>%
                accent_table(trans = trans, Var1 = v1)

  #chisq.test
  onset_accent <- data
  res = chisq.test(onset_accent, simulate.p.value = simulate.p.value)
  res_df = chisq.test(data)%>% broom::tidy()

  # msg = paste0("chisq = ", res_df[1,1])

  #proption table
  chi_table = (res$observed/ res$expected) %>% as.data.frame() %>%
    tidyr::pivot_wider(names_from = all_of(v2), values_from = Freq) %>%
    tibble::column_to_rownames(all_of(v1)) %>% round(digits)



  chi_table_md = chi_table %>%
    markdown_table(caption = paste0(title," observed/Expected table"),
                   digits = digits,
                   general = NULL)

  result = list(chisq_test = res,
                margin = data_margin %>%
                  markdown_table(caption = paste0(title,"observed table "),
                                 general = NULL),
                chi_table_md,
                chi_table = chi_table)
  result1 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," observed table "),
                     general = NULL),
    chisq_test = res,
    chi_df = res_df,
    chi_table = chi_table ,
    chi_table_md)




  switch(type,
         ct = data,
         df = data.frame(data),
         margin = data_margin,
         chisq_test= res,
         chitable = chi_table,
         res1 = result,
         res2 = result1)

  if(warn){
    options(warn = 0)
  }else{
    options(warn = -1)
  }


}
