#'
#' #' accent_table
#' #'
#' #' @param data data.frame
#' #' @param cols v1
#' #' @param rows v2
#' #' @param trans transpose
#' #'
#' #' @return cross table df
#' #' @export
#' #'
#' # accent_table = function(data, cols ="speaker", rows ="성조", trans = TRUE){
#' #
#' #
#' #   res = data  %>% as.matrix() %>%data.frame() %>%
#' #     pivot_wider(names_from = rows, values_from = Freq) %>%
#' #     rename(accent = cols) %>% tibble::column_to_rownames("accent")
#' #
#' #   if(trans){res= res%>% t()
#' #   }else{res}
#' #   res
#' # }
#'
#' kge_accent_table = function(data,
#'                         cols="a1",
#'                         rows = "성조",
#'                         trans = TRUE,
#'                         type="res"){
#'
#'   res = data  %>% as.matrix() %>%data.frame() %>%
#'     pivot_wider(names_from = rows, values_from = Freq) %>%
#'     rename(accent = cols) %>% tibble::column_to_rownames("accent")
#'
#'   res_df = data  %>% as.matrix() %>%data.frame()
#'
#'   if(trans){res= res%>% t()
#'   }else{res}
#'
#'   switch(type,
#'          res = res,
#'          df = res_df)
#' }
#'
#'
#' #' hisq table observed/Expected table
#' #'
#' #' @param data data.frame
#' #' @param v1 v1
#' #' @param v2 v1
#' #' @param title title name
#' #' @param type outtype ct: crosstable, df: data.frame. 'margin','chisq_test','chitable','res1','res2'
#' #' @param digits round 3
#' #' @param trans transpose
#' #' @param simulate.p.value Chi-squared approximation may be incorrect, default= FALSE
#' #' @param warn warning msg
#' #' #'
#' #' @return output
#' #' @export
#' #'
#'
#' kge_chisq_table = function(data,
#'                            v1="a1",
#'                            v2="성조",
#'                            title ="Table",
#'                            type = "res2",
#'                            digits = 3,yadd=0.1,ncol=NULL,
#'                            trans = FALSE)
#' {
#'
#'   data =  data %>%
#'     dplyr::select(all_of(v1), all_of(v2)) %>%
#'     table()
#'   # 최종결과 에 포함
#'   data_margin = data %>% addmargins() %>%
#'     accent_table( v1, v2, trans = trans)
#'   #
#'   #chisq.test
#'   Onset_or_Coda_Accent_Contingency_table <- data
#'   res = chisq.test(Onset_or_Coda_Accent_Contingency_table)
#'   res_df = chisq.test(data)%>% broom::tidy()
#'   res_report = chisq.test(data)%>% report::report()
#'
#'   chi_mag = paste0(" [chisq = ",round(res$statistic, digits),
#'                    ", df = ",res$parameter,
#'                    ", p = ", format_number(res$p.value, digits),"]" )
#'   # res$statistic
#'   # res$parameter
#'   # res$p.value
#'
#'   if(nrow(data) != 1){
#'     chi_table = (res$observed / res$expected)%>% as.data.frame() %>%#
#'       tidyr::pivot_wider(names_from = v2, values_from = Freq) %>%
#'       tibble::column_to_rownames(v1) %>%
#'       Round(digits)
#'
#'
#'     # g = chi_table %>%  patternGraph()
#'
#'
#'
#'   }else{
#'
#'     # chi_table = (res$observed / res$expected)
#'     chi_table =
#'       rbind(
#'         observed = data %>%
#'           accent_table( v1, v2, trans = trans),
#'         expected = res$expected,
#'         obs_expected_ratio = (res$observed / res$expected)
#'       ) %>% Round(digits)
#'
#'     # g= NULL
#'   }
#'   #
#'
#'
#'   chi_table_md = chi_table %>%
#'     markdown_table(caption = paste0(title,chi_mag),
#'                    digits = digits,
#'                    general = NULL)
#'
#'
#'   # 결과를 정리하여 나타내는 값들
#'   result = list(chisq_test = res,
#'                 margin = data_margin %>%
#'                   markdown_table(caption = paste0(title,"Contingency table"),
#'                                  general = NULL),
#'                 chi_table_md,
#'                 chi_table = chi_table)
#'   result1 = list(
#'     # msg=msg,
#'     crosstable = data_margin,
#'     data_margin %>%
#'       markdown_table(caption = paste0(title," Contingency table"),
#'                      general = NULL),
#'     chisq_test = res,
#'     # chi_df = res_df,
#'     chisq_report = res_report,
#'     chi_table = chi_table ,
#'     g = patternGraph1(chi_table,raw = FALSE, yadd = yadd, ncol = ncol),
#'     chi_table_md)
#'
#'   result2 = list(
#'     # msg=msg,
#'     crosstable = data_margin,
#'     data_margin %>%
#'       markdown_table(caption = paste0(title," Contingency table"),
#'                      general = NULL),
#'     chisq_test = res,
#'     # chi_df = res_df,
#'     chisq_report = res_report,
#'     chi_table = chi_table ,
#'     # g = patternGraph1(chi_table,raw = FALSE),
#'     chi_table_md)
#'
#'
#'   switch(type,
#'          ct = data,
#'          df = data.frame(data),
#'          margin = data_margin,
#'          chisq_test = res,
#'          chisq_df = res_df,
#'          chisq_report = res_report,
#'          chitable = chi_table,
#'          res1 = result,
#'          res2 = result1,
#'          res2 = result2)
#' }
