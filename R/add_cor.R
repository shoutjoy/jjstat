#' correlation analysis
#'
#' @param selectdata seledted daa
#' @param type res_all, res, p, r, res_p, res_md. g
#' @param digits default 3
#' @param plot graph performanceAnalytis::chart.Correlation
#' @param md markdown table output
#'
#' @return table and picture
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mtcars%>%select(mpg, disp, wt, hp, qsec)%>%add_cor()
#' }
add_cor = function(selectdata, type="res_all", digits=3, plot = TRUE,md=FALSE ){
  cor_data <- psych::corr.test(selectdata)
  rho = cor_data$r
  pvalue = cor_data$p
  res_p = combine_data(round(cor_data$r, digits),
                       format(round(cor_data$p, digits),scientific=TRUE, digits),"(",")")

  res = combine_data(round(cor_data$r, digits), add_significance_symbols(cor_data$p))
  res_p_star = combine_data(res_p, add_significance_symbols(cor_data$p))

  if(md){
    res_md = res%>%
      lowerMat(fill=NA, diag=NA) %>%
      markdown_table(digits = digits)
  }else{
    res_md = NULL
  }



  if(plot){
    graph = suppressWarnings({ PerformanceAnalytics::chart.Correlation(selectdata)})
    res_all = list(rho = rho,
                   pvalue = pvalue,
                   res_p = res_p,
                   res = res,
                   res_lower = res%>%
                     lowerMat(fill="-", diag="-"),
                   res_md = res_md,
                   plot = graph )
  }else{
    res_all = list(rho = rho,
                   pvalue = pvalue,
                   res_p = res_p,
                   res = res,
                   res_lower = res%>%
                     lowerMat(fill="-", diag="-"),
                   res_md = res_md )
  }


  switch(type,
         all = res_all,
         res_all = res_all,
         res_p_star = res_p_star,
         res = res,
         res_p = res_p,
         r = rho,
         p = pvalue ,
         g = graph,
         res_md = res_md)

}
