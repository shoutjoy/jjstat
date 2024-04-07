#' correlation analysis
#'
#' @param selectdata seledted daa
#' @param type res_all, res, p, r, res_p, res_md. g
#' @param digits default 3
#' @param g graph performanceAnalytis::chart.Correlation
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
add_cor = function(selectdata, type="res_all", digits=3, g=TRUE ){
  cor_data <- psych::corr.test(selectdata)
  rho = cor_data$r
  pvalue = cor_data$p
  res_p = combine_data(round(cor_data$r, digits),
                       format(round(cor_data$p, digits),scientific=TRUE, digits),"(",")")

  res = combine_data(round(cor_data$r, digits), add_significance_symbols(cor_data$p))
  res_p_star = combine_data(res_p, add_significance_symbols(cor_data$p))

  res_md = res%>%
    SEM212::lowerMat(fill=NA, diag=NA) %>%
    markdown_table(digits = digits)

  graph = PerformanceAnalytics::chart.Correlation(selectdata)

  if(g){

    res_all = list(rho,pvalue,res_p, res, res_md, graph )
  }else{
    res_all = list(rho,pvalue,res_p, res, res_md )
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
