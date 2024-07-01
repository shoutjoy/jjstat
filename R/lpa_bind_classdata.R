#' lpa_bind_classdata
#'
#' @param data data
#' @param n_profiles default 3
#' @param package mclucs , MplusAutomation
#' @param type all, res, fit, est, data, all, plot, group_plot
#' @param add_line  add_lineTRUE
#' @param add_line  add_line null
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #'
#' lpa_bind_classdata(select(iris, -Species))
#'
#' lpa_bind_classdata(select(iris, -Species), type="res")
#' lpa_bind_classdata(select(iris, -Species), type="fit")
#' lpa_bind_classdata(select(iris, -Species), type="est")
#' lpa_bind_classdata(select(iris, -Species), type="data")%>%Round(3)%>%dall
#' lpa_bind_classdata(select(iris, -Species), type="all")
#' #'
#'
#' }
#'
#'
#'
lpa_bind_classdata= function(data, n_profiles=3,model=NULL,
                             package=c("mclust","MplusAutomation"),
                             type="all",
                             add_line=TRUE){
  library(tidyLPA)

  lpa_res <-  data%>%
    single_imputation() %>%
    estimate_profiles(n_profiles=n_profiles, package=package,model=model)

  if(type=="group_plot"){
    lpa_res_group <-  data%>%
      poms() %>%
      estimate_profiles(n_profiles=1:n_profiles, package=package,model=model)
     group_plot = plot_profiles(lpa_res_group, add_line=add_line)

  }

  res_data =  get_data(lpa_res)
  est = get_estimates(lpa_res)
  fit = get_fit(lpa_res)
  bivariate = plot_bivariate(lpa_res)
  density = plot_density(lpa_res)
  plot = plot_profiles(lpa_res, add_line=add_line)


  if(type=="plot"|type=="group_plot"){
    x11()
  }

  if(type=="plot"|type=="group_plot"){
    x11()
  }

  all = list(fit= fit, estimate = est, data = res_data)


  switch(type, all= all, res= lpa_res,
         data=res_data,
         est =est%>%Round(3)%>%dall(),
         fit =fit %>%dall(),
         bivariate= bivariate,
         density = density,
         plot_density = density,
         plot=plot,
         plot_profiles = plot,
         plot_profiles_group = group_plot,
         group_plot = group_plot
  )
}
