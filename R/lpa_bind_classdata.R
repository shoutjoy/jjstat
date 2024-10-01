#' lpa_bind_classdata
#'
#' @param data data
#' @param n_profiles default 3
#' @param package mclucs , MplusAutomation
#' @param type all, res, fit, est, data, all, plot, group_plot
#' @param add_line  add_lineTRUE
#' @param var_model  "EEE","EEI,"VVV","EII"
#' @param Class_colors  1:4
#' @param linewidth  linewidth 0.9
#' @param sd  TRUE graph
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
#' # "EEE": variances = "equal", covariances = "equal"
#' # 프로파일 간 분산과 공분산이 모두 동일하다고 가정.
#' # "EEV": variances = "equal", covariances = "varying"
#' # 분산은 동일하지만, 공분산은 프로파일마다 다르게 추정.
#' # "EEI": variances = "equal", covariances = "zero"
#' # 분산은 동일하지만, 공분산은 추정하지 않음 (공변량 간 독립성 가정).
#' # "EII": variances = "equal", covariances = "zero"
#' # 동일한 가정으로 분산이 동일하고, 공분산은 추정되지 않음.
#' }
#'
#'
#'
lpa_bind_classdata <- function(data, n_profiles = 3, models = NULL,
                               package = c("mclust", "MplusAutomation"),
                               type = "all", add_line = TRUE, var_model = "EEE",
                               Class_colors = 1:4, linewidth =0.9,
                               sd=TRUE) {
  library(tidyLPA)

  # var_model에 따라 variances와 covariances 설정
  variances <- "equal"
  covariances <- "equal"

  if (var_model == "EII") {
    variances <- "equal"
    covariances <- "zero"
  } else if (var_model == "VII") {
    variances <- "varying"
    covariances <- "zero"
  } else if (var_model == "EEE") {
    variances <- "equal"
    covariances <- "equal"
  } else if (var_model == "VEE") {
    stop("VEE model is not supported in 'mclust', try with 'MplusAutomation'.")
  } else if (var_model == "EEV") {
    stop("EEV model is not supported in 'mclust', try with 'MplusAutomation'.")
  } else if (var_model == "VVV") {
    variances <- "varying"
    covariances <- "varying"
  } else {
    stop("Invalid var_model provided. Choose from 'EII', 'VII', 'EEE', 'VEE', 'EEV', 'VVV'.")
  }

  # 프로파일 추정
  lpa_res <- data %>%
    single_imputation() %>%
    estimate_profiles(n_profiles = n_profiles, package = package,
                      variances = variances, covariances = covariances)

  # 결과 데이터 및 플롯 추출
  res_data <- get_data(lpa_res)
  est <- get_estimates(lpa_res)
  fit <- get_fit(lpa_res)
  bivariate <- plot_bivariate(lpa_res)
  density <- plot_density(lpa_res)
  plot <- plot_profiles(lpa_res, add_line = add_line,
                        sd=sd  )+
    scale_linetype_manual(values = Class_colors) +
    aes(linewidth = linewidth) +
    scale_linewidth_identity()

  # plot_profiles(
  #    x,
  #    variables = NULL,
  #    ci = 0.95,
  #    sd = TRUE,
  #    add_line = FALSE,
  #    rawdata = TRUE,
  #    bw = FALSE,
  #    alpha_range = c(0, 0.1),
  #    ...
  #  )


  # Class 부분만 별도로 추출하여 저장
  raw_data = Mclust(data, G=n_profiles, modelNames = var_model)
  Class_data = raw_data$classification

  # 그룹 프로파일 플롯
  if (type == "group_plot") {
    lpa_res_group <- data %>%
      poms() %>%
      estimate_profiles(n_profiles = 1:n_profiles, package = package,
                        variances = variances, covariances = covariances)
    group_plot <- plot_profiles(lpa_res_group, add_line = add_line)
  }

  if (type == "plot" | type == "group_plot") {
    x11()
  }

  # all 변수에 class_data 추가

  x11()
  print(density)
  x11()
  print(plot)
  all <- list(fit = fit, estimate = est,
              data = res_data,
              # Class = Class_data,
              Class_freq = table(Class_data)
              # bivariate = bivariate,
              # density = density,
              # plot = plot
  )


  # switch 문으로 결과 반환
  switch(type,
         all = all,
         res = lpa_res,
         data = res_data,
         est = est %>% Round(3) %>% dall(),
         fit = fit %>% dall(),
         Class = Class_data,
         Class_freq = Freq_table(Class_data, prop=TRUE),
         bivariate = bivariate,
         density = density,
         plot_density = density,
         plot = plot,
         plot_profiles = plot,
         plot_profiles_group = group_plot,
         group_plot = group_plot
  )
}
# lpa_bind_classdata <- function(data, n_profiles = 3, models = NULL,
# package = c("mclust", "MplusAutomation"),
# type = "all", add_line = TRUE, var_model = "EEE") {
#   library(tidyLPA)
#
#   # var_model에 따라 variances와 covariances 설정
#   variances <- "equal"
#   covariances <- "equal"
#
#   if (var_model == "EII") {
#     # Equal variances and covariances fixed to 0
#     variances <- "equal"
#     covariances <- "zero"
#   } else if (var_model == "VII") {
#     # Varying variances and covariances fixed to 0
#     variances <- "varying"
#     covariances <- "zero"
#   } else if (var_model == "EEE") {
#     # Equal variances and equal covariances
#     variances <- "equal"
#     covariances <- "equal"
#   } else if (var_model == "VEE") {
#     # Varying variances and equal covariances (not supported in mclust)
#     stop("VEE model is not supported in 'mclust', try with 'MplusAutomation'.")
#   } else if (var_model == "EEV") {
#     # Equal variances and varying covariances (not supported in mclust)
#     stop("EEV model is not supported in 'mclust', try with 'MplusAutomation'.")
#   } else if (var_model == "VVV") {
#     # Varying variances and varying covariances
#     variances <- "varying"
#     covariances <- "varying"
#   } else {
#     stop("Invalid var_model provided. Choose from 'EII', 'VII', 'EEE', 'VEE', 'EEV', 'VVV'.")
#   }
#
#   # 프로파일 추정
#   lpa_res <- data %>%
#     single_imputation() %>%
#     estimate_profiles(n_profiles = n_profiles, package = package,
#                       variances = variances, covariances = covariances)
#
#   # 그룹 프로파일 플롯
#   if (type == "group_plot") {
#     lpa_res_group <- data %>%
#       poms() %>%
#       estimate_profiles(n_profiles = 1:n_profiles, package = package,
#                         variances = variances, covariances = covariances)
#     group_plot <- plot_profiles(lpa_res_group, add_line = add_line)
#   }
#
#   # 결과 데이터 및 플롯 추출
#   res_data <- get_data(lpa_res)
#   est <- get_estimates(lpa_res)
#   fit <- get_fit(lpa_res)
#   bivariate <- plot_bivariate(lpa_res)
#   density <- plot_density(lpa_res)
#   plot <- plot_profiles(lpa_res, add_line = add_line)
#
#
#   if (type == "plot" | type == "group_plot") {
#     x11()
#   }
#
#   all <- list(fit = fit, estimate = est, data = res_data,
#               bivariate= bivariate,density= density, plot=plot )
#
#   # switch 문으로 결과 반환
#   switch(type,
#          all = all,
#          res = lpa_res,
#          data = res_data,
#          est = est %>% Round(3) %>% dall(),
#          fit = fit %>% dall(),
#          bivariate = bivariate,
#          density = density,
#          plot_density = density,
#          plot = plot,
#          plot_profiles = plot,
#          plot_profiles_group = group_plot,
#          group_plot = group_plot
#   )
# }
# lpa_bind_classdata= function(data, n_profiles=3,model=NULL,
#                              package=c("mclust","MplusAutomation"),
#                              type="all",
#                              add_line=TRUE){
#   library(tidyLPA)
#
#   lpa_res <-  data%>%
#     single_imputation() %>%
#     estimate_profiles(n_profiles=n_profiles, package=package,model=model)
#
#   if(type=="group_plot"){
#     lpa_res_group <-  data%>%
#       poms() %>%
#       estimate_profiles(n_profiles=1:n_profiles, package=package,model=model)
#      group_plot = plot_profiles(lpa_res_group, add_line=add_line)
#
#   }
#
#   res_data =  get_data(lpa_res)
#   est = get_estimates(lpa_res)
#   fit = get_fit(lpa_res)
#   bivariate = plot_bivariate(lpa_res)
#   density = plot_density(lpa_res)
#   plot = plot_profiles(lpa_res, add_line=add_line)
#
#
#   if(type=="plot"|type=="group_plot"){
#     x11()
#   }
#
#   if(type=="plot"|type=="group_plot"){
#     x11()
#   }
#
#   all = list(fit= fit, estimate = est, data = res_data)
#
#
#   switch(type, all= all, res= lpa_res,
#          data=res_data,
#          est =est%>%Round(3)%>%dall(),
#          fit =fit %>%dall(),
#          bivariate= bivariate,
#          density = density,
#          plot_density = density,
#          plot=plot,
#          plot_profiles = plot,
#          plot_profiles_group = group_plot,
#          group_plot = group_plot
#   )
# }
