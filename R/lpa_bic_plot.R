#' lpa_BIC_plot
#'
#' @param data data
#' @param n_profiles_range 1:9
#' @param legend_position top
#' @param covar_model "EEI", "EEE", "VVI", "VVV", "EEV", "EII", "VII",VVV
#' @param xintercept model 4
#' @param size.text size.text 4
#' @param flip y value <0 default
#' @param basic basic is "EII", "EEE", "VII", "EEI"
#'
#' @return pot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' select(iris, -Species)%>%lpa_BIC_plot()
#' }
#'
lpa_BIC_plot <- function(data, n_profiles_range = 1:6,
                         legend_position = "top",
                         covar_model = c("EII", "EEE", "VII",
                                         "EEV", "EEI", "VVI", "VVV"),set_n_profile=NULL,
                         xintercept = 4, size.text = 4,
                         flip = TRUE,  # flip option added
                         basic = TRUE) {  # basic option added
  library(forcats)
  library(dplyr)

  # 기본 covar_model 값 설정 (basic이 TRUE이면 Mplus 전용 모델 제외)
  if (basic) {
    covar_model <- c("EII", "EEE", "VII", "EEI")
  }

  # 데이터를 long 형식으로 변환
  to_plot_wide <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range,
                                       set_n_profile=set_n_profile)

  # 데이터 내 실제 존재하는 covariance 모델만 필터링
  existing_models <- colnames(to_plot_wide)[-1]  # 첫 번째 열은 'n_profiles'이므로 제외
  covar_model <- intersect(covar_model, existing_models)  # 실제 데이터에 존재하는 모델만 선택

  to_plot <- to_plot_wide %>%
    gather(`Covariance matrix structure`, val, -n_profiles) %>%
    filter(`Covariance matrix structure` %in% covar_model) %>%
    mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`),
           val = abs(as.numeric(val)))  # val을 숫자로 변환 후 절댓값 적용

  # 기본 covar_model 값 설정 (basic이 TRUE인 경우)
  if (basic) {
    to_plot$`Covariance matrix structure` <- fct_relevel(
      to_plot$`Covariance matrix structure`,
      "EII", "EEE", "VII", "EEI"
    )
  } else {
    to_plot$`Covariance matrix structure` <- fct_relevel(
      to_plot$`Covariance matrix structure`,
      "EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"
    )
  }

  # flip이 TRUE이면 y값을 반전
  if (flip) {
    to_plot$val <- -to_plot$val  # y값을 반전
  }

  # 플롯 생성
  gg <- ggplot(to_plot, aes(x = n_profiles, y = val,
                            color = `Covariance matrix structure`,
                            group = `Covariance matrix structure`)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, color = "black") +
    ggrepel::geom_text_repel(aes(label =
                                   paste0(`Covariance matrix structure`,":",round(val, 2))),
                             vjust = -0.9, hjust=0.5, size = size.text, min.segment.length = 0.8) +
    ylab(ifelse(flip, "Flipped BIC (larger value is better)", "BIC (smaller value is better)")) +
    guides(linewidth = "none", alpha = "none") +
    geom_vline(xintercept = xintercept, linetype = "dashed", color = "tomato") +
    theme_bw() +
    theme(legend.position = legend_position)

  # 결과 반환
  # to_plot= to_plot%>%rename(Gaussian_Mixture_Model=`Covariance matrix structure`)

  res <- list(data = to_plot_wide, graph = gg,  to_plot)
  return(res)
}

# lpa_BIC_plot <- function(data, n_profiles_range = 1:9){
#   library(forcats)
#
#   #long data trnasformation
#   to_plot <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range)%>%
#     gather(`Covariance matrix structure`, val, -n_profiles) %>%
#     mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`), val = abs(val))
#
#   # C- variable arrange
#   to_plot$`Covariance matrix structure` <- fct_relevel(
#     to_plot$`Covariance matrix structure`,
#     "Constrained variance, fixed covariance",
#     "Freed variance, fixed covariance",
#     "Constrained variance, constrained covariance",
#     "Freed variance, freed covariance")
#
#   #plotting
#   gg <-ggplot(to_plot, aes(x = n_profiles, y = val,
#                            color = `Covariance matrix structure`,
#                            group = `Covariance matrix structure`)) +
#     geom_line(linewidth = 1) +
#     geom_point(size=2) +
#     ylab("BIC (smaller value is better)") +
#     guides( linewidth="none", alpha="none")+
#     theme_bw()
#
#   res= list(data = to_plot, graph = gg)
#   res
#
# }


#' lpa_BIC_plot2
#'
#' @param data data
#' @param n_profiles_range 1:9
#' @param legend_position top
#' @param covar_model "EEI", "EEE", "VVI", "VVV", "EEV", "EII", "VII",VVV
#' @param xintercept model 4
#' @param size.text size.text 4
#' @param flip y value <0 default
#' @param basic basic is "EII", "EEE", "VII", "EEI"
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' select(iris, -Species)%>%lpa_BIC_plot2()
#' }
#'
lpa_BIC_plot2 <-  function(data, n_profiles_range = 1:6, type="all",
                           legend_position = "top",
                           covar_model = c("EII", "EEE", "VII",
                                           "EEV", "EEI", "VVI", "VVV"),set_n_profile=NULL,                            xintercept = 4, size.text = 4,
                           flip = FALSE,  # flip option added
                           basic = TRUE) {  # basic option added
  library(forcats)
  library(dplyr)

  # 기본 covar_model 값 설정 (basic이 TRUE이면 Mplus 전용 모델 제외)
  if (basic) {
    covar_model <- c("EII", "EEE", "VII", "EEI")
  }

  # 데이터를 long 형식으로 변환
  to_plot_wide <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range,
                                       set_n_profile=set_n_profile)

  # 데이터 내 실제 존재하는 covariance 모델만 필터링
  existing_models <- colnames(to_plot_wide)[-1]  # 첫 번째 열은 'n_profiles'이므로 제외
  covar_model <- intersect(covar_model, existing_models)  # 실제 데이터에 존재하는 모델만 선택

  to_plot <- to_plot_wide %>%
    gather(`Covariance matrix structure`, val, -n_profiles) %>%
    filter(`Covariance matrix structure` %in% covar_model) %>%
    mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`),
           val = abs(as.numeric(val)))  # val을 숫자로 변환 후 절댓값 적용

  # 기본 covar_model 값 설정 (basic이 TRUE인 경우)
  if (basic) {
    to_plot$`Covariance matrix structure` <- fct_relevel(
      to_plot$`Covariance matrix structure`,
      "EII", "EEE", "VII", "EEI"
    )
  } else {
    to_plot$`Covariance matrix structure` <- fct_relevel(
      to_plot$`Covariance matrix structure`,
      "EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"
    )
  }

  # flip이 TRUE이면 y값을 반전
  if (flip) {
    to_plot$val <- -to_plot$val  # y값을 반전
  }

  # 플롯 생성
  gg <- ggplot(to_plot, aes(x = n_profiles, y = val,
                            color = `Covariance matrix structure`,
                            group = `Covariance matrix structure`)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, color = "black") +
    ggrepel::geom_text_repel(aes(label =
                                   paste0(`Covariance matrix structure`,":",round(val, 2))),
                             vjust = -0.9, hjust=0.5, size = size.text, min.segment.length = 0.8) +
    ylab(ifelse(flip, "Flipped BIC (larger value is better)", "BIC (smaller value is better)")) +
    guides(linewidth = "none", alpha = "none") +
    geom_vline(xintercept = xintercept, linetype = "dashed", color = "tomato") +
    theme_bw() +
    theme(legend.position = legend_position)

  # 결과 반환
  # to_plot= to_plot%>%rename(Gaussian_Mixture_Model=`Covariance matrix structure`)

  res <- list(data = to_plot_wide, graph = gg,  to_plot)

  switch( type, res = res, all = res,
          data = to_plot_wide, long_data= to_long ,
          wide_data =to_plot_wide,
          graph= gg, gg =gg  )

}



#' lpa_BIC_plot3, all plot
#'
#' @param data data
#' @param n_profiles 1:9
#' @param modelNames  NULL, if you want input  c("EEV","EEI","EEE","EII"),
#' @param v 0, abline v = 0
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' select(iris, -Species)%>%lpa_BIC_plot3()
#' }
#'
lpa_BIC_plot3 <-  function(data,
                           n_profiles=1:9,
                           modelNames =NULL,
                           v=0
){
  library(mclust)
  bic_data <- mclustBIC(data, G = n_profiles, modelNames =modelNames)

  gg<- plot(bic_data )
  abline(v=v, lty=2, col="red")
  grid()
  res=list(bic_data, gg)
  res

}

