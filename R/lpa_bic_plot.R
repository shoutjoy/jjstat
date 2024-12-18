#' BIC Plot for Latent Profile Analysis (LPA)
#'
#' @description This function generates a BIC plot for comparing covariance structures
#'              across different numbers of profiles in Latent Profile Analysis (LPA).
#'
#' @param data A data frame for LPA modeling.
#' @param n_profiles_range A numeric vector specifying the range of profiles to test (default: 1:6).
#' @param type A string specifying the output type: "all", "data", "long_data", "wide_data", "graph".
#' @param legend_position A string specifying legend position in the plot (default: "top").
#' @param covar_model A character vector specifying covariance models (default: c("EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV")).
#' @param set_n_profile A numeric value specifying the selected number of profiles (default: 4).
#' @param size.text Numeric, size of text annotations in the plot (default: 4).
#' @param flip Logical, whether to flip BIC values for plotting (default: TRUE).
#' @param na2zero Logical, whether to replace NA values with zeros in BIC results (default: FALSE).
#' @param basic Logical, whether to use a basic set of covariance models (default: TRUE).
#'
#' @return A list containing the wide data, long data, and the generated ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' lpa_BIC_plot(edLca, 1:8, basic = FALSE, set_n_profile = 4)
#' }
lpa_BIC_plot <- function(data, n_profiles_range = 1:6, type = "all",
                         legend_position = "top",
                         covar_model = c("EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"),
                         set_n_profile = 4,
                         size.text = 4,
                         flip = TRUE,
                         na2zero = FALSE,
                         basic = TRUE) {
  library(forcats)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)

  # Step 1: Default covariance models if basic is TRUE
  if (basic) {
    covar_model <- c("EII", "EEE", "VII", "EEI")
  }

  # Step 2: Generate wide-format data using lpa_explore_modelfit
  to_plot_wide <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range,
                                       set_n_profile = set_n_profile, na2zero = na2zero)

  # Step 3: Ensure 'n_profiles' column exists
  if (!"n_profiles" %in% colnames(to_plot_wide)) {
    to_plot_wide <- to_plot_wide %>% mutate(n_profiles = row_number())
  }

  # Step 4: Select numeric columns and include 'n_profiles'
  numeric_cols <- to_plot_wide[, sapply(to_plot_wide, is.numeric) | colnames(to_plot_wide) == "n_profiles"]

  # Step 5: Filter existing covariance models
  existing_models <- colnames(numeric_cols)[-1]  # Exclude 'n_profiles'
  covar_model <- intersect(covar_model, existing_models)

  # Step 6: Transform data to long format using pivot_longer
  to_plot <- numeric_cols %>%
    pivot_longer(cols = -n_profiles,
                 names_to = "Covariance matrix structure",
                 values_to = "val") %>%
    filter(`Covariance matrix structure` %in% covar_model) %>%
    mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`),
           val = abs(as.numeric(val)))

  # Step 7: Reorder covariance matrix structure
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

  # Step 8: Flip BIC values if flip is TRUE
  if (flip) {
    to_plot$val <- -to_plot$val
  }

  # Step 9: Generate ggplot object
  gg <- ggplot(to_plot, aes(x = n_profiles, y = val,
                            color = `Covariance matrix structure`,
                            group = `Covariance matrix structure`)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, color = "black") +
    ggrepel::geom_text_repel(aes(label =
                                   paste0(`Covariance matrix structure`, ":", round(val, 2))),
                             vjust = -0.9, hjust = 0.5, size = size.text, min.segment.length = 0.8) +
    ylab(ifelse(flip, "Flipped BIC (larger value is better)", "BIC (smaller value is better)")) +
    guides(linewidth = "none", alpha = "none") +
    geom_vline(xintercept = set_n_profile, linetype = "dashed", color = "tomato") +
    theme_bw() +
    theme(legend.position = legend_position)

  # Step 10: Return results based on type
  res <- list(data = to_plot_wide, graph = gg, to_plot = to_plot)

  switch(type,
         res = res,
         all = res,
         data = to_plot_wide,
         long_data = to_plot,
         wide_data = to_plot_wide,
         graph = gg, gg = gg)
}












#' #' lpa_BIC_plot
#' #'
#' #' @param data data
#' #' @param n_profiles_range 1:9
#' #' @param type  "all", res, data, long_data, wide_data, graph, gg
#' #' @param legend_position top
#' #' @param covar_model "EEI", "EEE", "VVI", "VVV", "EEV", "EII", "VII",VVV
#' #' @param set_n_profile model 4 default
#' #' @param size.text size.text 4
#' #' @param flip y value <0 default
#' #' @param na2zero na2zero=FALSE na to 0 change
#' #' @param basic basic is "EII", "EEE", "VII", "EEI"
#' #'
#' #' @return pot
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #'
#' #' select(iris, -Species)%>%lpa_BIC_plot()
#' #' }
#' #'
#' lpa_BIC_plot <- function(data, n_profiles_range = 1:6, type="all",
#'                          legend_position = "top",
#'                          covar_model = c("EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"),
#'                          set_n_profile= 4,
#'                          size.text = 4,
#'                          flip = TRUE,  # flip option added
#'                          na2zero=FALSE,
#'                          basic = TRUE) {  # basic option added
#'   library(forcats)
#'   library(dplyr)
#'   library(tidyr)  # 최신 pivot_longer 사용을 위해 필요
#'
#'   # 기본 covar_model 값 설정 (basic이 TRUE이면 Mplus 전용 모델 제외)
#'   if (basic) {
#'     covar_model <- c("EII", "EEE", "VII", "EEI")
#'   }
#'
#'   # 데이터를 wide 형식으로 변환
#'   to_plot_wide <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range,
#'                                        set_n_profile=set_n_profile, na2zero =na2zero)
#'
#'   # 'n_profiles' 열이 없는 경우 'row_number()'로 생성
#'   if (!"n_profiles" %in% colnames(to_plot_wide)) {
#'     to_plot_wide <- to_plot_wwide %>% mutate(n_profiles = row_number())
#'   }
#'
#'   # 숫자형 열만 선택 (문자형 열 제외)
#'   numeric_cols <- to_plot_wide %>% select(where(is.numeric), n_profiles)
#'
#'   # 데이터 내 실제 존재하는 covariance 모델만 필터링
#'   existing_models <- colnames(numeric_cols)[-1]  # 첫 번째 열은 'n_profiles'이므로 제외
#'   covar_model <- intersect(covar_model, existing_models)  # 실제 데이터에 존재하는 모델만 선택
#'
#'   # pivot_longer를 사용하여 long 형식으로 변환
#'   to_plot <- numeric_cols %>%
#'     pivot_longer(cols = -n_profiles,
#'                  names_to = "Covariance matrix structure",
#'                  values_to = "val") %>%
#'     filter(`Covariance matrix structure` %in% covar_model) %>%
#'     mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`),
#'            val = abs(as.numeric(val)))  # val을 숫자로 변환 후 절댓값 적용
#'
#'   # 기본 covar_model 값 설정 (basic이 TRUE인 경우)
#'   if (basic) {
#'     to_plot$`Covariance matrix structure` <- fct_relevel(
#'       to_plot$`Covariance matrix structure`,
#'       "EII", "EEE", "VII", "EEI"
#'     )
#'   } else {
#'     to_plot$`Covariance matrix structure` <- fct_relevel(
#'       to_plot$`Covariance matrix structure`,
#'       "EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"
#'     )
#'   }
#'
#'   # flip이 TRUE이면 y값을 반전
#'   if (flip) {
#'     to_plot$val <- -to_plot$val  # y값을 반전
#'   }
#'
#'   # 플롯 생성
#'   gg <- ggplot(to_plot, aes(x = n_profiles, y = val,
#'                             color = `Covariance matrix structure`,
#'                             group = `Covariance matrix structure`)) +
#'     geom_line(linewidth = 1) +
#'     geom_point(size = 2, color = "black") +
#'     ggrepel::geom_text_repel(aes(label =
#'                                    paste0(`Covariance matrix structure`,":",round(val, 2))),
#'                              vjust = -0.9, hjust=0.5, size = size.text, min.segment.length = 0.8) +
#'     ylab(ifelse(flip, "Flipped BIC (larger value is better)", "BIC (smaller value is better)")) +
#'     guides(linewidth = "none", alpha = "none") +
#'     geom_vline(xintercept = set_n_profile, linetype = "dashed", color = "tomato") +
#'     theme_bw() +
#'     theme(legend.position = legend_position)
#'
#'   # 결과 반환
#'   res <- list(data = to_plot_wide, graph = gg, to_plot = to_plot)
#'
#'   switch(type,
#'          res = res,
#'          all = res,
#'          data = to_plot_wide,
#'          long_data = to_plot,
#'          wide_data = to_plot_wide,
#'          graph = gg, gg = gg)
#' }
#'
#' # lpa_BIC_plot <- function(data, n_profiles_range = 1:9){
#' #   library(forcats)
#' #
#' #   #long data trnasformation
#' #   to_plot <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range)%>%
#' #     gather(`Covariance matrix structure`, val, -n_profiles) %>%
#' #     mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`), val = abs(val))
#' #
#' #   # C- variable arrange
#' #   to_plot$`Covariance matrix structure` <- fct_relevel(
#' #     to_plot$`Covariance matrix structure`,
#' #     "Constrained variance, fixed covariance",
#' #     "Freed variance, fixed covariance",
#' #     "Constrained variance, constrained covariance",
#' #     "Freed variance, freed covariance")
#' #
#' #   #plotting
#' #   gg <-ggplot(to_plot, aes(x = n_profiles, y = val,
#' #                            color = `Covariance matrix structure`,
#' #                            group = `Covariance matrix structure`)) +
#' #     geom_line(linewidth = 1) +
#' #     geom_point(size=2) +
#' #     ylab("BIC (smaller value is better)") +
#' #     guides( linewidth="none", alpha="none")+
#' #     theme_bw()
#' #
#' #   res= list(data = to_plot, graph = gg)
#' #   res
#' #
#' # }
#'
#'
#' #' lpa_BIC_plot2
#' #'
#' #' @param data data
#' #' @param n_profiles_range 1:9
#' #' @param type  "all", res, data, long_data, wide_data, graph, gg
#' #' @param legend_position top
#' #' @param covar_model "EEI", "EEE", "VVI", "VVV", "EEV", "EII", "VII",VVV
#' #' @param set_n_profile model 4 default
#' #' @param size.text size.text 4
#' #' @param flip y value <0 default
#' #' @param na2zero na2zero=FALSE na to 0 change
#' #' @param basic basic is "EII", "EEE", "VII", "EEI"
#' #'
#' #' @return pot
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #'
#' #' select(iris, -Species)%>%lpa_BIC_plot()
#' #' }
#' #'
#' lpa_BIC_plot2 <- function(data, n_profiles_range = 1:6, type="all",
#'                          legend_position = "top",
#'                          covar_model = c("EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"),
#'                          set_n_profile= 4,
#'                          size.text = 4,
#'                          flip = FALSE,  # flip option added
#'                          na2zero=FALSE,
#'                          basic = TRUE) {  # basic option added
#'   library(forcats)
#'   library(dplyr)
#'   library(tidyr)  # 최신 pivot_longer 사용을 위해 필요
#'
#'   # 기본 covar_model 값 설정 (basic이 TRUE이면 Mplus 전용 모델 제외)
#'   if (basic) {
#'     covar_model <- c("EII", "EEE", "VII", "EEI")
#'   }
#'
#'   # 데이터를 wide 형식으로 변환
#'   to_plot_wide <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range,
#'                                        set_n_profile=set_n_profile, na2zero =na2zero)
#'
#'   # 'n_profiles' 열이 없는 경우 'row_number()'로 생성
#'   if (!"n_profiles" %in% colnames(to_plot_wide)) {
#'     to_plot_wide <- to_plot_wwide %>% mutate(n_profiles = row_number())
#'   }
#'
#'   # 숫자형 열만 선택 (문자형 열 제외)
#'   numeric_cols <- to_plot_wide %>% select(where(is.numeric), n_profiles)
#'
#'   # 데이터 내 실제 존재하는 covariance 모델만 필터링
#'   existing_models <- colnames(numeric_cols)[-1]  # 첫 번째 열은 'n_profiles'이므로 제외
#'   covar_model <- intersect(covar_model, existing_models)  # 실제 데이터에 존재하는 모델만 선택
#'
#'   # pivot_longer를 사용하여 long 형식으로 변환
#'   to_plot <- numeric_cols %>%
#'     pivot_longer(cols = -n_profiles,
#'                  names_to = "Covariance matrix structure",
#'                  values_to = "val") %>%
#'     filter(`Covariance matrix structure` %in% covar_model) %>%
#'     mutate(`Covariance matrix structure` = as.factor(`Covariance matrix structure`),
#'            val = abs(as.numeric(val)))  # val을 숫자로 변환 후 절댓값 적용
#'
#'   # 기본 covar_model 값 설정 (basic이 TRUE인 경우)
#'   if (basic) {
#'     to_plot$`Covariance matrix structure` <- fct_relevel(
#'       to_plot$`Covariance matrix structure`,
#'       "EII", "EEE", "VII", "EEI"
#'     )
#'   } else {
#'     to_plot$`Covariance matrix structure` <- fct_relevel(
#'       to_plot$`Covariance matrix structure`,
#'       "EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"
#'     )
#'   }
#'
#'   # flip이 TRUE이면 y값을 반전
#'   if (flip) {
#'     to_plot$val <- -to_plot$val  # y값을 반전
#'   }
#'
#'   # 플롯 생성
#'   gg <- ggplot(to_plot, aes(x = n_profiles, y = val,
#'                             color = `Covariance matrix structure`,
#'                             group = `Covariance matrix structure`)) +
#'     geom_line(linewidth = 1) +
#'     geom_point(size = 2, color = "black") +
#'     ggrepel::geom_text_repel(aes(label =
#'                                    paste0(`Covariance matrix structure`,":",round(val, 2))),
#'                              vjust = -0.9, hjust=0.5, size = size.text, min.segment.length = 0.8) +
#'     ylab(ifelse(flip, "Flipped BIC (larger value is better)", "BIC (smaller value is better)")) +
#'     guides(linewidth = "none", alpha = "none") +
#'     geom_vline(xintercept = set_n_profile, linetype = "dashed", color = "tomato") +
#'     theme_bw() +
#'     theme(legend.position = legend_position)
#'
#'   # 결과 반환
#'   res <- list(data = to_plot_wide, graph = gg, to_plot = to_plot)
#'
#'   switch(type,
#'          res = res,
#'          all = res,
#'          data = to_plot_wide,
#'          long_data = to_plot,
#'          wide_data = to_plot_wide,
#'          graph = gg, gg = gg)
#' }
#'
#'
#'
#' #' lpa_BIC_plot3, all plot
#' #'
#' #' @param data data
#' #' @param n_profiles 1:9
#' #' @param modelNames  NULL, if you want input  c("EEV","EEI","EEE","EII"),
#' #' @param v 0, abline v = 0
#' #'
#' #' @return plot
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #'
#' #' select(iris, -Species)%>%lpa_BIC_plot3()
#' #' }
#' #'
#' lpa_BIC_plot3 <-  function(data,
#'                            n_profiles=1:9,
#'                            modelNames =NULL,
#'                            v=0
#' ){
#'   library(mclust)
#'   bic_data <- mclustBIC(data, G = n_profiles, modelNames =modelNames)
#'
#'   gg<- plot(bic_data )
#'   abline(v=v, lty=2, col="red")
#'   grid()
#'   res=list(bic_data, gg)
#'   res
#'
#' }
#'
