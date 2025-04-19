
#' Plot BIC Values for Latent Profile Analysis using mclust
#'
#' This function fits latent profile models using the `mclust` package
#' and plots BIC values across profile numbers and covariance structures.
#' If a raw data frame is passed, it computes the BIC matrix automatically.
#'
#' @param data A raw data frame (e.g., survey or psychological item data) or a BIC summary table.
#' @param n_profiles_range A numeric vector indicating the number of profiles to evaluate. Default is 1:6.
#' @param type The output type: one of "all", "graph", "data", "long_data", "wide_data". Default is "all".
#' @param legend_position Position of the legend in the plot. Default is "top".
#' @param covar_model A vector of covariance structures to use (e.g., "EII", "EEE", ...). Default includes all.
#' @param set_n_profile Optional: specify number of profiles to highlight with a vertical line.
#' @param xintercept Default x-position for vertical line (dashed).
#' @param size.text Size of text in plot labels. Default is 4.
#' @param flip Whether to multiply BIC values by -1 for plotting (to make larger = better). Default is TRUE.
#' @param na2zero Not used yet (reserved for missing value handling).
#' @param new.window Whether to plot in a new window (only for local use). Default is TRUE.
#' @param basic Whether to restrict to basic models (EII, EEE, VII, EEI). Default is TRUE.
#'
#' @return Depending on `type`, returns a plot object, data table, or list of results.
#' @export
#'
#' @examples
#' # Example using built-in mtcars dataset
#' data <- mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]
#' scaled_data <- scale(data)
#' result <- lpa_BIC_plot(as.data.frame(scaled_data))
#' # 데이터 준비: mtcars에서 LPA에 적합한 일부 변수만 추출
#' lpa_test_data <- mtcars %>%
#'   dplyr::select(mpg, disp, hp, wt, qsec) %>%
#'   scale() %>%         # 정규화
#'   as.data.frame()
#' # 기본 실행: BIC plot 출력
#' lpa_BIC_plot(lpa_test_data)
#'

#'
lpa_BIC_plot <- function(data,
                         n_profiles_range = 1:6,
                         type = "all",
                         legend_position = "top",
                         covar_model = c("EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"),
                         set_n_profile = NULL,
                         xintercept = 4,
                         size.text = 4,
                         flip = TRUE,
                         na2zero = FALSE,
                         new.window = TRUE,
                         basic = TRUE) {

  # Load required libraries
  library(dplyr)
  library(tidyr)
  library(forcats)
  library(ggplot2)
  library(ggrepel)
  library(mclust)

  # Restrict to basic models if required
  if (basic) {
    covar_model <- c("EII", "EEE", "VII", "EEI")
  }

  # Check if input is raw data or BIC summary table
  is_raw_data <- !any(colnames(data) %in% covar_model)
  bic_summary <- NULL

  if (is_raw_data) {
    message("Raw data detected. Running mclustBIC to compute model fit...")
    bic_res <- mclustBIC(data, G = n_profiles_range, modelNames = covar_model)

    bic_long <- as.data.frame(as.table(bic_res)) %>%
      rename(n_profiles = Var1,
             Covariance_matrix_structure = Var2,
             val = Freq) %>%
      mutate(n_profiles = as.integer(as.character(n_profiles))) %>%
      pivot_wider(names_from = Covariance_matrix_structure,
                  values_from = val)

    bic_summary <- bic_long
  } else {
    bic_summary <- data
    if (!"n_profiles" %in% colnames(bic_summary)) {
      bic_summary <- mutate(bic_summary, n_profiles = row_number())
    }
  }

  numeric_cols <- dplyr::select(bic_summary, where(is.numeric), n_profiles)
  existing_models <- intersect(covar_model, colnames(numeric_cols))
  numeric_cols <- dplyr::select(numeric_cols, n_profiles, dplyr::all_of(existing_models))

  to_plot <- numeric_cols %>%
    pivot_longer(cols = -n_profiles,
                 names_to = "Covariance_matrix_structure",
                 values_to = "val") %>%
    filter(!is.na(val)) %>%
    mutate(Covariance_matrix_structure = as.factor(Covariance_matrix_structure))

  to_plot$Covariance_matrix_structure <- fct_relevel(
    to_plot$Covariance_matrix_structure,
    intersect(c("EII", "EEE", "VII", "EEI", "EEV", "VVI", "VVV"),
              levels(to_plot$Covariance_matrix_structure))
  )

  if (flip) {
    to_plot$val <- -1 * to_plot$val
  }

  if (new.window) {
    if (.Platform$OS.type == "windows" || .Platform$OS.type == "unix") {
      x11()
    } else if (.Platform$OS.type == "mac") {
      quartz()
    } else {
      warning("New window plotting is not supported on this platform.")
    }
  }

  gg <- ggplot(to_plot, aes(x = n_profiles, y = val,
                            color = Covariance_matrix_structure,
                            group = Covariance_matrix_structure)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, color = "black") +
    ggrepel::geom_text_repel(aes(label = paste0(Covariance_matrix_structure, ":", round(val, 2))),
                             vjust = -0.9, hjust = 0.5, size = size.text,
                             min.segment.length = 0.8) +
    ylab(ifelse(flip, "Flipped BIC (larger value is better)", "BIC (smaller value is better)")) +
    guides(linewidth = "none", alpha = "none") +
    geom_vline(xintercept = xintercept, linetype = "dashed", color = "tomato") +
    theme_bw() +
    theme(legend.position = legend_position)

  res <- list(data = bic_summary, graph = gg, to_plot = to_plot)

  switch(type,
         res = res,
         all = res,
         data = bic_summary,
         long_data = to_plot,
         wide_data = bic_summary,
         graph = gg, gg = gg)
}

#' #' lpa_BIC_plot: Visualize BIC Values for Latent Profile Analysis
#' #'
#' #' @description This function generates a plot to visualize BIC values from latent profile analysis.
#' #'
#' #' @param data A data frame containing BIC values for different models and profiles.
#' #' @param n_profiles_range A numeric vector specifying the range of profiles to analyze.
#' #' @param type Type of output to return. Options include "all", "res", "data", "long_data", "wide_data", or "graph".
#' #' @param legend_position The position of the legend in the plot (e.g., "top", "bottom").
#' #' @param covar_model A character vector specifying covariance models to include in the analysis.
#' #' @param set_n_profile A fixed number of profiles to use in the analysis (optional).
#' #' @param xintercept The x-axis intercept line for visualization.
#' #' @param size.text The size of text labels in the plot.
#' #' @param flip Logical. If TRUE, flips the y-axis values for visualization. Default is TRUE.
#' #' @param na2zero Logical. If TRUE, replaces NA values with zero. Default is FALSE.
#' #' @param new.window Logical. If TRUE, opens a new graphics window for the plot. Default is TRUE.
#' #' @param basic Logical. If TRUE, uses only basic covariance models (e.g., "EII", "EEE"). Default is TRUE.
#' #'
#' #' @return A list containing:
#' #' \item{data}{The wide-format data used for plotting.}
#' #' \item{graph}{The ggplot object for visualization.}
#' #' \item{to_plot}{The long-format data used for plotting.}
#' #'
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Example usage of lpa_BIC_plot:
#' #' lpa_BIC_plot(data, n_profiles_range = 1:6, basic = FALSE, set_n_profile = 4)
#' #' }
#' #'
#' lpa_BIC_plot <- function(data, n_profiles_range = 1:6, type = "all",
#'                          legend_position = "top",
#'                          covar_model = c("EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"),
#'                          set_n_profile = NULL, xintercept = 4,
#'                          size.text = 4,
#'                          flip = TRUE,
#'                          na2zero = FALSE,
#'                          new.window = TRUE,
#'                          basic = TRUE) {
#'   library(forcats)
#'   library(dplyr)
#'   library(tidyr)
#'   library(ggplot2)
#'   library(ggrepel)
#'
#'   # 기본 covar_model 값 설정 (basic이 TRUE이면 Mplus 전용 모델 제외)
#'   if (basic) {
#'     covar_model <- c("EII", "EEE", "VII", "EEI")
#'   }
#'
#'   # 데이터를 wide 형식으로 변환
#'   to_plot_wide <- lpa_explore_modelfit(data, n_profiles_range = n_profiles_range,
#'                                        set_n_profile = set_n_profile, na2zero = na2zero)
#'
#'   # 'n_profiles' 열이 없는 경우 'row_number()'로 생성
#'   if (!"n_profiles" %in% colnames(to_plot_wide)) {
#'     to_plot_wide <- to_plot_wide %>% mutate(n_profiles = row_number())
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
#'                  names_to = "Covariance_matrix_structure",
#'                  values_to = "val") %>%
#'     filter(Covariance_matrix_structure %in% covar_model) %>%
#'     filter(!is.na(val)) %>%  # 결측치를 제외
#'     mutate(Covariance_matrix_structure = as.factor(Covariance_matrix_structure))
#'
#'   # 기본 covar_model 값 설정 (basic이 TRUE인 경우)
#'   if (basic) {
#'     to_plot$Covariance_matrix_structure <- fct_relevel(
#'       to_plot$Covariance_matrix_structure,
#'       intersect(c("EII", "EEE", "VII", "EEI"), levels(to_plot$Covariance_matrix_structure))
#'     )
#'   } else {
#'     to_plot$Covariance_matrix_structure <- fct_relevel(
#'       to_plot$Covariance_matrix_structure,
#'       intersect(c("EII", "EEE", "VII", "EEV", "EEI", "VVI", "VVV"), levels(to_plot$Covariance_matrix_structure))
#'     )
#'   }
#'
#'   # flip이 TRUE이면 값을 그대로 유지
#'   if (flip) {
#'     to_plot$val <- to_plot$val
#'   }
#'
#'   # 새로운 창에서 플롯을 띄울 옵션 추가
#'   if (new.window) {
#'     if (.Platform$OS.type == "windows" || .Platform$OS.type == "unix") {
#'       x11()  # Windows/Linux 환경
#'     } else if (.Platform$OS.type == "mac") {
#'       quartz()  # macOS 환경
#'     } else {
#'       warning("New window plotting is not supported on this platform.")
#'     }
#'   }
#'
#'   # 플롯 생성
#'   gg <- ggplot(to_plot, aes(x = n_profiles, y = val,
#'                             color = Covariance_matrix_structure,
#'                             group = Covariance_matrix_structure)) +
#'     geom_line(linewidth = 1) +
#'     geom_point(size = 2, color = "black") +
#'     ggrepel::geom_text_repel(aes(label =
#'                                    paste0(Covariance_matrix_structure, ":", round(val, 2))),
#'                              vjust = -0.9, hjust = 0.5, size = size.text, min.segment.length = 0.8) +
#'     ylab(ifelse(flip, "Flipped BIC (larger value is better)", "BIC (smaller value is better)")) +
#'     guides(linewidth = "none", alpha = "none") +
#'     geom_vline(xintercept = xintercept, linetype = "dashed", color = "tomato") +
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
