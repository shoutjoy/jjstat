#' Latent Profile Analysis Profile Plot
#'
#' @param data A data frame containing numeric variables for latent profile analysis.
#' @param n_profiles Integer. Number of latent profiles to identify. Default is 3.
#' @param model_name Character. Model name to use in Mclust for Gaussian mixture modeling (e.g., "EEE", "EEI", "VVI", "VVV"). Default is "EEE".
#' @param view Character. Determines the view type of the plots ("pair", "each"). Default is "pair".
#' @param alpha Numeric. Transparency level for bar plots. Default is 0.6.
#' @param gtype Character. Graph type to generate ("normal", "trans", "each"). Default is "normal".
#' @param angle Numeric. Angle for the x-axis text labels. Default is 90.
#' @param size.x Numeric. Font size for the x-axis text labels. Default is 13.
#' @param size.p Numeric. Size of points in the plot. Default is 5.
#' @param ncol Integer. Number of columns for the facet plots in "each" view. Default is 2.
#' @param fct_reorder Character vector or NULL. Specifies the order of factor levels for plotting. Default is NULL.
#' @param show.legend Logical. Whether to show legends for line plots. Default is FALSE.
#' @param legend.text.size Numeric. Font size for legend text. Default is 16.
#' @param legend_position Character. Position of the legend ("top", "bottom", "left", "right"). Default is "top".
#' @param linewidth Numeric. Line width for plots. Default is 1.
#' @param title_size Numeric. Font size for plot titles. Default is 18.
#' @param seed Integer or NULL. Seed value for reproducibility. Default is 123.
#'
#' @return A list containing:
#'   \item{std}{Standardized mean data for each profile.}
#'   \item{est}{Estimated raw mean data for each profile.}
#'   \item{graph}{Combined plots for the profiles.}
#'   \item{plot_data}{Data used for plotting.}
#'   \item{Class_freq}{Frequency table of profile classifications.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage with iris dataset
#' result <- plot_lpa(iris[, -5], n_profiles = 3, model_name = "EEE")
#' print(result$graph)
#' }
plot_lpa <- function(data, n_profiles = 3,
                             model_name = "EEE",
                             view = "pair", alpha = 0.6,
                             gtype = "normal", angle = 90,
                             size.x = 13, size.p = 5, ncol = 2,
                             fct_reorder = NULL,  # fct_reorder 추가
                             show.legend = FALSE,
                             legend.text.size = 16,
                             legend_position = "top",
                             linewidth = 1, title_size = 18,
                             seed = 123) {  # set.seed 추가
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)
  library(gridExtra)

  # set.seed 추가 (seed 값이 있으면 사용)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # 프로파일 레이블 정의 추가
  profile_labels <- setNames(paste0("Profile ", 1:n_profiles), 1:n_profiles)

  # 잠재 프로파일 분석 수행 (평균과 표준편차 모두 사용)
  lpa_all <- lpa_create_profiles2(df = data, n_profiles = n_profiles,
                                  model_name = model_name, type = "res", seed = seed)

  mean_data <- lpa_all$mean
  sd_data <- lpa_all$sd

  # 표준편차 데이터를 긴 형식으로 변환
  sd_data_long <- sd_data %>%
    pivot_longer(cols = -profile, names_to = "key", values_to = "sd_val")

  # 평균 데이터를 긴 형식으로 변환하고 표준편차 데이터와 병합
  plot_data <- mean_data %>%
    pivot_longer(cols = -profile, names_to = "key", values_to = "val") %>%
    left_join(sd_data_long, by = c("profile", "key"))

  # fct_reorder가 NULL이 아닌 경우, key 및 factor의 순서를 변경
  if (!is.null(fct_reorder)) {
    plot_data <- plot_data %>%
      mutate(key = factor(key, levels = fct_reorder))
  }

  # 비표준화 데이터를 위한 Mclust 결과 가져오기
  set.seed(seed)
  raw_data <- Mclust(data, G = n_profiles, modelNames = model_name)
  raw_mean_data <- raw_data$parameters$mean %>% as_tibble()

  names(raw_mean_data) <- str_c("profile", 1:n_profiles)
  raw_mean_data$factor <- colnames(data)
  raw_mean_data <- raw_mean_data %>% dplyr::select(factor, 1:ncol(raw_mean_data) - 1)

  # fct_reorder가 NULL이 아닌 경우 factor 변수의 순서 재정렬
  if (!is.null(fct_reorder)) {
    raw_mean_data <- raw_mean_data %>%
      mutate(factor = factor(factor, levels = fct_reorder))
  }

  # 프로파일의 빈도수 계산
  class_assignments <- raw_data$classification
  Class_freq <- table(class_assignments)

  # 그래프 생성
  if (gtype == "normal") {
    gg <- plot_data %>%
      ggplot(aes(x = profile, y = val, fill = key, group = key)) +
      geom_col(position = "dodge", alpha = alpha + 0.1, color = NA) +  # 막대 그래프에서 border 제거
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),
                    position = position_dodge(width = 0.9), width = 0.25) +
      geom_point(aes(shape = key), size = 2, position = position_dodge(width = 0.9), show.legend = TRUE) +
      scale_x_discrete(labels = profile_labels) +  # x축 레이블에 profile_labels 추가
      ylab("Z-score") +
      xlab("(b) 표준화 자료(scale)") +
      scale_fill_discrete("") +
      labs(title = "Standardized score of the average of LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.text = element_text(size = legend.text.size),
            legend.position = legend_position,
            legend.title = element_blank())

    gg2 <- raw_mean_data %>%
      pivot_longer(cols = -factor, names_to = "profile", values_to = "val") %>%
      ggplot(aes(x = profile, y = val, fill = factor, group = factor)) +
      geom_col(position = "dodge", alpha = alpha + 0.1, color = NA) +
      geom_point(aes(shape = factor), size = 2, position = position_dodge(width = 0.9), show.legend = TRUE) +
      scale_x_discrete(labels = profile_labels) +  # x축 레이블에 profile_labels 추가
      ylab("Raw-score") +
      xlab("(a) 원데이터(Raw)") +
      labs(title = "Raw score of the average of the LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.text = element_text(size = legend.text.size),
            legend.position = legend_position,
            legend.title = element_blank())

    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = 2)

  } else if (gtype == "trans") {
    gg <- plot_data %>%
      ggplot(aes(x = key, y = val, fill = profile, group = profile)) +
      geom_col(position = "dodge", alpha = alpha, color = NA) +
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),
                    position = position_dodge(width = 0.9), width = 0.25) +
      geom_line(aes(group = profile), color = "black", size = linewidth, show.legend = TRUE) +
      geom_hline(yintercept = 0)+
      geom_point(aes(group = profile, shape = profile, color = profile), size = size.p, show.legend = TRUE) +
      ylab("Z-score") +
      xlab("(b) Standardization data") +
      scale_fill_discrete("") +
      labs(title = "Standardized score of the average of LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.position = legend_position,
            legend.title = element_blank(),
            plot.title = element_text(size = title_size))

    gg2 <- raw_mean_data %>%
      pivot_longer(cols = -factor, names_to = "profile", values_to = "val") %>%
      ggplot(aes(x = factor, y = val, fill = profile, group = profile)) +
      geom_col(position = "dodge", alpha = alpha, color = NA) +
      geom_line(aes(group = profile), color = "black", size = linewidth, show.legend = TRUE) +
      geom_point(aes(group = profile, shape = profile, color = profile), size = size.p, show.legend = TRUE) +
      ylab("Raw-score") +
      xlab("(a) Raw data") +
      labs(title = "Raw score of the average of the LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.position = legend_position,
            legend.title = element_blank(),
            plot.title = element_text(size = title_size))

    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = 2)

  } else if (gtype == "each") {
    gg <- plot_data %>%
      ggplot(aes(x = key, y = val, group = profile)) +
      geom_col(aes(fill = key), position = "dodge", color = NA, show.legend = show.legend) +
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),
                    position = position_dodge(width = 0.9), width = 0.25) +
      geom_line(aes(group = profile), size = 1, show.legend = FALSE) +
      geom_point(aes(group = key, shape = key), color = "black", size = size.p, show.legend = FALSE) +
      facet_wrap(~ profile, labeller = labeller(profile = profile_labels)) +
      ylab("Z-score") +
      xlab("(a) Profile Decomposition") +
      scale_fill_discrete("") +
      labs(title = "Standardized score of the average of LPA") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            strip.text = element_text(size = size.x, face = "bold"))

    gg2 <- raw_mean_data %>%
      pivot_longer(cols = -factor, names_to = "profile", values_to = "val") %>%
      ggplot(aes(x = factor, y = val, group = profile)) +
      geom_col(aes(fill = factor), position = "dodge", color = NA, show.legend = show.legend) +
      geom_line(aes(group = profile), size = 1, show.legend = FALSE) +
      geom_point(aes(group = factor, shape = factor), color = "black", size = size.p, show.legend = FALSE) +
      facet_wrap(~ profile, labeller = labeller(profile = profile_labels)) +
      ylab("Raw-score") +
      xlab("(b) Profile Overlap") +
      scale_fill_discrete("") +
      labs(title = "Raw score of the average of the LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            strip.text = element_text(size = size.x, face = "bold"))

    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = ncol)
  }

  # 결과 반환
  res <- list(std = mean_data, est = raw_mean_data,
              graph = ggg, plot_data = data.frame(mean_data),
              Class_freq = Class_freq)

  return(res)
}

#' Latent Profile Analysis Profile Plot
#'
#' @param data A data frame containing numeric variables for latent profile analysis.
#' @param n_profiles Integer. Number of latent profiles to identify. Default is 3.
#' @param model_name Character. Model name to use in Mclust for Gaussian mixture modeling (e.g., "EEE", "EEI", "VVI", "VVV"). Default is "EEE".
#' @param view Character. Determines the view type of the plots ("pair", "each"). Default is "pair".
#' @param alpha Numeric. Transparency level for bar plots. Default is 0.6.
#' @param gtype Character. Graph type to generate ("normal", "trans", "each"). Default is "normal".
#' @param angle Numeric. Angle for the x-axis text labels. Default is 90.
#' @param size.x Numeric. Font size for the x-axis text labels. Default is 13.
#' @param size.p Numeric. Size of points in the plot. Default is 5.
#' @param ncol Integer. Number of columns for the facet plots in "each" view. Default is 2.
#' @param fct_reorder Character vector or NULL. Specifies the order of factor levels for plotting. Default is NULL.
#' @param show.legend Logical. Whether to show legends for line plots. Default is FALSE.
#' @param legend.text.size Numeric. Font size for legend text. Default is 16.
#' @param legend_position Character. Position of the legend ("top", "bottom", "left", "right"). Default is "top".
#' @param linewidth Numeric. Line width for plots. Default is 1.
#' @param title_size Numeric. Font size for plot titles. Default is 18.
#' @param seed Integer or NULL. Seed value for reproducibility. Default is 123.
#'
#' @return A list containing:
#'   \item{std}{Standardized mean data for each profile.}
#'   \item{est}{Estimated raw mean data for each profile.}
#'   \item{graph}{Combined plots for the profiles.}
#'   \item{plot_data}{Data used for plotting.}
#'   \item{Class_freq}{Frequency table of profile classifications.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage with iris dataset
#' result <- lpa_plot(iris[, -5], n_profiles = 3, model_name = "EEE")
#' print(result$graph)
#' }
lpa_plot <- function(data, n_profiles = 3,
                     model_name = "EEE",
                     view = "pair", alpha = 0.6,
                     gtype = "normal", angle = 90,
                     size.x = 13, size.p = 5, ncol = 2,
                     fct_reorder = NULL,  # fct_reorder 추가
                     show.legend = FALSE,
                     legend.text.size = 16,
                     legend_position = "top",
                     linewidth = 1, title_size = 18,
                     seed = 123) {  # set.seed 추가
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)
  library(gridExtra)

  # set.seed 추가 (seed 값이 있으면 사용)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # 프로파일 레이블 정의 추가
  profile_labels <- setNames(paste0("Profile ", 1:n_profiles), 1:n_profiles)

  # 잠재 프로파일 분석 수행 (평균과 표준편차 모두 사용)
  lpa_all <- lpa_create_profiles2(df = data, n_profiles = n_profiles,
                                  model_name = model_name, type = "res", seed = seed)

  mean_data <- lpa_all$mean
  sd_data <- lpa_all$sd

  # 표준편차 데이터를 긴 형식으로 변환
  sd_data_long <- sd_data %>%
    pivot_longer(cols = -profile, names_to = "key", values_to = "sd_val")

  # 평균 데이터를 긴 형식으로 변환하고 표준편차 데이터와 병합
  plot_data <- mean_data %>%
    pivot_longer(cols = -profile, names_to = "key", values_to = "val") %>%
    left_join(sd_data_long, by = c("profile", "key"))

  # fct_reorder가 NULL이 아닌 경우, key 및 factor의 순서를 변경
  if (!is.null(fct_reorder)) {
    plot_data <- plot_data %>%
      mutate(key = factor(key, levels = fct_reorder))
  }

  # 비표준화 데이터를 위한 Mclust 결과 가져오기
  set.seed(seed)
  raw_data <- Mclust(data, G = n_profiles, modelNames = model_name)
  raw_mean_data <- raw_data$parameters$mean %>% as_tibble()

  names(raw_mean_data) <- str_c("profile", 1:n_profiles)
  raw_mean_data$factor <- colnames(data)
  raw_mean_data <- raw_mean_data %>% dplyr::select(factor, 1:ncol(raw_mean_data) - 1)

  # fct_reorder가 NULL이 아닌 경우 factor 변수의 순서 재정렬
  if (!is.null(fct_reorder)) {
    raw_mean_data <- raw_mean_data %>%
      mutate(factor = factor(factor, levels = fct_reorder))
  }

  # 프로파일의 빈도수 계산
  class_assignments <- raw_data$classification
  Class_freq <- table(class_assignments)

  # 그래프 생성
  if (gtype == "normal") {
    gg <- plot_data %>%
      ggplot(aes(x = profile, y = val, fill = key, group = key)) +
      geom_col(position = "dodge", alpha = alpha + 0.1, color = NA) +  # 막대 그래프에서 border 제거
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),
                    position = position_dodge(width = 0.9), width = 0.25) +
      geom_point(aes(shape = key), size = 2, position = position_dodge(width = 0.9), show.legend = TRUE) +
      scale_x_discrete(labels = profile_labels) +  # x축 레이블에 profile_labels 추가
      ylab("Z-score") +
      xlab("(b) 표준화 자료(scale)") +
      scale_fill_discrete("") +
      labs(title = "Standardized score of the average of LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.text = element_text(size = legend.text.size),
            legend.position = legend_position,
            legend.title = element_blank())

    gg2 <- raw_mean_data %>%
      pivot_longer(cols = -factor, names_to = "profile", values_to = "val") %>%
      ggplot(aes(x = profile, y = val, fill = factor, group = factor)) +
      geom_col(position = "dodge", alpha = alpha + 0.1, color = NA) +
      geom_point(aes(shape = factor), size = 2, position = position_dodge(width = 0.9), show.legend = TRUE) +
      scale_x_discrete(labels = profile_labels) +  # x축 레이블에 profile_labels 추가
      ylab("Raw-score") +
      xlab("(a) 원데이터(Raw)") +
      labs(title = "Raw score of the average of the LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.text = element_text(size = legend.text.size),
            legend.position = legend_position,
            legend.title = element_blank())

    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = 2)

  } else if (gtype == "trans") {
    gg <- plot_data %>%
      ggplot(aes(x = key, y = val, fill = profile, group = profile)) +
      geom_col(position = "dodge", alpha = alpha, color = NA) +
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),
                    position = position_dodge(width = 0.9), width = 0.25) +
      geom_line(aes(group = profile), color = "black", size = linewidth, show.legend = TRUE) +
      geom_hline(yintercept = 0)+
      geom_point(aes(group = profile, shape = profile, color = profile),
                 size = size.p, show.legend = TRUE) +
      ylab("Z-score") +
      xlab("(b) Standardization data") +
      scale_fill_discrete("") +
      labs(title = "Standardized score of the average of LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.position = legend_position,
            legend.title = element_blank(),
            plot.title = element_text(size = title_size))

    gg2 <- raw_mean_data %>%
      pivot_longer(cols = -factor, names_to = "profile", values_to = "val") %>%
      ggplot(aes(x = factor, y = val, fill = profile, group = profile)) +
      geom_col(position = "dodge", alpha = alpha, color = NA) +
      geom_line(aes(group = profile), color = "black", size = linewidth, show.legend = TRUE) +
      geom_point(aes(group = profile, shape = profile,
                     color = profile), size = size.p, show.legend = TRUE) +
      ylab("Raw-score") +
      xlab("(a) Raw data") +
      labs(title = "Raw score of the average of the LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.position = legend_position,
            legend.title = element_blank(),
            plot.title = element_text(size = title_size))

    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = 2)

  } else if (gtype == "each") {
    gg <- plot_data %>%
      ggplot(aes(x = key, y = val, group = profile)) +
      geom_col(aes(fill = key), position = "dodge", color = NA, show.legend = show.legend) +
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),
                    position = position_dodge(width = 0.9), width = 0.25) +
      geom_line(aes(group = profile), size = 1, show.legend = FALSE) +
      geom_point(aes(group = key, shape = key), color = "black", size = size.p, show.legend = FALSE) +
      facet_wrap(~ profile, labeller = labeller(profile = profile_labels)) +
      ylab("Z-score") +
      xlab("(a) Profile Decomposition") +
      scale_fill_discrete("") +
      labs(title = "Standardized score of the average of LPA") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            strip.text = element_text(size = size.x, face = "bold"))

    gg2 <- raw_mean_data %>%
      pivot_longer(cols = -factor, names_to = "profile", values_to = "val") %>%
      ggplot(aes(x = factor, y = val, group = profile)) +
      geom_col(aes(fill = factor), position = "dodge", color = NA, show.legend = show.legend) +
      geom_line(aes(group = profile), size = 1, show.legend = FALSE) +
      geom_point(aes(group = factor, shape = factor),
                 color = "black", size = size.p, show.legend = FALSE) +
      facet_wrap(~ profile, labeller = labeller(profile = profile_labels)) +
      ylab("Raw-score") +
      xlab("(b) Profile Overlap") +
      scale_fill_discrete("") +
      labs(title = "Raw score of the average of the LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            strip.text = element_text(size = size.x, face = "bold"))

    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = ncol)
  }

  # 결과 반환
  res <- list(std = mean_data, est = raw_mean_data,
              graph = ggg, plot_data = data.frame(mean_data),
              Class_freq = Class_freq)

  x11()
  return(res)
}

