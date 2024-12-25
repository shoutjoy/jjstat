#' Latent Profile Analysis (LPA) Profile Plot Function
#'
#' This function generates plots to visualize latent profile analysis results, including raw scores and standardized scores for profiles.
#'
#' @param data A data frame containing numeric variables for latent profile analysis.
#' @param n_profiles Integer. Number of latent profiles to identify. Default is 3.
#' @param model_name Character. Model name for Gaussian mixture modeling in Mclust (e.g., "EEE", "EEI", "VVI", "VVV"). Default is "EEE".
#' @param view Character. Determines the view type of the plots ("pair", "each"). Default is "pair".
#' @param alpha Numeric. Transparency level for bar plots. Default is 0.6.
#' @param gtype Character. Type of graph to generate ("normal", "trans"). Default is "normal".
#' @param angle Numeric. Angle for the x-axis text labels. Default is 90.
#' @param size.x Numeric. Font size for the x-axis text labels. Default is 13.
#' @param size.p Numeric. Size of points in the plot. Default is 5.
#' @param ncol Integer. Number of columns for grid-arranged plots. Default is 2.
#' @param fct_reorder Character vector or NULL. Specifies the order of factor levels for plotting. Default is NULL.
#' @param show.legend Logical. Whether to show legends for line plots. Default is FALSE.
#' @param legend.text.size Numeric. Font size for legend text. Default is 16.
#' @param legend_position Character. Position of the legend ("top", "bottom", "left", "right"). Default is "top".
#' @param linewidth Numeric. Line width for line plots. Default is 1.
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
#' # Example usage with the iris dataset
#' result <- lpa_profile_plot(iris[, -5], n_profiles = 3, model_name = "EEE", gtype = "normal")
#' print(result$graph)
#' }
lpa_profile_plot <- function(data, n_profiles = 3,
                             model_name = "EEE",
                             view = "pair", alpha = 0.6,
                             gtype = "normal",
                             angle = 90,
                             size.x = 13, size.p = 5, ncol = 2,
                             fct_reorder = NULL,
                             show.legend = FALSE,
                             legend.text.size = 16,
                             legend_position = "top",
                             linewidth = 1, title_size = 18,
                             seed = 123) {
  # Load necessary libraries
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)
  library(gridExtra)

  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)

  # Define profile labels
  profile_labels <- setNames(paste0("Profile ", 1:n_profiles), 1:n_profiles)

  # Perform Latent Profile Analysis
  lpa_all <- lpa_create_profiles2(df = data, n_profiles = n_profiles,
                                  model_name = model_name, type = "res", seed = seed)

  mean_data <- lpa_all$mean
  sd_data <- lpa_all$sd

  # Transform data for plotting
  sd_data_long <- sd_data %>%
    pivot_longer(cols = -profile, names_to = "key", values_to = "sd_val")

  plot_data <- mean_data %>%
    pivot_longer(cols = -profile, names_to = "key", values_to = "val") %>%
    left_join(sd_data_long, by = c("profile", "key"))

  if (!is.null(fct_reorder)) {
    plot_data <- plot_data %>%
      mutate(key = factor(key, levels = fct_reorder))
  }

  # Raw data processing
  raw_data <- Mclust(data, G = n_profiles, modelNames = model_name)
  raw_mean_data <- as_tibble(raw_data$parameters$mean)

  names(raw_mean_data) <- str_c("profile", 1:n_profiles)
  raw_mean_data$factor <- colnames(data)
  raw_mean_data <- raw_mean_data %>% select(factor, everything())

  if (!is.null(fct_reorder)) {
    raw_mean_data <- raw_mean_data %>%
      mutate(factor = factor(factor, levels = fct_reorder))
  }

  class_assignments <- raw_data$classification
  Class_freq <- table(class_assignments)

  # Create plots
  if (gtype == "normal") {
    gg <- plot_data %>%
      ggplot(aes(x = profile, y = val, group = key)) +
      geom_col(aes(fill = key), position = "dodge", alpha = alpha + 0.1, show.legend = FALSE) +
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),
                    position = position_dodge(width = 0.9), width = 0.25, show.legend = FALSE) +
      geom_point(aes(shape = key), size = 2, position = position_dodge(width = 0.9), show.legend = TRUE) +
      scale_x_discrete(labels = profile_labels) +
      ylab("Z-score") +
      xlab("(b) 표준화 자료(scale)") +
      labs(title = "Standardized score of the average of LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.text = element_text(size = legend.text.size),
            legend.position = legend_position,
            legend.title = element_blank())

    gg2 <- raw_mean_data %>%
      pivot_longer(cols = -factor, names_to = "profile", values_to = "val") %>%
      ggplot(aes(x = profile, y = val, group = factor)) +
      geom_col(aes(fill = factor), position = "dodge", alpha = alpha + 0.1, show.legend = FALSE) +
      geom_point(aes(shape = factor), size = 2, position = position_dodge(width = 0.9), show.legend = TRUE) +
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
      ggplot(aes(x = key, y = val, group = profile)) +
      geom_col(aes(fill = profile), position = "dodge", alpha = alpha, show.legend = FALSE) +
      geom_errorbar(aes(ymin = val - sd_val, ymax = val + sd_val),
                    position = position_dodge(width = 0.9), width = 0.25, show.legend = FALSE) +
      geom_line(aes(group = profile), color = "black", size = linewidth, show.legend = FALSE) +
      geom_point(aes(shape = profile, color = profile), size = size.p, show.legend = TRUE) +
      ylab("Z-score") +
      xlab("(b) Standardization data") +
      labs(title = "Standardized score of the average of LPA") +
      theme_bw() +
      theme(axis.text = element_text(size = size.x, angle = angle, face = "bold"),
            axis.title = element_text(size = size.x),
            legend.position = legend_position,
            legend.title = element_blank(),
            plot.title = element_text(size = title_size))

    gg2 <- raw_mean_data %>%
      pivot_longer(cols = -factor, names_to = "profile", values_to = "val") %>%
      ggplot(aes(x = factor, y = val, group = profile)) +
      geom_col(aes(fill = profile), position = "dodge", alpha = alpha, show.legend = FALSE) +
      geom_point(aes(shape = profile, color = profile), size = size.p, show.legend = TRUE) +
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
  }else if (gtype == "each") {
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

    ggg <- gridExtra::grid.arrange(gg2, gg, ncol = ncol)  # ggg 추가
  }



  # Return results
  res <- list(std = mean_data, est = raw_mean_data,
              graph = ggg, plot_data = data.frame(mean_data),
              Class_freq = Class_freq)

  return(res)
}
