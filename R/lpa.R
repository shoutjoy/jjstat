#' Latent Profile Analysis (LPA) Function
#'
#' This function performs latent profile analysis (LPA) using the `mclust` package, creates visualizations,
#' and returns summary statistics and plots.
#'
#' @param df Data frame containing the numeric variables for LPA.
#' @param profile Integer specifying the number of profiles (clusters). Default is 4.
#' @param model Character vector specifying the covariance model to use in the analysis.
#' Options include "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE",
#' "EEV", "VEV", "EVV", "VVV". Default is "EEE".
#' @param seed Integer for setting the random seed for reproducibility. Default is 123.
#' @param size_axis Numeric value specifying the size of the axis text in plots. Default is 14.
#' @param size_strip Numeric value specifying the size of the strip text in plots. Default is 14.
#' @param name_strip Optional vector of names for customizing profile strip labels.
#' @param md Logical indicating whether to convert the combined data to markdown format. Default is FALSE.
#'
#' @return A list containing:
#'   \item{summary_table}{Data frame with raw means, frequencies, and proportions for each profile.}
#'   \item{scaled_summary_table}{Data frame with standardized means, frequencies, and proportions for each profile.}
#'   \item{profile_frequencies}{Data frame with profile frequencies and proportions.}
#'   \item{combined_data}{Combined data in either raw or markdown format.}
#'   \item{mclust_object}{Summary of the Mclust object from the analysis.}
#'   \item{raw_plot}{Plot of raw scores by profile.}
#'   \item{scaled_plot}{Plot of standardized scores by profile.}
#'   \item{combined_plot}{Combined plot of raw and standardized scores.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- mtcars[, c("mpg", "disp", "hp", "drat", "wt")]
#' result <- lpa(data, profile = 3, model = "EEE", seed = 123)
#' result$raw_plot
#' result$scaled_plot
#' result$combined_data
#' }
lpa <- function(df, profile = 4, model = c("EII", "VII", "EEI", "VEI", "EVI", "VVI",
                                           "EEE", "EVE", "VEE", "VVE", "EEV", "VEV",
                                           "EVV", "VVV"), seed = 123,
                size_axis = 14, size_strip = 14, name_strip = NULL,
                md = FALSE) {
  library(mclust)
  library(ggplot2)
  library(reshape2)
  library(cowplot)
  library(dplyr)

  # Ensure model is valid
  model <- match.arg(model)

  # Set random seed for reproducibility
  set.seed(seed)

  # Standardize the data
  df_scaled <- scale(df)

  # Perform LPA using Mclust with specified model
  mclust_result <- Mclust(df_scaled, G = profile, modelNames = model)

  # Get profile assignments
  profiles <- mclust_result$classification

  # Calculate frequency of each profile
  total_count <- length(profiles)
  profile_freq <- as.data.frame(table(profiles))
  colnames(profile_freq) <- c("Profile", "Freq")
  profile_freq$Profile <- paste0("profile_", profile_freq$Profile)
  profile_freq$prop <- profile_freq$Freq / total_count

  # Reorder columns
  profile_freq <- profile_freq[, c("Profile", "Freq", "prop")]

  # Calculate means for each profile
  raw_means <- aggregate(df, by = list(Profile = profiles), FUN = mean)
  scaled_means <- aggregate(df_scaled, by = list(Profile = profiles), FUN = mean)

  # Adjust Profile column to have "profile_x" format
  raw_means$Profile <- paste0("profile_", raw_means$Profile)
  scaled_means$Profile <- paste0("profile_", scaled_means$Profile)

  # Merge frequencies with means
  raw_means <- merge(profile_freq, raw_means, by.x = "Profile", by.y = "Profile")
  scaled_means <- merge(profile_freq, scaled_means, by.x = "Profile", by.y = "Profile")

  # Prepare data for plotting
  raw_means_long <- melt(raw_means, id.vars = c("Profile", "Freq", "prop"))
  scaled_means_long <- melt(scaled_means, id.vars = c("Profile", "Freq", "prop"))

  # Modify strip labels if name_strip is provided
  if (!is.null(name_strip) && length(name_strip) == profile) {
    label_map <- setNames(name_strip, unique(raw_means_long$Profile))
    raw_means_long$Profile <- paste(raw_means_long$Profile, ":", label_map[raw_means_long$Profile])
    scaled_means_long$Profile <- paste(scaled_means_long$Profile, ":", label_map[scaled_means_long$Profile])
  }

  # Create raw score plot
  raw_plot <- ggplot(raw_means_long, aes(x = variable, y = value, fill = variable, group = 1)) +
    geom_bar(stat = "identity") +
    geom_point(size = 2, color = "black") +
    geom_line(color = "black") +
    facet_wrap(~Profile, ncol = 2) +
    theme_bw() +
    labs(title = "Raw score of the average of the LPA",
         x = "", y = "Raw Score") +
    theme(
      strip.text = element_text(size = size_strip),
      axis.text.x = element_text(angle = 90, hjust = 1, size = size_axis, face = "bold"),
      legend.position = "top",
      legend.title = element_blank()
    )

  # Create standardized score plot
  scaled_plot <- ggplot(scaled_means_long, aes(x = variable, y = value, fill = variable, group = 1)) +
    geom_bar(stat = "identity") +
    geom_point(size = 2, color = "black") +
    geom_line(color = "black") +
    facet_wrap(~Profile, ncol = 2) +
    theme_bw() +
    labs(title = "Standardized score of the average of LPA",
         x = "", y = "Z-Score") +
    theme(
      strip.text = element_text(size = size_strip),
      axis.text.x = element_text(angle = 90, hjust = 1, size = size_axis, face = "bold"),
      legend.position = "top",
      legend.title = element_blank()
    )

  # Combine raw and scaled plots with labels
  combined_plot <- plot_grid(
    raw_plot + labs(subtitle = "(a) Raw Data"),
    scaled_plot + labs(subtitle = "(b) Standardization Data"),
    labels = NULL, ncol = 2, align = "h"
  )

  # Combine raw and scaled data
  types <- c(rep("raw", nrow(raw_means)), rep("stand", nrow(scaled_means)))
  combined_data0 <- bind_rows(raw_means, scaled_means)
  combined_data <- bind_cols(data = types, combined_data0)

  # Create combined data output
  if (md) {
    combined_data_md <- md(combined_data %>% dplyr::select(-prop))
  } else {
    combined_data_md <- tibble(combined_data %>% dplyr::select(-prop))
  }

  # Return results as a list
  return(list(
    summary_table = raw_means,
    scaled_summary_table = scaled_means,
    profile_frequencies = tibble(profile_freq),
    combined_data = combined_data_md,
    mclust_object = summary(mclust_result),
    raw_plot = raw_plot,
    scaled_plot = scaled_plot,
    combined_plot = combined_plot
  ))
}
