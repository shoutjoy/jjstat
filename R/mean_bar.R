#' mean_bar: Bar plot of means with optional sorting and customizations
#'
#' This function generates a bar plot displaying mean values, allowing customization of axis labels, text size, angle, and sorting.
#'
#' @param df A data frame containing 'var' and 'MEAN' columns.
#' @param x.title A character string for the x-axis title. Default is "문항".
#' @param sort Logical. If TRUE, sorts the data by mean values in ascending order. Default is FALSE.
#' @param size.text Numeric. Size of the text labels on bars. Default is 4.
#' @param text.angle Numeric. Angle of the x-axis text labels. Default is 0.
#' @param size.axis Numeric. Size of the axis text labels. Default is 12.
#'
#' @return A ggplot object showing a bar plot of means.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' df <- data.frame(var = c("Q1", "Q2", "Q3"), MEAN = c(3.5, 4.2, 2.8))
#'
#' # Default bar plot
#' mean_bar(df)
#'
#' # Sorted bar plot with customized text size and angle
#' mean_bar(df, sort = TRUE, size.text = 5, text.angle = 45)
#' }
mean_bar <- function(df, x.title = "문항", sort = FALSE, size.text = 4, text.angle = 0, size.axis = 12) {
  # Load necessary libraries
  library(ggplot2)
  library(dplyr)

  # Check for required columns
  if (!("var" %in% names(df)) || !("MEAN" %in% names(df))) {
    stop("Input data frame must contain 'var' and 'MEAN' columns.")
  }

  # Optionally sort the data
  if (sort) {
    df <- df %>% arrange(MEAN)
  }

  # Convert 'var' to a factor to maintain the order
  df$var <- factor(df$var, levels = df$var)

  x11() # Open a new graphics device (optional, can be removed for non-interactive sessions)

  # Generate the bar plot
  ggplot(df, aes(x = var, y = MEAN, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = paste0("m=", round(MEAN, 2))), size = size.text, vjust = -0.3) +
    labs(title = "Compare response averages by dependent variable question",
         x = x.title,
         y = "평균값") +
    theme_bw() +
    theme(axis.text = element_text(size = size.axis, angle = text.angle, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"))
}
