#' mean_bar: Create a Bar Plot to Compare Means
#'
#' This function generates a bar plot comparing the mean values of a dependent variable,
#' with options to sort the bars based on the mean values.
#'
#' @param df A data frame containing at least two columns:
#' - `var`: A categorical variable representing the grouping.
#' - `MEAN`: A numeric variable representing the mean values.
#' @param sort Logical. If TRUE, sorts the bars by mean values. Default is FALSE.
#' @param x.title text default "문항"
#'
#' @return A ggplot2 object displaying the bar plot.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example data
#' data <- data.frame(var = c("A", "B", "C"),
#'                    MEAN = c(3.5, 2.8, 4.1))
#' # Plot without sorting
#' mean_bar(data)
#'
#' # Plot with sorting
#' mean_bar(data, sort = TRUE)
#' }
mean_bar <- function(df, x.title = "문항", sort = FALSE) {
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

  x11()
  # Generate the bar plot
  ggplot(df, aes(x = var, y = MEAN, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = paste0("m=", round(MEAN, 2))), vjust = -0.3) +
    labs(title = "Compare response averages by dependent variable question",
         x = x.title,
         y = "평균값") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"))
}
