#' table_df_bar2, Bar and Line Plot for Frequency Data
#'
#' This function creates a bar and line plot from a data frame containing terms, frequencies, and proportions.
#'
#' @param df Data frame with three columns: term (character), frequency (Freq), and percentage (prop(%)).
#' @param size.x Numeric. Font size for x-axis labels. Default is 16.
#' @param size.text Numeric. Font size for text labels inside the plot. Default is 4.
#' @param add_range Numeric. Additional range added to the y-axis limit. Default is 100.
#' @param x.title Character. Title for the x-axis. Default is "Items".
#' @param hjust Numeric. Horizontal adjustment for text labels. Default is -0.1.
#' @param vjust Numeric. Vertical adjustment for text labels. Default is -0.5.
#' @param x11_width Numeric. Width of the X11 window for plotting. Default is 8.
#' @param x11_height Numeric. Height of the X11 window for plotting. Default is 5.
#' @param linetype Character. Type of line connecting the points. Default is "dashed".
#' @param linecolor Character. Color of the connecting line. Default is "gray50".
#' @param show_line Logical. Whether to add a line connecting the points. Default is TRUE.
#' @param remove_bar Logical. Whether to remove the bar plot. Default is FALSE.
#' @param sort Logical. Whether to sort the data by frequency in descending order. Default is FALSE.
#' @param text_angle Numeric. Angle for x-axis text labels. Default is 45.
#'
#' @return None. This function creates and displays a plot in a new X11 window.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(term = c("A", "B", "C"),
#'                  Freq = c(50, 30, 40),
#'                  `prop(%)` = c(50, 30, 40))
#' table_df_bar2(df, sort = TRUE)
#' }
#'
#' @export
table_df_bar2 <- function(df, size.x = 16, size.text = 4, add_range = 100,
                          x.title = "Items",
                          hjust = -0.1, vjust = -0.5,
                          x11_width = 8, x11_height = 5,
                          linetype = "dashed", linecolor = "gray50",
                          show_line = TRUE,
                          remove_bar = FALSE,
                          sort = FALSE,
                          text_angle = 45) {

  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Package 'ggrepel' is required.")

  # Remove NA values and rename columns
  df <- dplyr::filter(df, !is.na(df[[1]]))
  colnames(df)[1:3] <- c("term", "Freq", "prop(%)")

  # Sort if requested
  if (sort) {
    df <- df[order(df$Freq, decreasing = TRUE), ]
  }

  # Convert term to factor to preserve order
  df$term <- factor(df$term, levels = unique(df$term))

  # Define y-axis limit
  max_value <- max(df$Freq, na.rm = TRUE) + add_range

  # Initialize plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = term, y = Freq, fill = term))

  # Add bar layer
  if (!remove_bar) {
    p <- p + ggplot2::geom_bar(stat = "identity", alpha = 0.7)
  }

  # Add points and text
  p <- p +
    ggplot2::geom_point(size = 3, color = "black") +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = paste0(Freq, "\n(", round(`prop(%)`, 2), "%)")),
      size = size.text,
      direction = "y",
      nudge_y = 10
    ) +
    ggplot2::labs(x = x.title, y = "Frequency") +
    ggplot2::ylim(0, max_value) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = size.x, angle = text_angle, hjust = 1, face = "bold"),
      axis.title = ggplot2::element_text(size = size.x, face = "bold"),
      legend.position = "none"
    )

  # Add line if enabled
  if (show_line) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(group = 1),
      linetype = linetype,
      color = linecolor,
      linewidth = 0.8
    )
  }

  # Display in X11 window
  grDevices::x11(width = x11_width, height = x11_height)
  print(p)
}
