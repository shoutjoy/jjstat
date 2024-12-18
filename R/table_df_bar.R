#' Generate a Bar Plot with Automatic Column Renaming
#'
#' @param df A data frame with at least three columns: the first column for terms, the second for frequencies, and the third for proportions.
#' @param size.x Numeric. Font size for axis text. Default is 20.
#' @param size.text Numeric. Font size for text labels on bars. Default is 6.
#' @param add_range Numeric. Additional range added to the maximum frequency for y-axis limits. Default is 100.
#' @param x.title Character. Title for the x-axis. Default is "Items".
#' @param flip Logical. Whether to flip the plot to horizontal orientation. Default is TRUE.
#' @param hjust Numeric. Horizontal adjustment for text labels on bars. Default is -0.1.
#' @param vjust Numeric. Vertical adjustment for text labels on bars. Default is -0.5.
#' @param x11_width Numeric. Width of the x11 window. Default is 8.
#' @param x11_height Numeric. Height of the x11 window. Default is 5.
#' @param linetype Character. Line type for the optional line. Default is "dashed".
#' @param linecolor Character. Color of the optional line. Default is "gray50".
#' @param show_line Logical. Whether to display a line connecting the bars. Default is FALSE.
#' @param levels Character vector. Custom order for categories on the x-axis. Default is c("전혀 아니다", "아니다", "중간이다", "그렇다", "매우 그렇다").
#' @param sort Logical. Whether to sort the bars by frequency in descending order. Default is FALSE.
#'
#' @return A bar plot displayed in an x11 window.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(term = c("1", "2", "3", "4"), Freq = c(250, 66, 215, 364),
#'                    `prop(%)` = c(27.9, 7.37, 24.0, 40.7))
#' table_df_bar(data)
#' }
table_df_bar <- function(df, size.x = 20, size.text = 6, add_range = 100,
                         x.title = "Items",
                         flip = TRUE,
                         hjust = -0.1, vjust = -0.5,
                         x11_width = 8, x11_height = 5,
                         linetype = "dashed", linecolor = "gray50",
                         show_line = FALSE,
                         levels = c("전혀 아니다", "아니다", "중간이다", "그렇다", "매우 그렇다"),
                         sort = FALSE) {
  # Rename columns to standard names
  colnames(df)[1] <- "term"
  colnames(df)[2] <- "Freq"
  colnames(df)[3] <- "prop(%)"

  # Sort the data frame if requested
  if (sort) {
    df <- df[order(df$Freq, decreasing = TRUE), ]
  }

  # Adjust y-axis limit
  max_value <- max(df$Freq, na.rm = TRUE) + add_range

  if (flip) {
    # Create a horizontal bar plot
    if (sort) {
      p0 <- ggplot(df, aes(x = reorder(term, Freq), y = Freq, fill = term))
    } else {
      p0 <- ggplot(df, aes(x = term, y = Freq, fill = term))
    }
    p <- p0 +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Freq, " (", round(`prop(%)`, 2), "%)")),
                hjust = hjust, size = size.text) +
      labs(x = x.title, y = "빈도(Freq)") +
      ylim(0, max_value) +
      theme_bw() +
      theme(
        axis.text = element_text(size = size.x, face = "bold"),
        axis.title = element_text(size = size.x, face = "bold"),
        legend.position = "none"
      ) +
      coord_flip()
  } else {
    # Set factor levels for x-axis
    df$term <- factor(df$term, levels = levels)

    # Create a vertical bar plot
    p <- ggplot(df, aes(x = term, y = Freq, group = 1, fill = term)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Freq, "\n (", round(`prop(%)`, 2), "%)")),
                vjust = vjust, size = size.text)

    if (show_line) {
      p <- p + geom_line(aes(x = term, y = Freq), show.legend = FALSE,
                         linetype = linetype, color = linecolor, linewidth = 0.8) +
        geom_point(aes(x = term, y = Freq), show.legend = FALSE)
    }

    p <- p + labs(x = x.title, y = "빈도(Freq)") +
      ylim(0, max_value) +
      theme_bw() +
      theme(
        axis.text = element_text(size = size.x, face = "bold"),
        axis.title = element_text(size = size.x, face = "bold"),
        legend.position = "none"
      )
  }

  x11(width = x11_width, height = x11_height)
  print(p)
}
