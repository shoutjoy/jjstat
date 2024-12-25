#' EAI_index: Compute and visualize the Eduteck Acceptance Index (EAI) for given profiles
#'
#' @param df Data frame containing the necessary columns for EAI calculation.
#' @param profile A specific profile (e.g., a category from a column) to filter the data. Default is NULL, meaning all data will be used.
#' @param caption Optional caption for the plot title. Default is an empty string.
#' @param sel_col The column name in the data frame used for selecting profiles. Default is "Class".
#' @param level_start Starting level for categorization in the plot. Default is 9.
#' @param jump Increment to calculate subsequent levels. Default is 5.
#' @param adj Adjustment factor for level calculations. Default is 0.
#' @param size_label Font size for level annotations. Default is 4.
#' @param size_text Font size for score labels in the plot. Default is 5.
#' @param color_line Color for the dashed lines in the plot. Default is "gray20".
#' @param color_label Color for the level annotations. Default is "gray20".
#' @param lpos Horizontal position for left-side annotations. Default is 2.35.
#' @param rpos Horizontal position for right-side annotations. Default is 2.65.
#' @param print Logical indicating whether to print the summary table. Default is TRUE.
#' @param web Logical indicating whether to print web the summary table. Default is FALSE.
#'
#' @return A ggplot object visualizing the EAI index along with calculated levels and annotations.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Class = rep(c("A", "B"), each = 10),
#'   EAI = rnorm(20, 15, 5),
#'   PU_sum = rnorm(20, 20, 4),
#'   PEOU_sum = rnorm(20, 18, 3),
#'   ATT_sum = rnorm(20, 17, 4),
#'   USE_sum = rnorm(20, 19, 5),
#'   EAI_check = rnorm(20, 16, 2)
#' )
#' g <- EAI_index(df, profile = "A", caption = "EAI Analysis", sel_col = "Class")
#' print(g)
#' }
EAI_index <- function(df, profile = NULL, caption = "", sel_col = "Class",   level_start = 9, jump = 5, adj = 0,
                      size_label = 5, size_text = 5, color_line = "gray20", color_label = "gray20",
                      lpos = 2.35, rpos = 2.65, print=TRUE, web=FALSE) {
  # Step 1: Filter dataframe based on profile
  if (!is.null(profile)) {
    # Ensure sel_col exists in the dataframe
    if (!sel_col %in% colnames(df)) {
      stop(paste("The column", sel_col, "does not exist in the provided data frame. Please check the column names using colnames(df)."))
    }

    if (is.factor(df[[sel_col]])) {
      df[[sel_col]] <- as.factor(df[[sel_col]])
    } else if (is.character(df[[sel_col]])) {
      df[[sel_col]] <- as.character(df[[sel_col]])
    }

    # Subset the dataframe
    subset_df <- df[df[[sel_col]] == profile, ]
    if (nrow(subset_df) == 0) {
      stop("No data found for the given profile.")
    }
  } else {
    # Use the input dataframe directly
    subset_df <- df
  }

  # Step 2: Calculate levels and mean
  level <- c(level_start, level_start + jump, level_start + jump + jump + 1)
  left_level <- c(level[1], level[2] + adj / jump, level[3] + adj)

  add_minus <- level_start - 9
  right_level <- c(level[1] - 4 - add_minus / jump,
                   level[2] - 6 - add_minus / jump + adj / jump,
                   level[3] - 8 - add_minus / jump + adj)

  eai_mean <- round(mean(subset_df$EAI, na.rm = TRUE), 2)

  # Step 3: Prepare the table
  selected_columns <- c(sel_col, "PU_sum", "PEOU_sum", "ATT_sum", "USE_sum", "EAI", "EAI_check")
  selected_columns <- selected_columns[selected_columns %in% colnames(subset_df)]  # Ensure columns exist
  table <- subset_df[selected_columns]

  colnames(table) <- c("profile", "f1_유용성", "f2_편리성", "f3_태도", "f4_지속사용", "EAI", "level")

  summary_table_output <- mysummary(table, msdn = TRUE) %>% remove_rows(1)
  summary_table <- mysummary(table, msdn = TRUE) %>% remove_rows(1) %>% remove_rows(5)

  # Step 4: Create the plot

  library(ggplot2)

  g <- ggplot(summary_table, aes(x = var, y = MEAN, fill = var)) +
    geom_col() +
    geom_text(aes(label = paste0("score=", round(MEAN, 2))), vjust = -0.5, size=size_text) +
    labs(
      x = "EAI_index list",
      y = "EAI score (Mean)",
      title = paste0("EAI (Eduteck Acceptance Index): Profile ", profile,
                     "\nMean EAI: ", eai_mean, "\n", caption)
    ) +
    ylim(0, 25) +

    geom_segment(aes(x = 0.5, xend = 2.5, y = left_level[1], yend = left_level[1]),
                 color = color_line, size = 1, linetype = "dashed") +
    geom_segment(aes(x = 0.5, xend = 2.5, y = left_level[2], yend = left_level[2]),
                 color = color_line, size = 1, linetype = "dashed") +
    geom_segment(aes(x = 0.5, xend = 2.5, y = left_level[3], yend = left_level[3]),
                 color = color_line, size = 1, linetype = "dashed") +

    annotate("text", x = lpos, y = left_level[1] - 1.5,
             label = "Resistant", color = color_label, size = size_label, vjust = -0.5) +
    annotate("text", x = lpos, y = left_level[1] + 0.5,
             label = "Passive", color = color_label, size = size_label, vjust = -0.5) +
    annotate("text", x = lpos, y = left_level[2] + 0.5,
             label = "Active", color = color_label, size = size_label, vjust = -0.5) +
    annotate("text", x = lpos, y = left_level[3] + 0.5,
             label = "Adopter", color = color_label, size = size_label, vjust = -0.5) +


    geom_segment(aes(x = 4.5, xend = 2.5, y = right_level[1], yend = right_level[1]),
                 color = color_line, size = 1, linetype = "dashed") +
    geom_segment(aes(x = 4.5, xend = 2.5, y = right_level[2], yend = right_level[2]),
                 color = color_line, size = 1, linetype = "dashed") +

    geom_segment(aes(x = 4.5, xend = 2.5, y = right_level[3], yend = right_level[3]),
                 color = color_line, size = 1, , linetype = "dashed") +
    geom_vline(xintercept = 2.5, linetype = "dashed") +
    annotate("text", x = rpos, y = right_level[1] - 1.5,
             label = "Resistant", color = color_label, size = size_label, vjust = -0.5) +
    annotate("text", x = rpos, y = right_level[1] + 0.5,
             label = "Passive", color = color_label, size = size_label, vjust = -0.5) +
    annotate("text", x = rpos, y = right_level[2] + 0.5,
             label = "Active", color = color_label, size = size_label, vjust = -0.5) +
    annotate("text", x = rpos, y = right_level[3] + 0.5,
             label = "Adopter", color = color_label, size = size_label, vjust = -0.5) +
    annotate("text", x = 4.2, y = 25, label = paste("EAI index:", eai_mean), size = 6) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 14, face = "bold"),
      legend.position = "top",
      legend.box = "horizontal"
    )

  # Step 7: Output results
  if(print){
    print(summary_table_output)
  }
  if(web){
    md= summary_table_output %>% web()
  }
  #  print(mysummary(table, msdn = TRUE) %>% remove_rows(1) %>% mysummary_apa())
  # return(list(g, md))
  x11()
  return(g)
}
