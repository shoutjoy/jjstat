#' plspm_effectbar: Bar graph with direct, indirect, and total effects
#'
#' @description This function creates a bar graph visualizing direct, indirect, and total effects
#' using plspm result data. Users can input plspm results directly or provide custom data.
#'
#' @param x_pls A plspm result object or a custom data frame with columns: "relationships", "direct", "indirect".
#' @param axis_x Size of x-axis text. Default is 1.3.
#' @param axis_y Size of y-axis text. Default is 1.3.
#' @param angle Angle of x-axis text. Default is 90 degrees.
#' @param hjust Horizontal alignment of x-axis text. Default is 1.
#' @param axis_x_pos Adjust x-axis position when axis labels are too large. Default is 1.
#' @param size_text Size of text displayed on the bars. Default is 4.
#' @param legend.position Position of the legend. Default is "top". Options include "top", "bottom", "left", "right", or "none".
#' @param input Boolean indicating whether the user provides a custom dataset. Default is FALSE.
#' @param user_data A data frame containing relationships, direct, and indirect columns if `input = TRUE`.
#' @param ylim A numeric vector of length 2 specifying the y-axis limits. Default is c(0, 1).
#' @param col A character vector specifying bar colors for "direct" and "indirect" effects. Default is c("black", "gray").
#'
#' @return Returns the processed data frame with rounded values. It also prints a ggplot2 object.
#' @export
#'
#' @examples
#' # Basic usage with plspm results
#' edt_pls %>% plspm_effectbar()
#'
#' # Directly specifying data with variable renaming
#' edt_pls$effects %>% select(1:3) %>%
#'     replace_df_rep(var_named) %>%
#'     plspm_effectbar(axis_x = 1.6, legend.position = "bottom")
plspm_effectbar <- function(x_pls,
                            axis_x = 1.3,
                            axis_y = 1.3,
                            angle = 90,          # x축 텍스트 각도
                            hjust = 1,           # x축 텍스트 정렬
                            axis_x_pos = 1,      # x축 텍스트 위치 조절
                            size_text = 4,       # geom_text 텍스트 크기
                            legend.position = "top", # 범례 위치 옵션 추가
                            input = FALSE,       # 사용자 입력 여부
                            user_data = NULL,    # 사용자가 입력한 데이터
                            ylim = c(0, 1),
                            col = c("black", "gray")) { # 막대 색상
  # Load necessary libraries
  library(dplyr)
  library(ggplot2)
  library(tidyr)

  # Check if user input is provided
  if (input && !is.null(user_data)) {
    if (!is.data.frame(user_data)) {
      stop("user_data must be a data frame.")
    }
    plsdf <- user_data
  } else {
    # Convert input to data frame if necessary
    if (is.matrix(x_pls)) {
      x_pls <- as.data.frame(x_pls)
    }

    if (length(x_pls) == 13 | length(x_pls) == 11) {
      plsdf <- x_pls$effects
    } else {
      plsdf <- x_pls
    }
  }

  # Ensure the data has proper column names
  if (is.null(colnames(plsdf))) {
    colnames(plsdf) <- c("relationships", "direct", "indirect")
  }

  # Transform data into long format
  path_effs_long <- plsdf %>%
    pivot_longer(cols = c("direct", "indirect"),
                 names_to = "effect_type",
                 values_to = "effect_value")

  # Preserve the original order of relationships
  path_effs_long$relationships <- factor(
    path_effs_long$relationships,
    levels = unique(plsdf$relationships)
  )

  # Add label text for D:, I:, and T:
  label_df <- plsdf %>%
    mutate(total = direct + indirect, # Calculate total effect
           label = paste0(
             "D: ", round(direct, 3), "\n",
             "I: ", round(indirect, 3), "\n",
             "T: ", round(total, 3) # Include total value
           ))

  # Create ggplot bar graph
  p <- ggplot(data = path_effs_long,
              aes(x = relationships,
                  y = effect_value,
                  fill = effect_type)) +
    geom_bar(stat = "identity", position = "stack", color = "black") +
    scale_fill_manual(values = col, labels = c("Direct", "Indirect")) +
    geom_text(data = label_df,
              aes(x = relationships, y = total, label = label),
              inherit.aes = FALSE,
              size = size_text, # Use size_text option
              fontface = "bold", vjust = -0.5) + # Position above the bar
    labs(x = NULL, y = "Effect Value", fill = "Effect Type") +
    theme_bw() +
    theme(axis.text.x = element_text(size = axis_x * 10,
                                     angle = angle, hjust = hjust, face = "bold"),
          axis.text.y = element_text(size = axis_y * 10),
          axis.title = element_text(size = axis_y * 12),
          legend.title = element_text(size = axis_y * 10),
          legend.text = element_text(size = axis_x * 10),
          legend.position = legend.position, # Use legend.position option
          plot.margin = unit(c(1, 1, axis_x_pos, 1), "lines")) + # axis_x_pos 옵션 추가
    coord_cartesian(ylim = ylim)

  print(p)

  # Apply rounding only to numeric columns
  return(plsdf %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
}
