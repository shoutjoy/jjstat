
#' Plot Confidence Intervals from a Summary Data Frame
#'
#' Draws horizontal error bars using data with estimate and confidence interval columns.
#'
#' @param data A data.frame or tibble containing path analysis results with columns: `Path`, `est`, `ci.lower`, `ci.upper`. Optionally `sig`.
#' @param var_named Optional named vector to rename `Path` labels.
#' @param size_axis Font size for axis text. Default is 14.
#' @param size_text Font size for estimate labels. Default is 4.
#' @param linewidth Line width of confidence interval bars. Default is 2.
#'
#' @return A ggplot object showing the confidence intervals.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- sem_effect_ie(fit_le)
#' confint_plot_data(df)
#'
#' # With renaming
#' named_vec <- c("IE.경험_효능감_진로의지" = "경험→효능감→진로의지")
#' confint_plot_data(df, var_named = named_vec)
#' }
confint_plot_data <- function(data,
                              var_named = NULL,
                              size_axis = 14,
                              size_text = 4,
                              linewidth = 2) {

  # 필수 컬럼 확인
  required_cols <- c("Path", "est", "ci.lower", "ci.upper")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # 선택적 변수명 치환
  if (!is.null(var_named)) {
    data <- data %>% replace_df_rep(var_named)
  }

  # 추정값 + 유의성 병합
  if ("sig" %in% names(data)) {
    data <- data %>%
      mutate(Est = paste0(round(est, 3), sig))
  } else {
    data <- data %>%
      mutate(Est = round(est, 3))
  }

  # 그래프 생성
  g <- ggplot(data, aes(x = est, y = reorder(Path, est),
                        xmin = ci.lower, xmax = ci.upper)) +
    geom_errorbarh(height = 0, color = "steelblue", linewidth = linewidth, alpha = 0.8) +
    geom_text(aes(label = Est), vjust = -0.5, size = size_text) +
    geom_vline(xintercept = 0, lty = 2, color = "red") +
    labs(x = "estimate", y = "path") +
    theme_bw() +
    theme(axis.text = element_text(size = size_axis, face = "bold"))

  return(g)
}

