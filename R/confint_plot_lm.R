
#' Plot Confidence Intervals for Linear Model or Data Frame
#'
#' @param lm_data An object of class 'lm' (linear model) or a data.frame containing results with estimates and confidence intervals.
#' @param input Character, one of 'lm' or 'data.frame'. Determines how lm_data is handled.
#' @param size_text Numeric, size of text in the plot (default = 12).
#' @param size_title Numeric, size of axis titles in the plot (default = 14).
#' @param color Character, color of error bars (default = "steelblue").
#' @param linewidth Numeric, thickness of error bars (default = 1).
#' @param alpha Numeric, transparency of error bars (default = 1).
#' @param intercept Logical, whether to include the intercept in the plot (default = FALSE).
#' @param errorbar_height Numeric, height of error bar ends (default = 0.3).
#' @param x.title Character, label for the x-axis (default = "Estimate").
#' @param y.title Character, label for the y-axis (default = "Item").
#' @param type Character, output type ('g' for plot, 'res' for results, 'df' for tidy dataframe).
#'
#' @return A ggplot object, results list, or tidy dataframe based on `type`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with lm object
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' confint_plot_lm(model, input = "lm", errorbar_height = 0.1, size_title = 16)
#'
#' # Example with data.frame
#' df <- data.frame(term = c("wt", "hp"), estimate = c(-5.344, -0.018),
#'                  conf.low = c(-7.344, -0.032), conf.high = c(-3.344, -0.004))
#' confint_plot_lm(df, input = "data.frame", errorbar_height = 0.3,
#'                 x.title = "Beta Coefficients", y.title = "Predictors")
#' }
confint_plot_lm <- function(lm_data,
                            input = c("lm", "data.frame"),
                            size_text = 12,
                            size_title = 14,
                            color = "steelblue",
                            linewidth = 1,
                            alpha = 1,
                            intercept = FALSE,
                            errorbar_height = 0.3,
                            x.title = "Estimate",
                            y.title = "Item",
                            type = "g") {
  # Validate input
  input <- match.arg(input)

  # Process lm object
  if (input == "lm") {
    lm_data0 <- broom::tidy(lm_data, conf.int = TRUE)
    if (intercept) {
      lm_data <- lm_data0
    } else {
      lm_data <- lm_data0 %>% dplyr::slice(-1) # Remove intercept
    }
  } else if (input == "data.frame") {
    # Use data.frame directly
    lm_data0 <- lm_data
    if (!intercept) {
      lm_data <- lm_data %>% dplyr::filter(term != "(Intercept)")
    }
  }

  # Generate plot
  g <- lm_data %>%
    ggplot2::ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
    ggplot2::geom_errorbarh(color = color, linewidth = linewidth, alpha = alpha, height = errorbar_height) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_vline(xintercept = 0, lty = 2, color = "red") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x.title, y = y.title) +
    ggplot2::theme(axis.text.y = element_text(size = size_text, face = "bold"),
                   axis.title = element_text(size = size_title, face = "bold"))

  # Return output based on type
  res <- list(data = lm_data0, plot = g)

  switch(type,
         res = res,
         g = g,
         df = lm_data0)
}
