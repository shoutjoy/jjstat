#' Add Normality Kline
#'
#' This function evaluates normality for each variable in the dataset based on skewness and kurtosis.
#'
#' @param df A data frame containing statistical summary including Skew and Kurt columns.
#' @param skew Threshold for skewness. Default is 3.0.
#' @param kurt Threshold for kurtosis. Default is 10.0.
#'
#' @return A data frame with added columns `skew_sig` and `kurt_sig` indicating the normality evaluation.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   var = c("SUX", "FLOW", "PU", "PEOU", "ATT", "USE"),
#'   N = c(895, 895, 895, 895, 895, 895),
#'   MEAN = runif(6, 2, 3),
#'   SD = runif(6, 0.9, 1.1),
#'   MIN = rep(1, 6),
#'   MAX = rep(5, 6),
#'   Skew = c(0.74, 0.23, 0.81, 0.75, 0.69, 0.53),
#'   Kurt = c(0.16, -0.31, 0.37, 0.06, 0.02, -0.42)
#' )
#' add_normality_kline(data)
#' }
add_normality_kline <- function(df, skew = 3, kurt = 10) {
  # Evaluate skewness
  df$skew_sig <- ifelse(abs(df$Skew) <= skew, "Good(|s|<3)", "NO")

  # Evaluate kurtosis with detailed thresholds
  df$kurt_sig <- ifelse(abs(df$Kurt) <= 7, "Good(|k|<7)",
                        ifelse(abs(df$Kurt) <= kurt, "Fair", "NO"))

  # Return updated data frame
  return(df)
}
