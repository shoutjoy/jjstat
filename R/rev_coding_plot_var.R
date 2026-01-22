#' Plot reverse-coded variable against original values
#'
#' Displays a comparison between the original and reverse-coded values.
#' Works only when original variable is still available in parent environment or in the given data.
#'
#' @param x A reverse-coded vector with class 'reverse_var'
#' @param data Optional: original data.frame containing the original variable
#' @param ... Additional graphical arguments passed to plot
#'
#' @return A scatter plot of original vs. reverse-coded values
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(A = 1:5)
#' res <- rev_coding_rep(df, 6, A)
#' rev_coding_plot_var(res$A_r, data = df)
#' }
rev_coding_plot_var <- function(x, data = NULL, ...) {
  original_var <- attr(x, "original_var")
  max_scale <- attr(x, "max_scale")

  if (is.null(original_var)) stop("Original variable name not found in attributes.")

  if (is.null(data)) {
    # Try to find original in calling environment
    data_env <- parent.frame()
    if (exists(original_var, envir = data_env)) {
      original <- get(original_var, envir = data_env)
    } else {
      stop("Original variable not found. Pass 'data' explicitly.")
    }
  } else {
    if (!original_var %in% names(data)) stop("Original variable not found in 'data'.")
    original <- data[[original_var]]
  }

  # Plot
  plot(original, x,
       xlab = paste0("Original: ", original_var),
       ylab = paste0("Reverse-coded (", max_scale, " - x)"),
       main = "Reverse Coding Comparison",
       pch = 19, col = "blue", ...
  )
  abline(a = max_scale, b = -1, col = "red", lwd = 2, lty = 2)
  legend("topright", legend = paste("y = ", max_scale, " - x"), col = "red", lty = 2, bty = "n")
}
