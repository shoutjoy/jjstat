
#' calculation vif
#'
#' @param model lm model data
#'
#' @return vif
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Create the linear model
#' model <- lm(hp ~ wt + disp + drat, data = mtcars)
#'
#' # Calculate VIF using the custom function
#' cal_vif(model)
#'
#' lm(mpg ~ wt + disp + drat + hp , data = mtcars) %>%cal_vif()
#'
#'
#' }
cal_vif <- function(model) {
  # Extract the design matrix (excluding the intercept)
  X <- model.matrix(model)[, -1]

  # Calculate VIF for each predictor
  vif_values <- sapply(1:ncol(X), function(i) {
    # Fit a model with the ith predictor as the response
    vif_model <- lm(X[, i] ~ X[, -i])
    # Calculate the R-squared value
    r_squared <- summary(vif_model)$r.squared
    # Calculate VIF
    vif <- 1 / (1 - r_squared)
    return(vif)
  })

  # Set names of the VIF values
  names(vif_values) <- colnames(X)
  return(vif_values)
}
