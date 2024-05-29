#' Organizing and displaying confidence intervals
#'
#' @param data data
#' @param col1 lower col
#' @param col2 upper col
#' @param left left Parentheses
#' @param right right Parentheses
#' @param digits round
#' @param colname "95%CI"
#' @param remove True to remove existing columns, False to keep existing columns
#'
#' @return data
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' #'
#' # Sample data to simulate the scenario
#' data <- data.frame(
#'   paths = c('자기효능감-C_S1', '자기효능감-C_S2', '자기효능감-C_S3'),
#'   Original = c(0.883, 0.826, 0.794),
#'   Mean.Boot = c(0.885, 0.833, 0.8),
#'   Std.Error = c(0.022, 0.041, 0.039),
#'   t = c('40.771***', '20.22***', '20.612***'),
#'   perc.025 = c(0.837, 0.733, 0.725),
#'   perc.975 = c(0.923, 0.893, 0.872)
#' )
#'
#' # Apply the function to the dataframe with remove = TRUE
#' unite_ci(data, col1 = 6, col2 = 7, remove = TRUE)
#'
#' # Apply the function to the dataframe with remove = FALSE
#' unite_ci(data, col1 = 'perc.025', col2 = 'perc.975', remove = FALSE)

#'
#' }
#'
unite_ci <- function(data,
                     col1 = ncol(data) - 1,
                     col2 = ncol(data),
                     left="[",
                     right="]",
                     digits=3,
                     colname="95%CI",
                     remove=TRUE) {
  # Convert column indices to names if necessary
  if (is.numeric(col1)) col1 <- names(data)[col1]
  if (is.numeric(col2)) col2 <- names(data)[col2]

  # Ensure the columns exist
  if (col1 %in% names(data) && col2 %in% names(data)) {
    # Combine the columns using format for specified digits
    data[[colname]] <- paste0(left, format(round(data[[col1]], digits), nsmall = digits),
                              ", ", format(round(data[[col2]], digits), nsmall = digits), right)
    # Drop the original columns if remove is TRUE
    if (remove) {
      data <- data[, !(names(data) %in% c(col1, col2))]
    }
  } else {
    stop("Specified columns do not exist in the dataframe")
  }
  return(data)
}
