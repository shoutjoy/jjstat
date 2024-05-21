#' NA replace to value default 0
#'
#' @param data data.frame or matrix
#' @param imp imputation value default 0
#' @param range range is column
#'
#' @return dat
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' data %>% na2zero()
#' data %>% na2zero(0)
#' data %>% na2zero(1)
#'
#' #'
#'
#' # Example data frame entry
#' df <- data.frame(a = c(1, NA, 3), b = c(NA, 2, 3))
#' na2zero(df)
#'
#' # Vector input examples
#' vec <- c(1, NA, 3)
#' print(na2zero(vec))
#'
#' # Character Vector Input Example
#' char_vec <- c("a", NA, "b")
#' print(na2zero(char_vec))
#' #'
#'
#' }
#'
# na2zero= function(data, imp=0){
#   data[is.na(data)] <- imp
#   as.data.frame(data)
#   # data
# }
na2zero <- function(data, imp = 0, range = NULL) {
  # Set the value to replace with a string
  imp <- as.character(imp)

  if (is.vector(data)) {
    if (is.character(data) || is.factor(data)) {
      data[data == "<NA>"] <- NA
    }
    data <- ifelse(is.na(data), imp, data)
    return(data)
  } else if (is.data.frame(data)) {
    if (is.null(range)) {
      range <- 1:ncol(data)
    }

    data[, range] <- lapply(data[, range], function(col) {
      col[col == "<NA>"] <- NA
      if (is.character(col) || is.factor(col)) {
        col[is.na(col)] <- imp
      } else {
        col[is.na(col)] <- as.numeric(imp)
      }
      return(col)
    })
    return(data)
  } else {
    stop("Unsupported data type")
  }
}

