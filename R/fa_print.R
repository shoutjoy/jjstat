#' fa_print: A custom function to format and display Factor Analysis (FA) results
#'
#' @param data A data frame containing the FA results to format.
#' @param cut Numeric. The threshold for suppressing small factor loadings. Defaults to 0.4.
#' @param sort Logical. If TRUE, sorts the factor loadings. Defaults to TRUE.
#' @param abs Logical. If TRUE, applies the absolute value to the factor loadings for threshold comparison. Defaults to TRUE.
#' @param desc Logical. If TRUE, sorts in descending order. Defaults to TRUE.
#' @param star Logical. If TRUE, marks factor loadings exceeding the cut-off with "*". Defaults to FALSE.
#' @param justify Character. Specifies justification ("left", "center", or "right") for formatted values. Defaults to "right".
#'
#' @return A data frame with formatted and optionally sorted FA results, keeping "Eigenvalues",
#'         "Proportion Var", and "Cumulative Var" unaffected by filtering and sorting.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage with a FA result
#' library(psych)
#' results <- psych::fa(mtcars, nfactors = 2, rotate = "promax")
#' summary <- summary_fa(results)
#' formatted_results <- fa_print(summary, cut = 0.3, star = TRUE)
#' print(formatted_results)
#' }
fa_print <- function(data, cut = 0.4,
                     sort = TRUE, abs = TRUE,
                     desc = TRUE, star = FALSE, justify = "right") {
  # Separate data_head and data_tail by filtering rows that meet a condition
  data_tail <- data[data$source %in% c("Eigenvalues", "Proportion Var", "Cumulative Var"), ]
  data_head <- data[!data$source %in% c("Eigenvalues", "Proportion Var", "Cumulative Var"), ]

  # Process data_head with specified conditions
  if (abs) {
    if (star) {
      for (i in 2:ncol(data_head)) {
        if (is.numeric(data_head[[i]])) {
          data_head[[i]] <- ifelse(abs(data_head[[i]]) < cut,
                                   format(paste0(
                                     format(data_head[[i]], nsmall = 3), " "),
                                     nsmall = 3, width = 8,
                                     justify = justify),
                                   format(paste0(
                                     format(data_head[[i]], nsmall = 3), "*"),
                                     nsmall = 3, width = 8,
                                     justify = justify))
        }
      }
    } else {
      for (i in 2:ncol(data_head)) {
        if (is.numeric(data_head[[i]])) {
          data_head[[i]] <- ifelse(abs(data_head[[i]]) < cut, "",
                                   format(data_head[[i]], nsmall = 3))
        }
      }
    }
  } else {
    if (star) {
      for (i in 2:ncol(data_head)) {
        if (is.numeric(data_head[[i]])) {
          data_head[[i]] <- ifelse(data_head[[i]] < cut,
                                   format(paste0(
                                     format(data_head[[i]], nsmall = 3), " "),
                                     nsmall = 3, width = 8,
                                     justify = justify),
                                   format(paste0(
                                     format(data_head[[i]], nsmall = 3), "*"),
                                     nsmall = 3, width = 8,
                                     justify = justify))
        }
      }
    } else {
      for (i in 2:ncol(data_head)) {
        if (is.numeric(data_head[[i]])) {
          data_head[[i]] <- ifelse(data_head[[i]] < cut, "",
                                   format(data_head[[i]], nsmall = 3))
        }
      }
    }
  }

  # Sort the data_head if sort is TRUE
  if (sort) {
    data_head <- data_head[do.call(order, c(as.list(data_head[, -1]),
                                            list(decreasing = desc))), ]
  }

  # Combine the two parts back together, ensuring data_tail retains its order
  res <- rbind(data_head, data_tail)

  return(data.frame(res))
}


#'
#' fa_print <- function(data, cut = 0.4, sort = TRUE, desc=TRUE) {
#'   # Separate data_head and data_tail by filtering rows that meet a condition
#   data_head <- data[!data$source %in% c("eigen_vlaue", "Proportion_Var", "Cumulative_Var"), ]
#   data_tail <- data[data$source %in% c("eigen_vlaue", "Proportion_Var", "Cumulative_Var"), ]
#
#   # Apply cut value for data_head to replace with ""
#   for (i in 2:ncol(data_head)) {
#     if (is.numeric(data_head[[i]])) {
#       data_head[[i]] <- ifelse(data_head[[i]] < cut, "", data_head[[i]])
#     } # remove abs()
#   }
#
#   # Sort by all columns if sort is TRUE
#   if (sort) {
#     # Sort only parts of data except column names
#     # data_head <- data_head[do.call(order, as.list(data_head[ , -1])), ]
#     data_head <- data_head[do.call(order, c(as.list(data_head[ , -1]),
#                                             list(decreasing = desc))), ]
#   }
#
#   # Combine the two parts back together
#   res <- rbind(data_head, data_tail)
#
#   return(data.frame(res))
# }
