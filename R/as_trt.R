#' Treat selected variables as factors
#'
#' @param data data.frame
#' @param ...  variables
#' @param fun funtion default as.factor
#'
#' @return data treatment
#' @export
#'
#' @examples
#' \dontrun{
#'  str(mrcars)
#'
#'  mtcars1 = mtcars %>% as_trt("cyl", "am","vs", "gear", "carb")
#'  str(mtcars1)
#'
#'  #col number
#'  mtcars%>%as_trt(c(2,8:11))%>%str()
#' #'
#' # 데이터 프레임 생성
#' df <- data.frame(matrix(1:500, ncol=100))
#' colnames(df) <- paste0("P", sprintf("%02d", 1:100))
#'
#' # 열 번호 범위로 변환
#' as_trt(df, 81:91)%>%str()
#' as_trt(df, "P81:P91")%>%str()
#' as_trt(df, "P81","P91")%>%str()
#' as_trt(df, 81, 92)%>%str()
#' #'
#'
#'
#' }
#'
as_trt2 <- function(data, ..., fun = factor) {
  vars <- list(...)

  # Helper function to expand range-like expressions to column indices
  expand_vars <- function(var_list, data) {
    expanded_vars <- c()
    for (var in var_list) {
      if (is.character(var) && grepl(":", var)) {
        # Split by colon to get the start and end of the range
        parts <- strsplit(var, ":")[[1]]
        if (length(parts) == 2) {
          start <- parts[1]
          end <- parts[2]
          # Get the column indices within the specified range
          start_index <- which(names(data) == start)
          end_index <- which(names(data) == end)
          if (length(start_index) == 1 && length(end_index) == 1) {
            expanded_vars <- c(expanded_vars, start_index:end_index)
          } else {
            warning(paste("Invalid range specification:", var))
          }
        } else {
          warning(paste("Invalid range specification:", var))
        }
      } else if (is.numeric(var)) {
        if (length(var) > 1) {
          # Handle numeric ranges (e.g., 81:91)
          if (all(var > 0 & var <= ncol(data))) {
            expanded_vars <- c(expanded_vars, var)
          } else {
            warning("Some variable indices are out of bounds")
          }
        } else {
          if (var > 0 && var <= ncol(data)) {
            expanded_vars <- c(expanded_vars, var)
          } else {
            warning(paste("Variable index", var, "is out of bounds"))
          }
        }
      } else if (is.character(var)) {
        if (var %in% names(data)) {
          expanded_vars <- c(expanded_vars, which(names(data) == var))
        } else {
          warning(paste("Variable", var, "not found in the dataset"))
        }
      } else {
        warning(paste("Invalid variable identifier:", var))
      }
    }
    return(expanded_vars)
  }

  # Expand variables to their column indices
  vars <- expand_vars(vars, data)

  # Apply the function to the specified variables
  for (var in vars) {
    if (var > 0 && var <= ncol(data)) {
      data[[var]] <- fun(data[[var]])
    } else {
      warning(paste("Variable index", var, "is out of bounds"))
    }
  }

  return(data)
}



#' Treat selected variables as factors
#'
#' @param data data.frame
#' @param ...  variables
#' @param fun funtion default as.factor
#'
#' @return data treatment
#' @export
#'
#' @examples
#' \dontrun{
#'  str(mrcars)
#'
#'  mtcars1 = mtcars %>% sel_as_trt("cyl", "am","vs", "gear", "carb")
#'  str(mtcars1)
#'
#'  #col number
#'  mtcars%>%as_trt(c(2,8:11))%>%str()
#' #'
#' # 데이터 프레임 생성
#' df <- data.frame(matrix(1:500, ncol=100))
#' colnames(df) <- paste0("P", sprintf("%02d", 1:100))
#'
#' # 열 번호 범위로 변환
#' as_trt(df, 81:91)%>%str()
#' as_trt(df, "P81","P91")%>%str()
#' as_trt(df, 81, 92)%>%str()
#' #'
#'
#'
#' }
#'
as_trt <- function(data, ..., fun = factor) {
  vars <- c(...)
  for (var in vars) {
    # Check if var is numeric (i.e., a column index)
    if (is.numeric(var)) {
      if (var > 0 && var <= ncol(data)) {
        var_name <- names(data)[var]
        data[[var_name]] <- fun(data[[var_name]])
      } else {
        warning(paste("Variable index", var, "is out of bounds"))
      }
    } else if (is.character(var)) { # Check if var is a character (i.e., a column name)
      if (var %in% names(data)) {
        data[[var]] <- fun(data[[var]])
      } else {
        warning(paste("Variable", var, "not found in the dataset"))
      }
    } else {
      warning(paste("Invalid variable identifier:", var))
    }
  }
  return(data)
}
