#' Custom function to perform reverse coding for multiple variables with custom n values
#'
#' @param data Data frame containing the variables
#' @param n Vector of maximum scale values (one for each vadata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCCriable), c("v1","v2",...)
#' @param ... Variable names to be reverse coded
#' @param postfix postfix is reverse variable name default '_r'
#' @param replace How to enter and replace the reverse coding results
#' @return add reverse data in data.frame
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ### How to decode all variables by applying only one n
#' revcode(stat_onl, 8, "upgrade", "satisfy", "fatigue" )|>
#' select(upgrade, satisfy, fatigue, upgrade_r, satisfy_r, fatigue_r)
#'
#' ### the method for the reverse coding variable being applied separately
#' ### to each variable and decoding
#' revcode(stat_onl, n= c(8,8,8), "upgrade", "satisfy", "fatigue" )|>
#' select(upgrade, satisfy, fatigue, upgrade_r, satisfy_r, fatigue_r)
#'
#' ## mtcars data
#' ## Since the inverse coding value of the cyl variable is 4,6,8,
#' ## n enters 4 + 8 = 12; the minimum + maximum = n value.
#' revcode(mtcars[,c("cyl", "gear","carb")], 12, "cyl")
#'
#'
#' revcode(mtcars[,c("cyl", "gear","carb")],
#'             n = c(12, 6, 9),"cyl","gear","carb")
#' #'
#' df <- data.frame(
#'   A = c(1, 2, 3, 4, 5),
#'   B = c(5, 4, 3, 2, 1)
#' )
#'
#' # Passing variable names as symbols
#' df_new1 <- revcode(df, n = 6, A, B)
#' print(df_new1)
#' str(df_new1)
#' print(attr(df_new1$A_r, "reverse")) # Output properties for verification
#' print(attr(df_new1$B_r, "reverse"))
#'
#' # pass the variable name as a string
#' df_new2 <- revcode(df, n = 6, "A", "B")
#' print(df_new2)
#' str(df_new2)
#' print(attr(df_new2$A_r, "reverse"))  # Output properties for verification
#' print(attr(df_new2$B_r, "reverse"))
#'
#' # Reverse-code to replace original column
#' df_replaced <- revcode(df, n = 6, A, B, replace = TRUE)
#' print(df_replaced)
#' str(df_replaced)
#' print(attr(df_replaced$A, "reverse"))  # Output properties for verification
#' print(attr(df_replaced$B, "reverse"))
#'
#' #'
#'
#' }
revcode <- function(data, n = 8, ..., postfix = "_r", replace = FALSE) {
  # Arguments:
  #   data: Data frame containing the variables
  #   n: Vector of maximum scale values (one for each variable)
  #   ...: Variable names to be reverse coded (can be passed as symbols or strings)
  #   postfix: Postfix for the new reverse coded variables
  #   replace: Whether to replace the original variables with the reverse coded variables

  # Capture variable names
  var_names <- sapply(substitute(list(...))[-1], function(x) {
    if (is.character(x)) {
      as.name(x)
    } else {
      x
    }
  })

  if (length(n) == 1) {
    n <- rep(n, length(var_names))
  }

  for (i in seq_along(var_names)) {
    var <- var_names[i]
    n_val <- n[i]

    reversed_var <- n_val - data[[as.character(var)]]

    if (replace) {
      data[[as.character(var)]] <- reversed_var
      attr(data[[as.character(var)]], "reverse") <- TRUE
    } else {
      new_var_name <- paste0(as.character(var), postfix)
      data[[new_var_name]] <- reversed_var
      attr(data[[new_var_name]], "reverse") <- TRUE
    }
  }

  return(data)
}

# revcode <- function(data, n = 8, ..., postfix="_r") {
#   # Arguments:
#   #   data: Data frame containing the variables
#   #   n: Vector of maximum scale values (one for each variable)
#   #   ...: Variable names to be reverse coded
#   if(length(n)==1){
#     # Loop through each variable
#     for (var in c(...)) {
#       # Reverse code the variable
#       data[[paste0(var, postfix)]] <- n - data[[var]]
#     }
#   }else{
#     # Loop through each variable
#     for (i in seq_along(c(...))) {
#       var <- c(...)[i]
#       n_val <- n[i]
#
#       # Reverse code the variable
#       data[[paste0(var, postfix)]] <- n_val - data[[var]]
#     }
#
#   }
#   return(data)
# }


