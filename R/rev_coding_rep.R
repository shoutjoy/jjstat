#' Reverse Coding for Multiple Variables with S3 Methods
#'
#' Reverse-codes specified variables in a data frame by subtracting values
#' from a given maximum scale value. Supports symbol and character input
#' for variable names. Adds S3 classes for customized print and summary behavior.
#'
#' @param data A data.frame containing variables to be reverse coded.
#' @param n A numeric scalar or numeric vector specifying maximum scale values.
#'          If length 1, the value is recycled for all variables.
#' @param ... Variables to be reverse coded. Can be symbols or character strings.
#' @param postfix Character string appended to new variable names.
#'        Default is "_r".
#' @param replace Logical. If TRUE, replaces original variables.
#'
#' @return A data.frame of class 'revcoded_df'.
#'         Reverse-coded variables have class 'reverse_var'.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ############################################
#' # 0. 기본 데이터 생성
#' ############################################
#' df <- data.frame(
#'   A = c(1, 2, 3, 4, 5),
#'   B = c(5, 4, 3, 2, 1),
#'   C = c(2, 3, 3, 4, 5)
#' )
#'
#' ############################################
#' # 1. 기본 사용: 하나의 n 값으로 여러 변수 역코딩
#' ############################################
#' res1 <- rev_coding_rep(df, n = 6, A, B)
#' print(res1)
#' summary(res1$A_r)
#'
#' ############################################
#' # 2. 문자열로 변수 이름 전달
#' ############################################
#' res2 <- rev_coding_rep(df, n = 6, "A", "B")
#' print(res2)
#'
#' ############################################
#' # 3. replace = TRUE : 기존 변수 덮어쓰기
#' ############################################
#' res3 <- rev_coding_rep(df, n = 6, A, B, replace = TRUE)
#' print(res3)
#' summary(res3$A)
#'
#' ############################################
#' # 4. 변수별로 다른 n 값 적용
#' ############################################
#' res4 <- rev_coding_rep(df, n = c(6, 5, 7), A, B, C)
#' print(res4)
#'
#' ############################################
#' # 5. postfix 변경
#' ############################################
#' res5 <- rev_coding_rep(df, n = 6, A, B, postfix = "_inv")
#' names(res5)
#'
#' ############################################
#' # 6. NA 값이 포함된 데이터
#' ############################################
#' df_na <- data.frame(
#'   Q1 = c(1, NA, 3, 5),
#'   Q2 = c(5, 4, NA, 2)
#' )
#' res6 <- rev_coding_rep(df_na, n = 6, Q1, Q2)
#' print(res6)
#' summary(res6$Q1_r)
#'
#' ############################################
#' # 7. mtcars 예제 (범주형처럼 쓰이는 수치형 변수)
#' ############################################
#' # cyl = {4,6,8} → min + max = 12
#' res7 <- rev_coding_rep(
#'   mtcars[, c("cyl", "gear", "carb")],
#'   n = c(12, 6, 9),
#'   "cyl", "gear", "carb"
#' )
#' head(res7)
#'
#' ############################################
#' # 8. tidyverse 파이프라인에서 사용
#' ############################################
#' library(dplyr)
#' df %>%
#'   rev_coding_rep(n = 6, A, B) %>%
#'   select(A_r, B_r) %>%
#'   summary()
#'
#' ############################################
#' # 9. reverse-coded 변수만 자동 추출
#' ############################################
#' get_reverse_vars <- function(x) {
#'   names(x)[sapply(x, function(col) inherits(col, "reverse_var"))]
#' }
#' res9 <- rev_coding_rep(df, n = 6, A, B)
#' get_reverse_vars(res9)
#'
#' ############################################
#' # 10. 심리척도 예제 (Likert scale)
#' ############################################
#' likert <- data.frame(
#'   stress1 = c(1,2,3,4,5),
#'   stress2 = c(2,3,4,4,5),
#'   stress3 = c(5,4,3,2,1)
#' )
#' res10 <- rev_coding_rep(likert, n = 6, stress3)
#' print(res10)
#'
#' ############################################
#' # 11. 실수 방지 예제 (factor 주의)
#' ############################################
#' df_factor <- data.frame(Q1 = factor(c(1,2,3)))
#' # rev_coding_rep(df_factor, n = 6, Q1)  # 권장하지 않음
#' }
rev_coding_rep <- function(data, n = 8, ..., postfix = "_r", replace = FALSE) {

  # Capture variable names
  var_names <- sapply(substitute(list(...))[-1], function(x) {
    if (is.character(x)) as.name(x) else x
  })

  # Validate n
  if (length(n) == 1) {
    n <- rep(n, length(var_names))
  }

  if (length(n) != length(var_names)) {
    stop("Length of n must be 1 or equal to the number of variables.")
  }

  for (i in seq_along(var_names)) {

    var     <- var_names[i]
    var_str <- as.character(var)
    n_val   <- n[i]

    reversed_var <- n_val - data[[var_str]]

    # Add class and attributes
    class(reversed_var) <- c("reverse_var", class(reversed_var))
    attr(reversed_var, "reverse") <- TRUE
    attr(reversed_var, "original_var") <- var_str
    attr(reversed_var, "max_scale") <- n_val

    if (replace) {
      data[[var_str]] <- reversed_var
    } else {
      data[[paste0(var_str, postfix)]] <- reversed_var
    }
  }

  class(data) <- c("revcoded_df", class(data))
  return(data)
}

# =====================================================
# S3 METHOD: print.revcoded_df
# =====================================================

#' @export
print.revcoded_df <- function(x, ...) {

  rev_vars <- names(x)[
    sapply(x, function(col) inherits(col, "reverse_var"))
  ]

  cat("Data Frame with Reverse-Coded Variables\n")
  cat("---------------------------------------\n")

  if (length(rev_vars) > 0) {
    cat("Reverse-coded variables:\n")
    cat(" -", paste(rev_vars, collapse = ", "), "\n\n")
  } else {
    cat("No reverse-coded variables detected.\n\n")
  }

  NextMethod("print")
}

# =====================================================
# S3 METHOD: summary.reverse_var
# =====================================================

#' @export
summary.reverse_var <- function(object, ...) {

  cat("Summary of Reverse-Coded Variable\n")
  cat("--------------------------------\n")
  cat("Original variable : ", attr(object, "original_var"), "\n")
  cat("Max scale used    : ", attr(object, "max_scale"), "\n")
  cat("Reverse coded     : ", attr(object, "reverse"), "\n\n")

  base::summary(unclass(object), ...)
}




#' #' Reverse Coding for Multiple Variables with Custom Scale Maximums
#' #'
#' #' Reverse-codes specified variables in a data frame by subtracting the variable values from a specified maximum scale value. Supports both symbol and string input for variable names. Can also replace original variables or create new ones with a postfix.
#' #'
#' #' @param data A data.frame containing the variables to be reverse coded.
#' #' @param n A single numeric value or vector of numeric values representing the maximum scale for each variable. If a single value is provided, it is applied to all variables.
#' #' @param ... Variables to be reverse coded. Can be specified as bare names (symbols) or as character strings.
#' #' @param postfix A character string used to append to the reverse coded variable names. Default is "_r".
#' #' @param replace Logical. If TRUE, replaces the original variable with its reverse-coded version. If FALSE (default), adds new variables with the specified postfix.
#' #'
#' #' @return A data.frame with reverse-coded variables either replacing the original ones or added as new columns. Each reverse-coded variable will have an attribute `"reverse" = TRUE`.
#' #' @export
#' #'
#' #' @examples
#' #' \dontrun{
#' #' library(dplyr)
#' #'
#' #' # Example 1: One n value applied to multiple variables
#' #' stat_onl <- data.frame(
#' #'   upgrade = c(1, 2, 3, 4, 5),
#' #'   satisfy = c(2, 3, 4, 4, 5),
#' #'   fatigue = c(5, 4, 3, 2, 1)
#' #' )
#' #'
#' #' rev_coding_rep(stat_onl, 8, upgrade, satisfy, fatigue) |>
#' #'   select(upgrade, satisfy, fatigue, upgrade_r, satisfy_r, fatigue_r)
#' #'
#' #' # Example 2: Different n values per variable
#' #' rev_coding_rep(stat_onl, n = c(8, 7, 6), "upgrade", "satisfy", "fatigue") |>
#' #'   select(upgrade, satisfy, fatigue, upgrade_r, satisfy_r, fatigue_r)
#' #'
#' #' # Example 3: Using mtcars
#' #' rev_coding_rep(mtcars[, c("cyl", "gear", "carb")], 12, "cyl")
#' #'
#' #' rev_coding_rep(mtcars[, c("cyl", "gear", "carb")],
#' #'                n = c(12, 6, 9), "cyl", "gear", "carb")
#' #'
#' #' # Example 4: Small custom dataset
#' #' df <- data.frame(
#' #'   A = c(1, 2, 3, 4, 5),
#' #'   B = c(5, 4, 3, 2, 1)
#' #' )
#' #'
#' #' # 4-1. Pass as symbols
#' #' df_new1 <- rev_coding_rep(df, n = 6, A, B)
#' #' print(df_new1)
#' #' str(df_new1)
#' #' print(attr(df_new1$A_r, "reverse"))
#' #' print(attr(df_new1$B_r, "reverse"))
#' #'
#' #' # 4-2. Pass as strings
#' #' df_new2 <- rev_coding_rep(df, n = 6, "A", "B")
#' #' print(df_new2)
#' #' str(df_new2)
#' #' print(attr(df_new2$A_r, "reverse"))
#' #' print(attr(df_new2$B_r, "reverse"))
#' #'
#' #' # 4-3. Replace original columns
#' #' df_replaced <- rev_coding_rep(df, n = 6, A, B, replace = TRUE)
#' #' print(df_replaced)
#' #' str(df_replaced)
#' #' print(attr(df_replaced$A, "reverse"))
#' #' print(attr(df_replaced$B, "reverse"))
#' #'
#' #' # Example 5: Mixed input types
#' #' rev_coding_rep(df, n = 5, "A", B)
#' #'
#' #' # Example 6: Tidyverse pipeline usage
#' #' df |> rev_coding_rep(n = 6, A, B)
#' #'
#' #' # Example 7: Variables with NA values
#' #' df_na <- data.frame(
#' #'   Q1 = c(1, NA, 3, 4),
#' #'   Q2 = c(5, 4, NA, 2)
#' #' )
#' #' rev_coding_rep(df_na, n = 5, Q1, Q2)
#' #'
#' #' }
#' rev_coding_rep <- function(data, n = 8, ..., postfix = "_r", replace = FALSE) {
#'   # Capture variable names
#'   var_names <- sapply(substitute(list(...))[-1], function(x) {
#'     if (is.character(x)) as.name(x) else x
#'   })
#'
#'   if (length(n) == 1) {
#'     n <- rep(n, length(var_names))
#'   }
#'
#'   for (i in seq_along(var_names)) {
#'     var <- var_names[i]
#'     var_str <- as.character(var)
#'     n_val <- n[i]
#'
#'     reversed_var <- n_val - data[[var_str]]
#'
#'     if (replace) {
#'       data[[var_str]] <- reversed_var
#'       attr(data[[var_str]], "reverse") <- TRUE
#'     } else {
#'       new_var_name <- paste0(var_str, postfix)
#'       data[[new_var_name]] <- reversed_var
#'       attr(data[[new_var_name]], "reverse") <- TRUE
#'     }
#'   }
#'
#'   return(data)
#' }
