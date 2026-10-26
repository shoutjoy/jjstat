

#' missfillin: Fill Missing Values in a Data Frame
#'
#' This function fills missing values in a data frame using either a specified function (e.g., `mean` or `median`)
#' or a method such as `pmm` (predictive mean matching) through the `mice` package.
#'
#' @param data A data frame containing missing values.
#' @param fn A function to apply for filling missing values (default: `mean`).
#' @param method The method to use for imputing missing values. Accepts `NULL` (default function-based) or `"pmm"`.
#'
#' @return A data frame with missing values imputed.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Impute with mean function
#' data <- data.frame(a = c(1, 2, NA, 4), b = c(NA, 3, 5, 6))
#' result <- missfillin(data, fn = mean)
#' print(result)
#'
#' # Example 2: Impute with median function
#' data <- data.frame(a = c(NA, 1, 2, 3), b = c(4, NA, NA, 8))
#' result <- missfillin(data, fn = median)
#' print(result)
#'
#' # Example 3: Impute using predictive mean matching (pmm)
#' data <- data.frame(a = c(1, NA, 3, 4), b = c(NA, 2, NA, 5))
#' result <- missfillin(data, method = "pmm")
#' print(result)
#'
#' # Example 4: Handle data without missing values
#' data <- data.frame(a = c(1, 2, 3, 4), b = c(4, 5, 6, 7))
#' result <- missfillin(data)
#' print(result)
#'
#' # Example 5: Error when insufficient columns for pmm
#' data <- data.frame(a = c(1, 2, NA, 4))
#' result <- tryCatch(missfillin(data, method = "pmm"), error = function(e) e)
#' print(result)
#' }
missfillin <- function(data, fn = mean, method = NULL) {
  data <- as.data.frame(data)
  if (!is.function(fn)) {
    stop("`fn` must be a function.", call. = FALSE)
  }

  missing_info <- colSums(is.na(data))

  if (any(missing_info > 0)) {
    if (!is.null(method)) {
      if (method == "pmm") {
        if (!requireNamespace("mice", quietly = TRUE)) {
          stop(
            "PMM 결측치 대치를 사용하려면 `mice` 패키지가 필요합니다.",
            call. = FALSE
          )
        }
        if (ncol(data) < 2) {
          stop("mice는 최소 두 개 이상의 열이 필요합니다.")
        }
        mice_result <- mice::mice(
          data,
          m = 5,
          method = "pmm",
          maxit = 10,
          seed = 123,
          printFlag = FALSE
        )
        data <- mice::complete(mice_result, 1)
      } else {
        stop("지원하지 않는 방법입니다. method = NULL 또는 'pmm' 중에서 선택하세요.")
      }
    } else {
      data[] <- lapply(data, function(column) {
        missing <- is.na(column)
        if (any(missing)) {
          column[missing] <- fn(column, na.rm = TRUE)
        }
        column
      })
    }
  }

  data
}


