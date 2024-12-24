

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
#' @import VIM
#' @import Hmisc
#' @import mice
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
  library(VIM)
  library(Hmisc)
  library(mice)

  # 데이터 프레임으로 변환
  data <- as.data.frame(data)

  # 결측치 확인
  missing_info <- aggr(data, prop = FALSE, numbers = TRUE, plot = FALSE)$missings

  # 결측치가 있을 경우
  if (any(missing_info > 0)) {
    if (!is.null(method)) {
      if (method == "pmm") {
        # fn 무시하고 pmm 방식 적용
        if (ncol(data) < 2) {
          stop("mice는 최소 두 개 이상의 열이 필요합니다.")
        }
        mice_result <- mice(data, m = 5, method = "pmm", maxit = 10, seed = 123)
        data <- complete(mice_result, 1)
      } else {
        stop("지원하지 않는 방법입니다. method = NULL 또는 'pmm' 중에서 선택하세요.")
      }
    } else {
      # mean 또는 median 함수로 채우기
      data <- Hmisc::impute(unlist(data), fn(unlist(data), na.rm = TRUE))
    }
  }

  return(data)
}


