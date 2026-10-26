#' missfillinFor: Fill Missing Values in Selected Columns of a Data Frame
#'
#' This function fills missing values in a specified range of columns within a data frame using either
#' a specified function (e.g., `mean`, `median`) or a method such as `pmm` (predictive mean matching).
#'
#' @param data A data frame containing missing values.
#' @param fn A function to apply for filling missing values (default: `mean`).
#' @param method The method to use for imputing missing values. Accepts `NULL` (default function-based) or `"pmm"`.
#' @param range A numeric vector indicating column indices or a character string specifying the range of column names (e.g., `"col1:col3"`).
#' If `NULL`, all columns are considered.
#'
#' @return A data frame with missing values imputed in the specified range of columns.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Impute all columns with mean function
#' data <- data.frame(a = c(1, 2, NA, 4), b = c(NA, 3, 5, 6), c = c(NA, NA, 7, 8))
#' result <- missfillinFor(data, fn = mean)
#' print(result)
#'
#' # Example 2: Impute specific column range with median function
#' data <- data.frame(a = c(1, NA, 3, 4), b = c(NA, 2, NA, 5), c = c(6, 7, 8, 9))
#' result <- missfillinFor(data, fn = median, range = 1:2)
#' print(result)
#'
#' # Example 3: Impute using predictive mean matching (pmm)
#' data <- data.frame(a = c(1, NA, 3, 4), b = c(NA, 2, NA, 5), c = c(6, 7, 8, 9))
#' result <- missfillinFor(data, method = "pmm", range = "a:b")
#' print(result)
#'
#' # Example 4: Impute using column names
#' data <- data.frame(a = c(1, 2, NA, 4), b = c(NA, 3, 5, 6), c = c(NA, NA, 7, 8))
#' result <- missfillinFor(data, fn = mean, range = "a:c")
#' print(result)
#'
#' # Example 5: Handle insufficient columns for pmm
#' data <- data.frame(a = c(1, 2, NA, 4))
#' result <- tryCatch(missfillinFor(data, method = "pmm", range = 1), error = function(e) e)
#' print(result)
#' }
missfillinFor <- function(data, fn = mean, method = NULL, range = NULL) {
  data <- as.data.frame(data)

  if (is.null(range)) {
    range <- seq_len(ncol(data))
  } else if (is.character(range)) {
    col_names <- colnames(data)
    range_split <- strsplit(range, ":")[[1]]
    if (length(range_split) != 2L) {
      stop("문자열 범위는 '시작열:끝열' 형식이어야 합니다.", call. = FALSE)
    }
    start_col <- which(col_names == range_split[1])
    end_col <- which(col_names == range_split[2])
    if (length(start_col) == 0 || length(end_col) == 0) {
      stop("지정된 열 이름이 데이터에 존재하지 않습니다.")
    }
    range <- seq(start_col, end_col)
  }

  if (!is.numeric(range) || anyNA(range) ||
      any(range < 1L | range > ncol(data))) {
    stop("`range`에는 존재하는 열의 위치를 지정해야 합니다.", call. = FALSE)
  }
  range <- unique(as.integer(range))

  if (!is.null(method) && method == "pmm") {
    if (!requireNamespace("mice", quietly = TRUE)) {
      stop(
        "PMM 결측치 대치를 사용하려면 `mice` 패키지가 필요합니다.",
        call. = FALSE
      )
    }
    selected_data <- data[, range, drop = FALSE]
    if (ncol(selected_data) < 2) {
      stop("mice는 최소 두 개 이상의 열이 필요합니다.")
    }
    mice_result <- mice::mice(
      selected_data,
      m = 5,
      method = "pmm",
      maxit = 10,
      seed = 123,
      printFlag = FALSE
    )
    data[, range] <- mice::complete(mice_result, 1)
  } else {
    for (i in range) {
      data[, i] <- missfillin(data[, i, drop = FALSE], fn = fn, method = method)
    }
  }

  data
}
