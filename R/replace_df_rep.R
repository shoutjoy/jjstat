#' replace_df_repeated adation
#'
#' @param data data
#' @param ... pattern and imputation
#' @param detect detect TRUE in data, FALSE data
#' @param chr transformation fac  to chr
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Use case 1: Passing pattern-replacement string pairs directly as variable arguments
#' data <- data.frame(
#'   col1 = c("aa", "bb", "cc"),
#'   col2 = c("aa", "bb", "dd")
#' )
#'
#' print("Before Replacement:")
#' print(data)
#'
#' result1 <- replace_df_rep(data, "aa", "a1", "bb", "b1")
#' print("After Replacement (direct arguments):")
#' print(result1)
#'
#' # Use case 2: Passing as a single vector
#' changes <- c("aa", "a1", "bb", "b1")
#' result2 <- replace_df_rep(data, changes)
#' print("After Replacement (vector):")
#' print(result2)
#' #'
#'
#' #'
#' # 예시 데이터
#' data <- data.frame(
#'   Question = c("나는 나의 세 가지 가장 큰 약점을 열거할 수 있다.", "나의 행동은 나의 핵심 가치를 반영한다.", "나는 스스로 결정하기 전에 다른 사람들의 의견을 구한다."),
#'   factor = c("자기", "자기", "자기"),
#'   stringsAsFactors = FALSE
#' )
#'
#' data
#' replace_df_rep(data, "약점", "강점","행동","수행","스스로","다른 사람이",  detect = TRUE)
#' replace_df_rep(data, "약점", "강점","행동","수행",  detect = FALSE)
#'
#'
#' }
#'
#'
#'
replace_df_rep <- function(data, ..., detect = TRUE, chr = TRUE) {
  # Convert a variable of type factor in the dataframe to type character if chr is TRUE
  if (chr) {
    data <- data %>%
      dplyr::mutate(across(where(is.factor), as.character))
  }

  args <- list(...)

  if (length(args) == 1 && is.vector(args[[1]])) {
    changes <- args[[1]]
  } else {
    changes <- unlist(args)
  }

  if (length(changes) %% 2 != 0) {
    stop("Arguments should be in pairs of pattern and replacement")
  }

  if (detect) {
    # if detect is TRUE, traverse all cells in the dataframe to replace words
    data <- replace_in_dataframe(data, changes)
  } else {
    # Replace dataframes with replace_dataframe if detect is FALSE
    for (i in seq(1, length(changes), by = 2)) {
      pattern <- changes[i]
      imp <- changes[i + 1]
      data <- replace_df(data, pattern = pattern, imp = imp)
    }
  }

  return(data)
}




#' replace_in_dataframe
#'
#' @param data  data
#' @param changes change
#'
#' @return data
#' @export
#'

replace_in_dataframe <- function(data, changes) {
  for (i in seq(1, length(changes), by = 2)) {
    pattern <- changes[i]
    imp <- changes[i + 1]
    data <- replace_dataframe(data, pattern, imp)
  }
  return(data)
}

