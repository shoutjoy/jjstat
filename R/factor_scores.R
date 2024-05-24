#' Add factor score calculation functions and data
#'
#' @param data data
#' @param range range default 1:ncol(data)
#' @param nfactor number of factor
#' @param rotate varimax
#' @param prefix data prefic
#'
#' @return data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#' # 활용방법 1
#' factor_scores(mtcars,
#'               range = c("disp", "hp", "drat", "wt", "qsec"),
#'               nfactor = 2, prefix="a")
#'
#' mtcars %>% select("disp", "hp", "drat", "wt", "qsec") %>% factor_scores(nfactor=2)
#'
#' # 활용방법 2
#' factor_scores(mtcars, range = c(3:7), nfactor = 2)
#'
#' factor_scores(mtcars, c("disp", "hp", "drat", "wt", "qsec"), nfactor = 2)
#' factor_scores(mtcars, c(3, 4, 5, 6, 7), nfactor = 2)
#'
#' factor_scores(mtcars, range = "disp:qsec", nfactor = 2)
#'
#'
#' mtcars %>% select("disp", "hp", "drat", "wt", "qsec") %>%
#'   factor_scores(c("disp","hp"),nfactor=2, prefix="a") %>%
#'   factor_scores(c("drat", "wt", "qsec"),nfactor=3, prefix="b") %>%head()
#'
#' }
factor_scores <- function(data, range = 1:ncol(data), nfactor=NULL,
                          prefix = "", rotate = "varimax") {
  # 범위가 문자열 범위인 경우 dplyr::select를 사용하여 데이터 선택
  if (is.character(range) && length(range) == 1 && grepl(":", range)) {
    range_vars <- unlist(strsplit(range, ":"))
    data_subset <- dplyr::select(data, dplyr::all_of(range_vars[1]):dplyr::all_of(range_vars[2]))
  } else if (is.character(range)) {
    data_subset <- data[, range]
  } else if (is.numeric(range)) {
    data_subset <- data[, range]
  } else {
    stop("range는 문자열 범위, 문자열 벡터 또는 숫자 벡터여야 합니다.")
  }

  # 요인분석 수행
  fa_result <- psych::fa(data_subset, nfactors = nfactor, rotate = rotate)

  # 요인 점수 계산
  factor_scores <- psych::factor.scores(data_subset, fa_result$loadings)$scores

  # 요인 점수 컬럼 이름 지정
  colnames(factor_scores) <- paste0(prefix, "Fac", seq_len(nfactor))

  # 계산된 요인 점수를 원본 데이터에 추가
  data <- cbind(data, factor_scores)

  return(data)
}



#' Add factor score calculation functions and data
#'
#' @param data data
#' @param range range default 1:ncol(data)
#' @param nfactor number of factor
#' @param rotate varimax
#' @param prefix data prefic
#'
#' @return data
#' @export
#'
#' @examples
#'
#'
#' \dontrun{
#' #'
#' # 활용방법 1
#' factor_scores(mtcars,
#'               range = c("disp", "hp", "drat", "wt", "qsec"),
#'               nfactor = 2, prefix="a")
#'
#' mtcars %>% select("disp", "hp", "drat", "wt", "qsec") %>% factor_scores(nfactor=2)
#'
#' # 활용방법 2
#' factor_scores(mtcars, range = c(3:7), nfactor = 2)
#'
#' factor_scores(mtcars, c("disp", "hp", "drat", "wt", "qsec"), nfactor = 2)
#' factor_scores(mtcars, c(3, 4, 5, 6, 7), nfactor = 2)
#'
#' factor_scores(mtcars, range = "disp:qsec", nfactor = 2)
#'
#'
#' mtcars %>% select("disp", "hp", "drat", "wt", "qsec") %>%
#'   factor_scores(c("disp","hp"),nfactor=2, prefix="a") %>%
#'   factor_scores(c("drat", "wt", "qsec"),nfactor=3, prefix="b") %>%head()
#'
#' }
fs <- function(data, range = 1:ncol(data), nfactor=NULL,
                           prefix = "", rotate = "varimax") {
  # 범위가 문자열 범위인 경우 dplyr::select를 사용하여 데이터 선택
  if (is.character(range) && length(range) == 1 && grepl(":", range)) {
    range_vars <- unlist(strsplit(range, ":"))
    data_subset <- dplyr::select(data, dplyr::all_of(range_vars[1]):dplyr::all_of(range_vars[2]))
  } else if (is.character(range)) {
    data_subset <- data[, range]
  } else if (is.numeric(range)) {
    data_subset <- data[, range]
  } else {
    stop("range는 문자열 범위, 문자열 벡터 또는 숫자 벡터여야 합니다.")
  }

  # 요인분석 수행
  fa_result <- psych::fa(data_subset, nfactors = nfactor, rotate = rotate)

  # 요인 점수 계산
  factor_scores <- psych::factor.scores(data_subset, fa_result$loadings)$scores

  # 요인 점수 컬럼 이름 지정
  colnames(factor_scores) <- paste0(prefix, "Fac", seq_len(nfactor))

  # 계산된 요인 점수를 원본 데이터에 추가
  data <- cbind(data, factor_scores)

  return(data)
}

