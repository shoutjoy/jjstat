#' cov to cor
#'
#' @param cov_matrix cov matrix
#'
#' @return cor mat
#' @export
#'
#' @examples
#'
#' \dontrun{
#' cov(mtcars[,c("mpg","hp")])%>%cov2cor()
#' }
cov2cor <- function(cov_matrix) {
  # 각 변수의 표준편차 추출
  sd_values <- sqrt(diag(cov_matrix))

  # 상관행렬 계산
  cor_matrix <- cov_matrix / (outer(sd_values, sd_values))

  return(cor_matrix)
}



#'cor2cov
#'
#' @param cor_matrix cor
#' @param sds sds =c(...), default sds 1
#'
#' @return cov
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # correlation matrix
#' mtcars[1:6] %>%cor()
#'
#' #change cor to cov
#' mtcars[1:6] %>%cor() %>% cor2cov(sds = mtcars[1:6] %>% sapply(sd))
#'
#' #real cov matrix
#' mtcars[1:6] %>%cov()
#'
#' }
#'
#'
cor2cov <- function(cor_matrix, sds = rep(1, ncol(cor_matrix))) {
  n <- ncol(cor_matrix)
  cov_matrix <- matrix(0, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in 1:n) {
      cov_matrix[i, j] <- cor_matrix[i, j] * sds[i] * sds[j]
    }
  }
  colnames(cov_matrix) <- rownames(cov_matrix) <- colnames(cor_matrix)
  return(cov_matrix)
}
