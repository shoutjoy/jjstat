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
