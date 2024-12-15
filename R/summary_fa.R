#' FA,PCA result summary
#'
#' @param fa_data fa, factanal, pricipla result
#' @param digits roundign defaylt 4
#'
#' @return table
#' @export
#'
#' @examples
#'
#' library(MVT)
#'
#' #  Mardia, K.V., Kent, J.T., and Bibby, J.M. (1979).  _Multivariate
#' #  Analysis_. Academic Press, London.
#' data(examScor)
#' factanal(scale(examScor), factors = 2, rotation = "varimax")%>% summary_fa()
#'
#' #
#' psych::principal(cor(examScor), nfactor = 2) %>%summary_fa()
#'
#' psych::prcomp(cor(examScor), nfactor = 2) %>%summary_fa()
#'
#' factanal(scale(examScor), factors = 2) %>%summary_fa()
#'
summary_fa <- function(fa_data, digits = 3) {
  # 요인 수 가져오기
  factors <- fa_data$factors
  loadings <- fa_data$loadings

  # 고유값 계산 결과 저장 행렬 초기화
  eigenvalues <- matrix(NA, nrow = 3, ncol = factors)

  for (i in 1:factors) {
    eigenvalues[1, i] <- sum(loadings[, i]^2)  # Eigenvalues (SS Loadings)
    eigenvalues[2, i] <- eigenvalues[1, i] / nrow(loadings)  # Proportion of Variance
  }

  # 누적 비율 계산
  eigenvalues[3, ] <- cumsum(eigenvalues[2, ])

  # 행 이름 추가
  rownames(eigenvalues) <- c("Eigenvalues", "Proportion Var", "Cumulative Var")
  colnames(eigenvalues) <- paste0("Factor", 1:factors)

  # 요인이 1개인 경우 처리
  if (factors == 1) {
    # 로딩값 테이블 생성 (1개 요인)
    temp <- round(rbind(loadings, eigenvalues), digits)
    colnames(temp) <- paste0("MR", 1:factors)
    rownames(temp) <- c(rownames(loadings), rownames(eigenvalues))
  } else {
    # 로딩값 테이블 생성 (2개 이상 요인)
    temp <- round(rbind(loadings, eigenvalues), digits)
    rownames(temp) <- c(rownames(loadings), rownames(eigenvalues))
  }

  # 결과를 tibble로 변환
  result <- as_tibble(temp) %>%
    dplyr::mutate(source = rownames(temp)) %>%
    dplyr::select(source, everything())

  return(result)
}
#' #'
#' summary_fa = function(fa_data, digits=3){
#'
#'   eigenvalues = matrix(NA, nrow = 3, ncol = fa_data$factors)
#'
#'   for(i in 1:fa_data$factors){
#'     eigenvalues[1, i] <- sum(fa_data$loadings[, i]^2)
#'     eigenvalues[2, i] <- eigenvalues[1, i]/ dim(fa_data$loadings[,])[1]
#'   }
#'
#'   eigenvalues[3,] <- cumsum(eigenvalues[2,])
#'
#'   rownames(eigenvalues) <- c("eigen_vlaue","Proportion_Var","Cumulative_Var")
#'
#'
#'
#'   temp  <- round(rbind(fa_data$loadings[,],
#'                        eigenvalues), digits)
#'
#'   result <- as_tibble(temp)%>%
#'     dplyr::mutate(source = rownames(temp)) %>%
#'     dplyr::select(source, colnames(temp))
#'   result
#' }
