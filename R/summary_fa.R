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
summary_fa = function(fa_data, digits=3){

  eigenvalues = matrix(NA, nrow = 3, ncol = fa_data$factors)

  for(i in 1:fa_data$factors){
    eigenvalues[1, i] <- sum(fa_data$loadings[, i]^2)
    eigenvalues[2, i] <- eigenvalues[1, i]/ dim(fa_data$loadings[,])[1]
  }

  eigenvalues[3,] <- cumsum(eigenvalues[2,])

  rownames(eigenvalues) <- c("eigen_vlaue","ProportionVar","Cumulative_Var")



  temp  <- round(rbind(fa_data$loadings[,],
                       eigenvalues), digits)

  result <- as_tibble(temp)%>%
    dplyr::mutate(source = rownames(temp)) %>%
    dplyr::select(source, colnames(temp))
  result
}
