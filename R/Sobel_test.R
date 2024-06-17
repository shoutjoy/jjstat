
#' sobel test
#'
#' @param a est1
#' @param b est2
#' @param sa  se1
#' @param sb  se2
#' @param type Sobel, Aroian, Goodman
#'
#' @return test result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example usage
#' a <- 0.5  # Example coefficient a
#' b <- 0.3  # Example coefficient b
#' sa <- 0.1 # Standard error of a
#' sb <- 0.05 # Standard error of b
#'
#' Sobel_test(a, b, sa, sb)
#' Sobel_test(a, b, sa, sb, "all")
#' Sobel_test(a, b, sa, sb, "Sobel")
#'
#' Sobel_test(a, b, sa, sb,"Aroian")
#'
#' Sobel_test(a, b, sa, sb,"Goodman")
#' }
#'
Sobel_test <-  function(a, b, sa, sb) {
  # Sobel test equation

  ind_coef = a*b
  sobel_se <- sqrt(b^2 * sa^2 + a^2 * sb^2)
  sobel_z <- a * b / sobel_se
  sobel_p <- 2 * (1 - pnorm(abs(sobel_z)))

  # Aroian test equation
  aroian_se <- sqrt(b^2 * sa^2 + a^2 * sb^2 + sa^2 * sb^2)
  aroian_z <- a * b / aroian_se
  aroian_p <- 2 * (1 - pnorm(abs(aroian_z)))

  # Goodman test equation
  goodman_se <- sqrt(b^2 * sa^2 + a^2 * sb^2 - sa^2 * sb^2)
  goodman_z <- a * b / goodman_se
  goodman_p <- 2 * (1 - pnorm(abs(goodman_z)))

  # Return results as a list
  result <- list(
    Sobel = cbind(ind_coef= ind_coef,  SE = sobel_se,Z = sobel_z, p_value = sobel_p),
    Aroian = cbind(ind_coef= ind_coef,SE = aroian_se,  Z = aroian_z,  p_value = aroian_p),
    Goodman = cbind(ind_coef= ind_coef, SE = goodman_se, Z = goodman_z, p_value = goodman_p)
  )
  test_name = names(result)
  data = result%>%do.call(what= rbind)

  bind_cols(Test= test_name, data)

}


#' Generalized Sobel Test function
#'
#' @param a est
#' @param SE se
#' @param digits 5
#'
#' @return result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' general_Sobel_test(a = c(0.5, 0.3), SE = c(0.1, 0.05))
#'
#' Sobel_test_2(a=0.5, b=0.3, sa=0.1, sb =0.05)
#'
#' # Input data
#' a <- c(0.2, 0.3, 0.4, 0.5)
#' SE <- c(0.05, 0.04, 0.03, 0.02)
#'
#' # Calculate results
#' sobel_test_general(a = c(0.2, 0.3, 0.4, 0.5), SE = c(0.05, 0.04, 0.03, 0.02))
#' aroian_sobel_test_general(a = c(0.2, 0.3, 0.4, 0.5), SE = c(0.05, 0.04, 0.03, 0.02))
#' Goodman_sobel_test_general(a = c(0.2, 0.3, 0.4, 0.5), SE = c(0.05, 0.04, 0.03, 0.02))
#' general_Sobel_test(a = c(0.2, 0.3, 0.4, 0.5), SE = c(0.05, 0.04, 0.03, 0.02))
#' #'
#' }
#'
#'
sobel_test_general <- function(a, SE, digits=5) {
  Indirect_Coefficient <- prod(a)
  k <- length(a)
  term1 <- sum(sapply(1:k, function(i) { (prod(a[-i])^2) * (SE[i]^2) }))
  SE_Indirect <- sqrt(term1)
  Z_statistic <- Indirect_Coefficient / SE_Indirect
  p_value <- 2 * (1 - pnorm(abs(Z_statistic)))
  test <- "Sobel_test_generalized"
  result <- data.frame(test = test, ind_coef = Indirect_Coefficient,
                       SE = SE_Indirect, Z = Z_statistic, p_value = p_value) %>%
    Round(digits)
  return(result)
}

#' Aroian Approximation Sobel Test generalized function
#'
#' @param a est1
#' @param b est2
#' @param sa  se1
#' @param sb  se2
#' @param type Sobel, Aroian, Goodman
#'
#' @return test result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example usage
#' a <- 0.5  # Example coefficient a
#' b <- 0.3  # Example coefficient b
#' sa <- 0.1 # Standard error of a
#' sb <- 0.05 # Standard error of b
#'
#' Sobel_test(a, b, sa, sb)
#' Sobel_test(a, b, sa, sb, "all")
#' Sobel_test(a, b, sa, sb, "Sobel")
#'
#' Sobel_test(a, b, sa, sb,"Aroian")
#'
#' Sobel_test(a, b, sa, sb,"Goodman")
#' }
aroian_sobel_test_general <- function(a, SE, digits=5) {
  Indirect_Coefficient <- prod(a)
  k <- length(a)
  term1 <- sum(sapply(1:k, function(i) { (prod(a[-i]^2) * SE[i]^2) }))
  term2 <- prod(SE^2)
  SE_Indirect <- sqrt(term1 + term2)
  Z_statistic <- Indirect_Coefficient / SE_Indirect
  p_value <- 2 * (1 - pnorm(abs(Z_statistic)))
  test <- "Aroian_sobel_test"
  result <- data.frame(test = test, ind_coef = Indirect_Coefficient,
                       SE = SE_Indirect, Z = Z_statistic, p_value = p_value) %>%
    Round(digits)
  return(result)
}

#' Goodman Sobel Test generalized function
#'
#' @param a est1
#' @param b est2
#' @param sa  se1
#' @param sb  se2
#' @param type Sobel, Aroian, Goodman
#'
#' @return test result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example usage
#' a <- 0.5  # Example coefficient a
#' b <- 0.3  # Example coefficient b
#' sa <- 0.1 # Standard error of a
#' sb <- 0.05 # Standard error of b
#'
#' Sobel_test(a, b, sa, sb)
#' Sobel_test(a, b, sa, sb, "all")
#' Sobel_test(a, b, sa, sb, "Sobel")
#'
#' Sobel_test(a, b, sa, sb,"Aroian")
#'
#' Sobel_test(a, b, sa, sb,"Goodman")
#' }
goodman_sobel_test_general <- function(a, SE, digits=5) {

  if(length(a) <= 2){
    Indirect_Coefficient <- prod(a)
    term1 <- a[2]^2 * SE[1]^2 + a[1]^2 * SE[2]^2 - SE[1]^2 * SE[2]^2
    SE_Indirect <- sqrt(term1)

  }else{

    Indirect_Coefficient <- prod(a)
    k <- length(a)
    term1 <- sum(sapply(1:k, function(i) { (prod(a[-i]^2) * SE[i]^2) }))
    term2 <- sum(sapply(1:(k-1), function(i) { sum(sapply((i+1):k,
                         function(j) { (a[i] * a[j])^2 * (SE[i]^2) * (SE[j]^2) })) }))
    term3 <- prod(SE^2)
    SE_Indirect <- sqrt(term1 + term2 + term3)
  }

  Z_statistic <- Indirect_Coefficient / SE_Indirect
  p_value <- 2 * (1 - pnorm(abs(Z_statistic)))
  test <- "Goodman_sobel_test"
  result <- data.frame(test = test, ind_coef = Indirect_Coefficient,
                       SE = SE_Indirect, Z = Z_statistic, p_value = p_value) %>%
    Round(digits)
  return(result)
}


#' Sobeltest toal
#'
#' @param a est1
#' @param b est2
#' @param sa  se1
#' @param sb  se2
#' @param type Sobel, Aroian, Goodman
#'
#' @return test result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' # Example usage
#' a <- 0.5  # Example coefficient a
#' b <- 0.3  # Example coefficient b
#' sa <- 0.1 # Standard error of a
#' sb <- 0.05 # Standard error of b
#'
#' Sobel_test(a, b, sa, sb)
#' Sobel_test(a, b, sa, sb, "all")
#' Sobel_test(a, b, sa, sb, "Sobel")
#'
#' Sobel_test(a, b, sa, sb,"Aroian")
#'
#' Sobel_test(a, b, sa, sb,"Goodman")
#' }
general_Sobel_test= function(a, SE, digits = 6){

  sobeltest = sobel_test_general(a, SE, digits )
  aroian_sobel = aroian_sobel_test_general(a, SE, digits )
  Goodman_sobel = goodman_sobel_test_general(a, SE, digits )

  res= bind_rows(sobeltest,aroian_sobel , Goodman_sobel)
  res
}


