
#' sobel test
#'
#' @param a est1
#' @param b est2
#' @param sa  se1
#' @param sb  se2
#' @param roburst  roburst T whide correction
#' @param n sample size
#' @param k1 number of regressin coefs, default 2
#' @param k2 number of regressin coefs, default 2
#' @param digits 5
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
#'
#'
#' # roburst se correction : White(1980)
#' Sobel_test(0.2, 0.3,0.05, 0.04, TRUE, 100)
#'
#' }
#'
Sobel_test <-  function(a, b, sa, sb,
                        roburst=FALSE, n=NULL, k1=2, k2=2,
                        digits= 5,
                        conf_level = 0.95) {

  if(roburst){
    # 헤테로스케다스티시티 조정 계수 계산
    adjustment_factor1 <- sqrt(n / (n + k1))
    adjustment_factor2 <- sqrt(n / (n + k2))

    sa0 = sa
    sb0 = sb
    # 강건 표준오차 계산
    sa <- sa *  sqrt(n / (n + k1))
    sb <- sb *  sqrt(n / (n + k2))

    cat("\n","Roburst SE ratio(Scott & Ervin(2000)):",  "\n",
        "adjustment_factor = sqrt(n / (n - k))
","\n",
        "sa = ", sa0, ", se_a_factor =", adjustment_factor1,"se_a: ",sa,"\n",
        "sb = ", sb0, ", se_b_factor =", adjustment_factor2,"se_b: ",sb,"\n")


  }
  # Sobel test equation

  ind_coef = a * b
  sobel_se <- sqrt(b^2 * sa^2 + a^2 * sb^2)
  sobel_z <- a * b / sobel_se
  sobel_p <- 2 * (1 - pnorm(abs(sobel_z)))

  # Calculate confidence intervals for Sobel test
  sobel_ci_lower <- ind_coef - qnorm(1 - (1 - conf_level) / 2) * sobel_se
  sobel_ci_upper <- ind_coef + qnorm(1 - (1 - conf_level) / 2) * sobel_se

  # Aroian test equation
  aroian_se <- sqrt(b^2 * sa^2 + a^2 * sb^2 + sa^2 * sb^2)
  aroian_z <- a * b / aroian_se
  aroian_p <- 2 * (1 - pnorm(abs(aroian_z)))

  # Calculate confidence intervals for Aroian test
  aroian_ci_lower <- ind_coef - qnorm(1 - (1 - conf_level) / 2) * aroian_se
  aroian_ci_upper <- ind_coef + qnorm(1 - (1 - conf_level) / 2) * aroian_se

  # Goodman test equation
  goodman_se <- sqrt(b^2 * sa^2 + a^2 * sb^2 - sa^2 * sb^2)
  goodman_z <- a * b / goodman_se
  goodman_p <- 2 * (1 - pnorm(abs(goodman_z)))

  # Calculate confidence intervals for Goodman test
  goodman_ci_lower <- ind_coef - qnorm(1 - (1 - conf_level) / 2) * goodman_se
  goodman_ci_upper <- ind_coef + qnorm(1 - (1 - conf_level) / 2) * goodman_se

  # Return results as a list
  result <- list(
    Sobel_delta = cbind(ind_coef = round(ind_coef, digits),
                        SE = round(sobel_se,digits),
                        Z = round(sobel_z,digits),
                        p_value = sobel_p,
                        CI_lower = sobel_ci_lower,
                        CI_upper = sobel_ci_upper),
    Aroian_exact = cbind(ind_coef = round(ind_coef, digits),
                         SE = round(aroian_se, digits),
                         Z = round(aroian_z, digits),
                         p_value = aroian_p,
                         CI_lower = aroian_ci_lower,
                         CI_upper = aroian_ci_upper),
    Goodman_unbaised = cbind(ind_coef = round(ind_coef,digits),
                             SE = round(goodman_se, digits),
                             Z = round(goodman_z, digits),
                             p_value = goodman_p,
                             CI_lower = goodman_ci_lower,
                             CI_upper = goodman_ci_upper)
  )
  test_name = names(result)
  data = result %>% do.call(what = rbind)

  bind_cols(test = test_name, data)
}




#' Generalized Sobel Test function
#'
#' @param a est
#' @param SE se
#' @param digits 5
#' @param conf 0.90, 0.95, 0.99, 0.999
#' @param roburst  roburst T whide correction
#' @param n sample size
#' @param k number of regressin coefs, default 2
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
#' # 예제 데이터
#' a <- c(0.5, 0.3, 0.4)
#' SE <- c(0.1, 0.1, 0.1)
#'
#' # 함수 실행 (기본 95% 신뢰구간)
#' sobel_test_general(a, SE)
#'
#' # 함수 실행 (90% 신뢰구간)
#' result_90 <- sobel_test_general(a, SE, conf=0.90)
#' print(result_90)
#'
#' # 함수 실행 (99% 신뢰구간)
#' result_99 <- sobel_test_general(a, SE, conf=0.99)
#' print(result_99)
#'
#' # 함수 실행 (99.9% 신뢰구간)
#' result_999 <- sobel_test_general(a, SE, conf=0.999)
#' print(result_999)
#'
#' }
#'
#'

sobel_test_general <- function(a, SE,
                               roburst=FALSE, n=NULL, k=2,
                               digits=5, conf=0.95) {

  library(dplyr)

  if(roburst){
    # 헤테로스케다스티시티 조정 계수 계산
    adjustment_factor <- sqrt(n / (n - k))


    # 강건 표준오차 계산
    SE = SE * adjustment_factor

    cat("\n","Roburst SE ratio(Scott & Ervin, 2000; Hinkley, 1977):",
        adjustment_factor,"\n")
  }else{
    SE=SE
  }


  # 매개변수 개수
  k <- length(a)

  coef_indirect <- prod(a)

  # 일반화된 소벨 테스트 표준 오차 계산
  SE_sobel_sum <- sum((SE^2) / (a^2))

  # 최종 표준 오차 계산
  SE_sobel <- sqrt((prod(a)^2) * SE_sobel_sum)

  # z값 계산
  z_value <- coef_indirect / SE_sobel

  # p값 계산 (양측 검정)
  p_value <- 2 * (1 - pnorm(abs(z_value)))

  # 허용되는 신뢰수준과 해당하는 Z 값
  conf_levels <- c(0.90, 0.95, 0.99, 0.999)
  Z_values <- c(1.645, 1.96, 2.576, 3.291)  # 각각의 신뢰수준에 해당하는 Z 값

  # 신뢰수준 확인
  if (!(conf %in% conf_levels)) {
    stop("conf는 0.90, 0.95, 0.99, 0.999 중 하나여야 합니다.")
  }

  # 적절한 Z 값 선택
  Z_alpha_half <- Z_values[which(conf_levels == conf)]

  # 신뢰구간 계산
  CI_lower <- coef_indirect - Z_alpha_half * SE_sobel
  CI_upper <- coef_indirect + Z_alpha_half * SE_sobel

  test <- "sobel_test_delta"
  # 결과를 data.frame으로 반환
  result <-  dplyr::bind_cols(
    test = test,
    ind_coef = round(coef_indirect, digits),
    SE = round(SE_sobel, digits),
    Z = round(z_value, digits),
    p_value = round(p_value, digits),
    CI_lower = round(CI_lower, digits),
    CI_upper = round(CI_upper, digits)
  )

  return(result)
}

#' Aroian Approximation Sobel Test generalized function
#'
#' @param a est
#' @param SE se
#' @param digits 5
#' @param conf 0.90, 0.95, 0.99, 0.999
#'
#' @param roburst  roburst T whide correction
#' @param n sample size
#' @param k number of regressin coefs, default 2
#' @return result
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
#'
#' #'
#' # 예제 데이터
#' a <- c(0.5, 0.3, 0.4)
#' SE <- c(0.1, 0.1, 0.1)
#'
#' # 함수 실행 (기본 95% 신뢰구간)
#' aroian_sobel_test_general(a, SE)
#' print(result_95)
#'
#' # 함수 실행 (90% 신뢰구간)
#' result_90 <- aroian_sobel_test_general(a, SE, conf=0.90)
#' print(result_90)
#'
#' # 함수 실행 (99% 신뢰구간)
#' result_99 <- aroian_sobel_test_general(a, SE, conf=0.99)
#' print(result_99)
#'
#' # 함수 실행 (99.9% 신뢰구간)
#' result_999 <- aroian_sobel_test_general(a, SE, conf=0.999)
#' print(result_999)
#'
#'
#' aroian_sobel_test_general(a = c(0.2, 0.3, 0.4, 0.5),
#'                           SE = c(0.05, 0.04, 0.03, 0.02))%>%unite_ci()

#'
#' }
aroian_sobel_test_general <- function(a, SE,
                                      roburst=FALSE, n=NULL, k=2,
                                      digits=5, conf=0.95) {

  # 허용되는 신뢰수준과 해당하는 Z 값
  if(roburst){
    # 헤테로스케다스티시티 조정 계수 계산
    adjustment_factor <- sqrt(n / (n - k))

    # 강건 표준오차 계산
    # 강건 표준오차 계산
    SE = SE * adjustment_factor
    cat("\n","Roburst SE ratio(Scott & Ervin, 2000; Hinkley, 1977):",
        adjustment_factor,"\n")
  }else{
    SE=SE
  }



  conf_levels <- c(0.90, 0.95, 0.99, 0.999)
  Z_values <- c(1.645, 1.96, 2.576, 3.291)  # 각각의 신뢰수준에 해당하는 Z 값

  # 신뢰수준 확인
  if (!(conf %in% conf_levels)) {
    stop("conf는 0.90, 0.95, 0.99, 0.999 중 하나여야 합니다.")
  }

  # 적절한 Z 값 선택
  Z_alpha_half <- Z_values[which(conf_levels == conf)]

  # Indirect coefficient
  coef_indirect <- prod(a)

  # 일반화된 식에 따른 표준 오차 계산
  n <- length(a)

  # 첫 번째 항의 합 계산
  term1 <- sum(sapply(1:n, function(i) {
    prod(a[-i]^2) * SE[i]^2
  }))

  # 두 번째 항의 합 계산
  term2 <- 0
  for (m in 2:n) {
    comb_terms <- combn(n, m, simplify = FALSE)
    term2 <- term2 + sum(sapply(comb_terms, function(indices) {
      prod(a[indices]^2) * prod(SE[indices]^2)
    }))
  }

  # term3 <- prod(SE^2)

  # Standard Error
  SE_indirect <- sqrt(term1 + term2 )

  # Z statistic
  Z <- coef_indirect / SE_indirect

  # p-value calculation
  p_value <- 2 * (1 - pnorm(abs(Z)))

  # 신뢰구간 계산
  CI_lower <- coef_indirect - Z_alpha_half * SE_indirect
  CI_upper <- coef_indirect + Z_alpha_half * SE_indirect

  test = "Aroian_sobel_exact"
  return( dplyr::bind_cols(test = test,
                           ind_coef = round(coef_indirect, digits),
                           SE = round(SE_indirect, digits),
                           Z = round(Z, digits),
                           p_value = round(p_value, digits),
                           CI_lower = round(CI_lower, digits),
                           CI_upper = round(CI_upper, digits)))
}
#' Goodman Sobel Test generalized function
#'
#' @param a est
#' @param SE se
#' @param digits 5
#' @param conf 0.90, 0.95, 0.99, 0.999
#' @param roburst  roburst T whide correction
#' @param n sample size
#' @param k number of regressin coefs, default 2
#'
#' @return result
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
#' #'
#' # 예제 데이터
#' a <- c(0.5, 0.3, 0.4)
#' SE <- c(0.1, 0.1, 0.1)
#'
#' # 함수 실행 (기본 95% 신뢰구간)
#' goodman_sobel_test_general(a, SE)
#'
#' # 함수 실행 (90% 신뢰구간)
#' result_90 <- goodman_sobel_test_general(a, SE, conf=0.90)
#' print(result_90)
#'
#' # 함수 실행 (99% 신뢰구간)
#' result_99 <- goodman_sobel_test_general(a, SE, conf=0.99)
#' print(result_99)
#'
#' # 함수 실행 (99.9% 신뢰구간)
#' result_999 <- goodman_sobel_test_general(a, SE, conf=0.999)
#' print(result_999)
#' #'
#'
#' }
goodman_sobel_test_general <- function(a, SE, roburst=FALSE, n=NULL, k=2, digits=5, conf=0.95) {
  library(dplyr)

  if (roburst) {
    adjustment_factor <- sqrt(n / (n - k))
    SE <- SE * adjustment_factor
    cat("\n", "Roburst SE ratio(Scott & Ervin, 2000; Hinkley, 1977):", adjustment_factor, "\n")
  } else {
    SE <- SE
  }

  conf_levels <- c(0.90, 0.95, 0.99, 0.999)
  Z_values <- c(1.645, 1.96, 2.576, 3.291)

  if (!(conf %in% conf_levels)) {
    stop("conf는 0.90, 0.95, 0.99, 0.999 중 하나여야 합니다.")
  }

  Z_alpha_half <- Z_values[which(conf_levels == conf)]

  n <- length(a)

  if (n == 2) {
    term1 <- sum(sapply(1:n, function(i) {
      SE[i]^2 * prod(vapply(setdiff(1:n, i), function(j) a[j]^2, numeric(1)))
    }))

    term2 <- sum(sapply(1:(n-1), function(i) {
      sum(sapply((i+1):n, function(j) {
        SE[i]^2 * SE[j]^2
      }))
    }))

    SE_generalized <- sqrt(term1 - term2)
  } else {
    term1 <- sum(sapply(1:n, function(i) {
      SE[i]^2 * prod(vapply(setdiff(1:n, i), function(j) a[j]^2, numeric(1)))
    }))

    term2 <- sum(sapply(1:(n-1), function(i) {
      sum(sapply((i+1):n, function(j) {
        SE[i]^2 * SE[j]^2 * prod(vapply(setdiff(1:n, c(i, j)),
                                        function(k) a[k]^2, numeric(1)))
      }))
    }))

    term3 <- prod(SE^2)

    SE_generalized <- sqrt(term1 - term2 + term3)
  }

  indirect_effect <- prod(a)
  Z <- indirect_effect / SE_generalized
  p_value <- 2 * (1 - pnorm(abs(Z)))
  CI_lower <- indirect_effect - Z_alpha_half * SE_generalized
  CI_upper <- indirect_effect + Z_alpha_half * SE_generalized

  res <- dplyr::bind_cols(
    test = "Goodman_sobel_unbiased",
    ind_coef = round(indirect_effect, digits),
    SE = round(SE_generalized, digits),
    Z = round(Z, digits),
    p_value = round(p_value, digits),
    CI_lower = round(CI_lower, digits),
    CI_upper = round(CI_upper, digits)
  )

  return(res)
}




#' Sobeltest toal
#'
#' @param a est
#' @param SE se
#' @param digits 5
#' @param conf 0.90, 0.95, 0.99, 0.999
#' @param roburst  roburst T whide correction
#' @param n sample size
#' @param k number of regressin coefs, default 2
#' @param type all, res, sobel, aroian, goodman
#'
#' @return result
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
#' Sobel_test(a, b, sa, sb,"aroian")
#'
#' Sobel_test(a, b, sa, sb,"Goodman")
#' #'
#' a <- c(0.5, 0.3, 0.4)
#' SE <- c(0.1, 0.1, 0.1)
#' general_Sobel_test(a, SE)
#' general_Sobel_test(a, SE)%>%unite_ci(digits=4)


#'
#'
#' }
general_Sobel_test= function(a, SE, roburst=FALSE,n=NULL,k=2, digits = 5){

  sobeltest = sobel_test_general(a, SE, digits, roburst=roburst, n=n, k=k )
  aroian_sobel = aroian_sobel_test_general(a, SE, digits , roburst=roburst, n=n, k=k)
  Goodman_sobel = goodman_sobel_test_general(a, SE, digits, roburst=roburst, n=n, k=k )

  res= dplyr::bind_rows(sobeltest,aroian_sobel , Goodman_sobel)#%>%p_mark_sig("p_value")
  res
}

