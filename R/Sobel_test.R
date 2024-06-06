
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
#' Sobel_test(a, b, sa, sb, "all")$Sobel
#' Sobel_test(a, b, sa, sb, "Sobel")
#' Sobel_test(a, b, sa, sb,"all")$Aroian
#' Sobel_test(a, b, sa, sb,"Aroian")
#' Sobel_test(a, b, sa, sb,"all")$Goodman
#' Sobel_test(a, b, sa, sb,"Goodman")
#' }
Sobel_test <- function(a, b, sa, sb, type="Sobel") {
  # Sobel test equation
  sobel_est = a*b
  sobel_se <- sqrt(b^2 * sa^2 + a^2 * sb^2)
  sobel_z <- a * b / sobel_se
  sobel_p <- 2 * (1 - pnorm(abs(sobel_z)))

  # Aroian test equation
  aroian_est = a*b
  aroian_se <- sqrt(b^2 * sa^2 + a^2 * sb^2 + sa^2 * sb^2)
  aroian_z <- a * b / aroian_se
  aroian_p <- 2 * (1 - pnorm(abs(aroian_z)))

  # Goodman test equation
  goodman_est = a*b
  goodman_se <- sqrt(b^2 * sa^2 + a^2 * sb^2 - sa^2 * sb^2)
  goodman_z <- a * b / goodman_se
  goodman_p <- 2 * (1 - pnorm(abs(goodman_z)))

  # Return results as a list
  result <- list(
    Sobel = list(est_ab = sobel_est,  se = sobel_se,z = sobel_z, p.value = sobel_p),
    Aroian = list(est_ab = aroian_est,  se = aroian_se,z = aroian_z, p.value = aroian_p),
    Goodman = list(est_ab = goodman_est, se = goodman_se, z = goodman_z, p.value = goodman_p)
  )

  switch(type, all= result,
         Sobel = result$Sobel %>% do.call(what=bind_rows),
         Aroian = result$Aroian%>% do.call(what=bind_rows),
         Goodman = result$Goodman%>% do.call(what=bind_rows)   )

}
