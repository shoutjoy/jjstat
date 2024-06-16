#' sobel_test_extend
#'
#' @param coefficients coefficients c()
#' @param se_values se
#' @param show TRUE
#' @param sobel aroian(default), sobel, goodman
#'
#' @return seobel resutl
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' # Example usage
#' sdata <- data.frame(
#'   paths = c("IMAG -> EXPE", "IMAG -> SAT",
#'               "IMAG -> LOY", "EXPE -> QUAL", "EXPE -> VAL", "EXPE -> SAT",
#'             "QUAL -> VAL", "QUAL -> SAT", "VAL -> SAT", "SAT -> LOY"),
#'   Original = c(0.578959128, 0.200724200, 0.275149576,
#'               0.848344408, 0.105477650, -0.002753995,
#'                0.676655529, 0.122144566, 0.589330650, 0.495479322),
#'   Mean.Boot = c(0.58232765, 0.21209094, 0.27486564,
#'                 0.84793899, 0.10992051, -0.01008222,
#'                 0.67780527, 0.15132543, 0.56312717, 0.50757755),
#'   Std.Error = c(0.04358293, 0.06056988, 0.07729000,
#'                  0.01864573, 0.06968826, 0.06507903,
#'                 0.07760527, 0.08440236, 0.08723293, 0.07856694),
#'   perc.025 = c(0.497644719, 0.107561558, 0.140641502,
#'                 0.815794494, -0.025566471, -0.147788188,
#'                0.519959683, -0.008629516, 0.392664443, 0.351774130),
#'   perc.975 = c(0.6646198, 0.3275312, 0.4161131,
#'                 0.8828875, 0.2545785, 0.1086856,
#'                0.8166486, 0.2991045, 0.7012494, 0.6556333)
#' )
#'
#'
#'
#' sobel_test_extend(coefficients= c("IMAG -> EXPE" = 0.578959128,
#'                                   "EXPE -> SAT" = -0.002753995,
#'                                   "SAT -> LOY" = 0.495479322),
#'                   se_values = c(se1 = 0.04358293,
#'                                 se2 = 0.06507903,
#'                                 se3 = 0.0785669))
#' # Example usage
#' coefficients1 <- c("IMAG -> EXPE" = 0.578959128,
#'                   "EXPE -> SAT" = -0.002753995, "SAT -> LOY" = 0.495479322)
#' se_values1 <- c(se1 = 0.04358293, se2 = 0.06507903, se3 = 0.0785669)
#'
#' sobel_test_extend(coefficients1, se_values1)
#'
#'
#' # Test with unnamed values
#' coefficients2 <- c(0.578959128, -0.002753995, 0.495479322)
#' se_values2 <- c(0.04358293, 0.06507903, 0.0785669)
#'
#' sobel_test_extend(coefficients2, se_values2)
#'
#'
#' # Test with est and se named values
#' coefficients3 <- c(imag2expe = 0.578959128,
#'                   expe2sat = -0.002753995, sat2loy = 0.495479322)
#' se_values3 <- c(se1 = 0.04358293, se2 = 0.06507903, se3 = 0.0785669)
#'
#' sobel_test_extend(coefficients3, se_values3)
#'

#'
#'
#' }
#'
#'
sobel_test_extend <- function(coefficients, se_values, show=TRUE, sobel="aroian") {
  n <- length(coefficients)

  # Check if the coefficients and se_values have names
  if (is.null(names(coefficients))) {
    names(coefficients) <- paste0("est", 1:n)
  }

  if (is.null(names(se_values))) {
    names(se_values) <- paste0("se", 1:n)
  }

  # Remove duplicates in the path
  path_names <- names(coefficients)
  unique_path <- unique(unlist(strsplit(path_names, " -> ")))
  effect_path <- paste(unique_path, collapse = " -> ")

  if (n == 2) {
    # 특별한 처리: 값이 두 개일 경우
    a <- coefficients[1]
    b <- coefficients[2]
    se_a <- se_values[1]
    se_b <- se_values[2]

    if (sobel == "sobel") {
      sobel_se <- sqrt(a^2 * se_b^2 + b^2 * se_a^2)
    } else if (sobel == "aroian") {
      sobel_se <- sqrt(a^2 * se_b^2 + b^2 * se_a^2 + se_a^2 * se_b^2)
    } else if (sobel == "goodman") {
      sobel_se <- sqrt(a^2 * se_b^2 + b^2 * se_a^2 - se_a^2 * se_b^2)
    }

    indirect_effect <- a * b
    z_value <- indirect_effect / sobel_se
    p_value <- 2 * (1 - stats::pnorm(abs(z_value)))

    result <- list(
      paths = effect_path,
      ind_effect = indirect_effect,
      sobel_se = sobel_se,
      z_value = z_value,
      p_value = p_value
    ) %>% p_mark_sig("p_value")
  } else {
    # Initialize the sum for Sobel method
    sobel_se_sum <- 0
    # Initialize the sum for Aroian method
    aroian_se_sum <- 0
    # Initialize the sum for Goodman method
    goodman_se_sum <- 0

    for (i in 1:n) {
      prod_a_except_i <- prod(coefficients[-i]) # i번째 요소를 제외한 모든 a의 곱
      sobel_se_sum <- sobel_se_sum + (prod_a_except_i^2 * se_values[i]^2)
    }

    aroian_se_sum <- sobel_se_sum + prod(se_values^2)
    goodman_se_sum <- sobel_se_sum - prod(se_values^2)

    # Calculate the SE based on the sobel option
    if (sobel == "sobel") {
      sobel_se <- sqrt(sobel_se_sum)
    } else if (sobel == "aroian") {
      sobel_se <- sqrt(aroian_se_sum)
    } else if (sobel == "goodman") {
      sobel_se <- sqrt(goodman_se_sum)
    }

    # Calculate the indirect effect
    indirect_effect <- prod(coefficients)
    # Calculate the z-value
    z_value <- indirect_effect / sobel_se
    # Calculate the p-value
    p_value <- 2 * (1 - stats::pnorm(abs(z_value)))
    # Return results as a list
    result <- list(
      paths = effect_path,
      ind_effect = indirect_effect,
      sobel_se = sobel_se,
      z_value = z_value,
      p_value = p_value
    ) %>% p_mark_sig("p_value")
  }
  sig = result$sig

  if (show) {
    # Print the indirect effect information
    cat("\n", "Indirect Effect(", sobel, "): ", "\n") # effect_path,
    for (i in 1:n) {
      cat("    ", names(coefficients)[i], ", est = ", round(coefficients[i], 3),
          ", se = ", round(se_values[i], 3), "\n", sep = "")
    }
    cat("   ", effect_path, ": est=",
        round(indirect_effect, 3), ", Z=",
        round(z_value, 3), ", se=",
        round(sobel_se, 5), ", p=",
        paste0(ifelse(p_value < 0.001, " < .001", round(p_value, 5))),
        "(", sig, ")", sep = "")
  }
  cat("\n")
  return(result %>% dplyr::as_tibble())
}



#' sobel_test_extend
#'
#' @param coefficients coefficients c()
#' @param se_values se
#' @param show TRUE
#' @param sobel aroina, sobel, goodman
#'
#' @return seobel resutl
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' # Example usage
#' sdata <- data.frame(
#'   paths = c("IMAG -> EXPE", "IMAG -> SAT",
#'               "IMAG -> LOY", "EXPE -> QUAL", "EXPE -> VAL", "EXPE -> SAT",
#'             "QUAL -> VAL", "QUAL -> SAT", "VAL -> SAT", "SAT -> LOY"),
#'   Original = c(0.578959128, 0.200724200, 0.275149576,
#'               0.848344408, 0.105477650, -0.002753995,
#'                0.676655529, 0.122144566, 0.589330650, 0.495479322),
#'   Mean.Boot = c(0.58232765, 0.21209094, 0.27486564,
#'                 0.84793899, 0.10992051, -0.01008222,
#'                 0.67780527, 0.15132543, 0.56312717, 0.50757755),
#'   Std.Error = c(0.04358293, 0.06056988, 0.07729000,
#'                  0.01864573, 0.06968826, 0.06507903,
#'                 0.07760527, 0.08440236, 0.08723293, 0.07856694),
#'   perc.025 = c(0.497644719, 0.107561558, 0.140641502,
#'                 0.815794494, -0.025566471, -0.147788188,
#'                0.519959683, -0.008629516, 0.392664443, 0.351774130),
#'   perc.975 = c(0.6646198, 0.3275312, 0.4161131,
#'                 0.8828875, 0.2545785, 0.1086856,
#'                0.8166486, 0.2991045, 0.7012494, 0.6556333)
#' )
#'
#'
#'
#' sobel_test_serial(coefficients= c("IMAG -> EXPE" = 0.578959128,
#'                                   "EXPE -> SAT" = -0.002753995,
#'                                   "SAT -> LOY" = 0.495479322),
#'                   se_values = c(se1 = 0.04358293,
#'                                 se2 = 0.06507903,
#'                                 se3 = 0.0785669))
#' # Example usage
#' coefficients1 <- c("IMAG -> EXPE" = 0.578959128,
#'                   "EXPE -> SAT" = -0.002753995, "SAT -> LOY" = 0.495479322)
#' se_values1 <- c(se1 = 0.04358293, se2 = 0.06507903, se3 = 0.0785669)
#'
#' sobel_test_serial(coefficients1, se_values1)
#'
#'
#' # Test with unnamed values
#' coefficients2 <- c(0.578959128, -0.002753995, 0.495479322)
#' se_values2 <- c(0.04358293, 0.06507903, 0.0785669)
#'
#' sobel_test_serial(coefficients2, se_values2)
#'
#'
#' # Test with est and se named values
#' coefficients3 <- c(imag2expe = 0.578959128,
#'                   expe2sat = -0.002753995, sat2loy = 0.495479322)
#' se_values3 <- c(se1 = 0.04358293, se2 = 0.06507903, se3 = 0.0785669)
#'
#' sobel_test_serial(coefficients3, se_values3)
#'

#'
#'
#' }
#'
#'
sobel_test_serial <- function(coefficients, se_values, show=TRUE, sobel="aroian") {
  n <- length(coefficients)

  # Check if the coefficients and se_values have names
  if (is.null(names(coefficients))) {
    names(coefficients) <- paste0("est", 1:n)
  }

  if (is.null(names(se_values))) {
    names(se_values) <- paste0("se", 1:n)
  }

  # Remove duplicates in the path
  path_names <- names(coefficients)
  unique_path <- unique(unlist(strsplit(path_names, " -> ")))
  effect_path <- paste(unique_path, collapse = " -> ")

  if (n == 2) {
    # 특별한 처리: 값이 두 개일 경우
    a <- coefficients[1]
    b <- coefficients[2]
    se_a <- se_values[1]
    se_b <- se_values[2]

    if (sobel == "sobel") {
      sobel_se <- sqrt(a^2 * se_b^2 + b^2 * se_a^2)
    } else if (sobel == "aroian") {
      sobel_se <- sqrt(a^2 * se_b^2 + b^2 * se_a^2 + se_a^2 * se_b^2)
    } else if (sobel == "goodman") {
      sobel_se <- sqrt(a^2 * se_b^2 + b^2 * se_a^2 - se_a^2 * se_b^2)
    }

    indirect_effect <- a * b
    z_value <- indirect_effect / sobel_se
    p_value <- 2 * (1 - stats::pnorm(abs(z_value)))

    result <- list(
      paths = effect_path,
      ind_effect = indirect_effect,
      sobel_se = sobel_se,
      z_value = z_value,
      p_value = p_value
    ) %>% p_mark_sig("p_value")
  } else {
    # Initialize the sum for Sobel method
    sobel_se_sum <- 0
    # Initialize the sum for Aroian method
    aroian_se_sum <- 0
    # Initialize the sum for Goodman method
    goodman_se_sum <- 0

    for (i in 1:n) {
      prod_a_except_i <- prod(coefficients[-i]) # i번째 요소를 제외한 모든 a의 곱
      sobel_se_sum <- sobel_se_sum + (prod_a_except_i^2 * se_values[i]^2)
    }

    aroian_se_sum <- sobel_se_sum + prod(se_values^2)
    goodman_se_sum <- sobel_se_sum - prod(se_values^2)

    # Calculate the SE based on the sobel option
    if (sobel == "sobel") {
      sobel_se <- sqrt(sobel_se_sum)
    } else if (sobel == "aroian") {
      sobel_se <- sqrt(aroian_se_sum)
    } else if (sobel == "goodman") {
      sobel_se <- sqrt(goodman_se_sum)
    }

    # Calculate the indirect effect
    indirect_effect <- prod(coefficients)
    # Calculate the z-value
    z_value <- indirect_effect / sobel_se
    # Calculate the p-value
    p_value <- 2 * (1 - stats::pnorm(abs(z_value)))
    # Return results as a list
    result <- list(
      paths = effect_path,
      ind_effect = indirect_effect,
      sobel_se = sobel_se,
      z_value = z_value,
      p_value = p_value
    ) %>% p_mark_sig("p_value")
  }
  sig = result$sig

  if (show) {
    # Print the indirect effect information
    cat("\n", "Indirect Effect(", sobel, "): ", "\n") # effect_path,
    for (i in 1:n) {
      cat("    ", names(coefficients)[i], ", est = ", round(coefficients[i], 3),
          ", se = ", round(se_values[i], 3), "\n", sep = "")
    }
    cat("   ", effect_path, ": est=",
        round(indirect_effect, 3), ", Z=",
        round(z_value, 3), ", se=",
        round(sobel_se, 5), ", p=",
        paste0(ifelse(p_value < 0.001, " < .001", round(p_value, 5))),
        "(", sig, ")", sep = "")
  }
  cat("\n")
  return(result %>% dplyr::as_tibble())
}
