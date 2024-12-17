

#' Pairwise Wald Test for LM Coefficients (Auto Input Detection)
#'
#' @param input_data Regression result: lm object, summary output, or tidy dataframe.
#' @param raw Logical. If TRUE, returns the raw result with significance marks.
#'            If FALSE, combines relevant columns for a simplified output. Default is FALSE.
#' @return A data frame with pairwise comparisons of coefficients using Wald test.
#' @export
#'
#' @examples
#' model <- lm(mpg ~ hp + wt + qsec + drat, data = mtcars)
#' wald_test_lm(model) # lm object
#'
#' summary_model <- summary(model)
#' wald_test_lm(summary_model) # summary object
#'
#' tidy_model <- broom::tidy(model)
#' wald_test_lm(tidy_model) # tidy dataframe
#'
#' wald_test_lm(model, raw = TRUE) # Raw output

wald_test_lm <- function(input_data, raw = FALSE) {
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(broom)

  # 자동 입력 타입 판별
  if (inherits(input_data, "lm")) {
    message("Detected 'lm' object as input. The results are as follows (Park JH, 2024).")
    coef_summary <- summary(input_data)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column("Variable") %>%
      filter(Variable != "(Intercept)") %>%
      rename(Estimate = Estimate, Std_Error = `Std. Error`) %>%
      select(Variable, Estimate, Std_Error)

  } else if (is.list(input_data) && "coefficients" %in% names(input_data)) {
    message("Detected 'summary' object as input. The results are as follows (Park JH, 2024).")
    coef_summary <- input_data$coefficients %>%
      as.data.frame() %>%
      rownames_to_column("Variable") %>%
      filter(Variable != "(Intercept)") %>%
      rename(Estimate = Estimate, Std_Error = `Std. Error`) %>%
      select(Variable, Estimate, Std_Error)

  } else if (is.data.frame(input_data) && all(c("term", "estimate", "std.error") %in% colnames(input_data))) {
    message("Detected 'tidy' dataframe as input. The results are as follows (Park JH, 2024).")
    coef_summary <- input_data %>%
      filter(term != "(Intercept)") %>%
      rename(Variable = term, Estimate = estimate, Std_Error = std.error) %>%
      select(Variable, Estimate, Std_Error)

  } else {
    stop("Input type not recognized. Please provide an 'lm', 'summary', or 'tidy' object.")
  }

  # Create all pairwise comparisons
  comparisons <- combn(coef_summary$Variable, 2, simplify = FALSE)

  # Run Wald tests for each pair
  results <- map_dfr(comparisons, function(pair) {
    var1 <- pair[1]
    var2 <- pair[2]

    b1 <- coef_summary %>% filter(Variable == var1) %>% pull(Estimate)
    se1 <- coef_summary %>% filter(Variable == var1) %>% pull(Std_Error)
    b2 <- coef_summary %>% filter(Variable == var2) %>% pull(Estimate)
    se2 <- coef_summary %>% filter(Variable == var2) %>% pull(Std_Error)

    # Calculate Wald statistics
    z <- (b1 - b2) / sqrt(se1^2 + se2^2)
    p <- 2 * (1 - pnorm(abs(z)))

    # Return results as a data frame
    tibble(
      var_comparison = paste0(var1, " - ", var2),
      est1 = round(b1, 3),
      se1 = round(se1, 3),
      est2 = round(b2, 3),
      se2 = round(se2, 3),
      Z = round(z, 3),
      p_value = round(p, 4)
    )
  })

  # Significance marking function
  p_mark_sig <- function(data, p_col, ns = "ns") {
    data <- data %>%
      mutate(sig = case_when(
        !!sym(p_col) < 0.001 ~ "***",
        !!sym(p_col) < 0.01 ~ "**",
        !!sym(p_col) < 0.05 ~ "*",
        TRUE ~ ns
      ))
    return(data)
  }

  # Final result based on 'raw' parameter
  if (raw) {
    return(results %>% p_mark_sig("p_value", ns = ""))
  } else {
    return(results %>%
             p_mark_sig("p_value", ns = "") %>%
             unite("Result", c(Z, sig), sep = " "))
  }
}

#
#
# wald_test_lm <- function(input_data, raw =FALSE) {
#   library(dplyr)
#   library(tidyr)
#   library(purrr)
#   library(broom)
#
#   # 자동 입력 타입 판별
#   if (inherits(input_data, "lm")) {
#     message("Detected 'lm' object as input. The results are as follows(Park JH, 2024)")
#     coef_summary <- summary(input_data)$coefficients %>%
#       as.data.frame() %>%
#       rownames_to_column("Variable") %>%
#       filter(Variable != "(Intercept)") %>%
#       rename(Estimate = Estimate, Std_Error = `Std. Error`) %>%
#       select(Variable, Estimate, Std_Error)
#
#   } else if (is.list(input_data) && "coefficients" %in% names(input_data)) {
#     message("Detected 'summary' object as input. The results are as follows(Park JH, 2024)")
#     coef_summary <- input_data$coefficients %>%
#       as.data.frame() %>%
#       rownames_to_column("Variable") %>%
#       filter(Variable != "(Intercept)") %>%
#       rename(Estimate = Estimate, Std_Error = `Std. Error`) %>%
#       select(Variable, Estimate, Std_Error)
#
#   } else if (is.data.frame(input_data) && all(c("term", "estimate", "std.error") %in% colnames(input_data))) {
#     message("Detected 'tidy' dataframe as input. The results are as follows(Park JH, 2024)")
#     coef_summary <- input_data %>%
#       filter(term != "(Intercept)") %>%
#       rename(Variable = term, Estimate = estimate, Std_Error = std.error) %>%
#       select(Variable, Estimate, Std_Error)
#
#   } else {
#     stop("Input type not recognized. Please provide an 'lm', 'summary', or 'tidy' object.")
#   }
#
#   # Create all pairwise comparisons
#   comparisons <- combn(coef_summary$Variable, 2, simplify = FALSE)
#
#   # Run Wald tests for each pair
#   results <- map_dfr(comparisons, function(pair) {
#     var1 <- pair[1]
#     var2 <- pair[2]
#
#     b1 <- coef_summary %>% filter(Variable == var1) %>% pull(Estimate)
#     se1 <- coef_summary %>% filter(Variable == var1) %>% pull(Std_Error)
#     b2 <- coef_summary %>% filter(Variable == var2) %>% pull(Estimate)
#     se2 <- coef_summary %>% filter(Variable == var2) %>% pull(Std_Error)
#
#     # Calculate Wald statistics
#     z <- (b1 - b2) / sqrt(se1^2 + se2^2)
#     p <- 2 * (1 - pnorm(abs(z)))
#
#     # Return results as a data frame
#     tibble(
#       var_comparison = paste0(var1, " - ", var2),
#       est1 = round(b1, 3),
#       se1 = round(se1, 3),
#       est2 = round(b2, 3),
#       se2 = round(se2, 3),
#       Z = round(z, 3),
#       p_value = round(p, 4)
#     )
#   })
#
#
#   if(raw){
#     return( results%>% p_mark_sig("p_value", ns="") )
#   }else{
#     return(results %>% p_mark_sig("p_value", ns="") %>%  Unite(6, 8) )
#   }
# }
#

