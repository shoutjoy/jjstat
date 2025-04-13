

#' Generate an AOV results table for multiple independent variables
#'
#' @param data data.frame
#' @param dv_var dv
#' @param iv_vars c(iv1, iv2....)
#' @param unite_F Fvalue unite
#' @param unite_p pvalue unite
#' @param sig 'sig = T' is add star
#' @param grp_mean 'grooup mean remove
#' @param posthoc 'tukey, scheffe, duncan, lsd
#' @param p.adj  p.adj=c("none","holm","hommel", "hochberg", "bonferroni", "BH", "BY", "fdr") only lsd test
#'
#' @return table
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mtcars <- mtcars%>%as_trt("cyl", "gear", "carb")
#' aov_table(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear", "carb"))
#'
#' aov_table(data = mtcars, dv_var = "mpg",
#' iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE)
#' aov_table(data = mtcars, dv_var = "mpg",
#' iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE) %>%
#' aov_table_apaOP
#'
#' aov_table(data = mtcars, dv_var = "mpg",
#'  iv_vars = c("cyl", "gear", "carb"), grp_mean = TRUE)
#'
#' aov_table(data = mtcars, dv_var = "mpg",
#'  iv_vars = c("cyl", "gear", "carb"), grp_mean = TRUE, unite_F = TRUE)
#'
#' aov_table(data = mtcars, dv_var = "mpg",
#'  iv_vars = c("cyl", "gear", "carb"),
#'   grp_mean = TRUE, unite_F = TRUE) %>% aov_rename()
#'   markdown_table()
#' }
#'
#'
#'
aov_table <- function(data,
                      dv_var = NULL,
                      iv_vars = NULL,
                      grp_mean = TRUE,
                      unite_p = FALSE,
                      unite_F = FALSE,
                      digits = 2,
                      posthoc = "lsd",
                      p.adj = "bonferroni",
                      sig = FALSE
) {

  if (!is.data.frame(data)) {
    stop("you need input data.frame")
  }

  if (is.null(dv_var) | is.null(iv_vars)) {
    stop("Please provide both dv_var and iv_vars.")
  }

  cat("\naov_df & aov_table Result\n",
      "grp_mean is TRUE -> add grp_mean / grp_mean is FALSE -> only levels \n\n")

  data <- as_trt(data, iv_vars)  # factor treatment

  # Initialize a data frame to store the results
  result_df <- data.frame(Indv_Variable = character(0),
                          F_value = numeric(0),
                          p_value = numeric(0))

  # Perform an ANOVA analysis for each independent variable
  for (iv in iv_vars) {
    # ANOVA
    anova_result <- aov(formula(paste(dv_var, "~", iv)), data = data)

    # Group-by mean calculation using dplyr to ensure correct matching
    meandata <- data %>%
      group_by_at(iv) %>%
      summarise(mean_value = mean(!!sym(dv_var), na.rm = TRUE))

    # Posthoc tests
    lsd = agricolae::LSD.test(anova_result, iv, console = FALSE, p.adj = p.adj)
    duncan = agricolae::duncan.test(anova_result, iv, console = FALSE)
    scheffe = agricolae::scheffe.test(anova_result, iv, console = FALSE)
    tukey = agricolae::HSD.test(anova_result, iv, console = FALSE)

    posthoc_groups_lsd <- lsd$groups[, 2]
    posthoc_groups_duncan <- duncan$groups[, 2]
    posthoc_groups_scheffe <- scheffe$groups[, 2]
    posthoc_groups_tukey <- tukey$groups[, 2]

    group_levels <- rownames(lsd$groups)

    # Reorder meandata to match the posthoc group order
    meandata <- meandata %>%
      mutate(group = !!sym(iv)) %>%
      arrange(match(group, group_levels))

    # Extract ANOVA result statistics
    tidy_result <- broom::tidy(anova_result)

    # Create result dataframe
    result_df <- rbind(result_df,
                       data.frame(
                         iv = iv,
                         dv = dv_var,
                         level = group_levels,  # Add levels to the result
                         Mean = meandata$mean_value,  # Matching means with groups
                         posthoc_lsd = posthoc_groups_lsd,
                         posthoc_duncan = posthoc_groups_duncan,
                         posthoc_scheffe = posthoc_groups_scheffe,
                         posthoc_tukey = posthoc_groups_tukey,
                         df1 = tidy_result$df[1],
                         df2 = tidy_result$df[2],
                         F_value = tidy_result$statistic[1],
                         p_value = tidy_result$p.value[1]
                       ))
  }

  # Posthoc selection and column renaming
  if (posthoc == "lsd") {
    result_df <- result_df %>%
      dplyr::select(-posthoc_duncan, -posthoc_scheffe, -posthoc_tukey) %>%
      dplyr::rename(POSTHOC = posthoc_lsd)
    cat("   Using The Least Significant Difference (LSD) posthoc \n\n")
  } else if (posthoc == "duncan") {
    result_df <- result_df %>%
      dplyr::select(-posthoc_lsd, -posthoc_scheffe, -posthoc_tukey) %>%
      dplyr::rename(POSTHOC = posthoc_duncan)
    cat("   Using Duncan's posthoc \n\n")
  } else if (posthoc == "scheffe") {
    result_df <- result_df %>%
      dplyr::select(-posthoc_lsd, -posthoc_duncan, -posthoc_tukey) %>%
      dplyr::rename(POSTHOC = posthoc_scheffe)
    cat("   Using Scheffe's posthoc \n\n")
  } else if (posthoc == "tukey") {
    result_df <- result_df %>%
      dplyr::select(-posthoc_lsd, -posthoc_duncan, -posthoc_scheffe) %>%
      dplyr::rename(POSTHOC = posthoc_tukey)
    cat("   Using Tukey's HSD posthoc \n\n")
  }

  # Group Mean Inclusion
  if (grp_mean) {
    result_df <- result_df %>%
      tibble::tibble()

    result_df$p_value <- as.numeric(format(result_df$p_value, nsmall = 3))

    if (sig) {
      result_df <- result_df %>% mutate(sig = add_sig(result_df$p_value))
    }

  } else {
    result_df <- result_df %>%
      dplyr::select(-Mean)

    result_df <- result_df %>%
      tibble::tibble()

    result_df$p_value <- as.numeric(format(result_df$p_value, nsmall = 3))

    if (sig) {
      result_df <- result_df %>% mutate(sig = add_sig(result_df$p_value))
    }
  }

  # Unite F-value and p-value if needed
  if (unite_F) {
    result_df <- result_df %>%
      mutate(sig = ifelse(p_value < 0.001, "***",
                          ifelse(p_value < 0.01, "**",
                                 ifelse(p_value < 0.05, "*", "")))) %>%
      unite(F_value, F_value, sig, sep = "")
  } else if (unite_p) {
    result_df <- result_df %>%
      mutate(sig = ifelse(p_value < 0.001, "***",
                          ifelse(p_value < 0.01, "**",
                                 ifelse(p_value < 0.05, "*", "")))) %>%
      unite(p_value, p_value, sig, sep = "")
  }

  return(result_df)
}


#' aov_rename
#'
#' @param data data
#'
#' @return result
#' @export
#'
aov_rename = function(data){
  data = data %>%rename(
    독립변수 = iv, 종속변수= dv,
    수준=level, 평균 = Mean,
    사후분석= POSTHOC,
    F = F_value, p = p_value
  )
  data.frame(data)
}






