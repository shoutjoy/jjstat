#' Perform ANOVA with emmeans and extract group letters and p-values
#'
#' This function performs ANOVA for each independent variable (IV) and extracts
#' the estimated marginal means (emmeans), post-hoc comparison group letters, and
#' p-values, based on the specified multiple comparisons adjustment method.
#'
#' ## Adjustment Methods
#' - `"none"`: No adjustment for multiple comparisons (raw p-values). ⚠️ Can inflate Type I error.
#' - `"tukey"`: Tukey's Honest Significant Difference method. Simultaneously compares all pairwise mean differences.
#' - `"bonferroni"`: Conservative method; adjusts p-values by multiplying by the number of comparisons.
#' - `"holm"`: Sequential Bonferroni procedure; less conservative and more powerful alternative.
#' - `"sidak"`: Adjustment assuming independent comparisons.
#' - `"fdr"`: Controls the False Discovery Rate (Benjamini-Hochberg procedure).
#' - `"scheffe"`: Not supported in `emmeans`; typically used for general contrasts.
#' - `"duncan"`, `"lsd"`: Not supported in `emmeans`; internally treated as `"none"`.
#'
#' @param data A data.frame containing the variables.
#' @param dv_var The name of the dependent variable (character).
#' @param iv_vars A character vector of independent variable names.
#' @param adjust Adjustment method for pairwise comparisons. One of:
#' "none", "tukey", "bonferroni", "holm", "sidak", "fdr", "scheffe".
#' @param alpha Significance level (default = 0.05).
#' @param digits Number of digits to round numeric results (default = 2).
#' @param show_group_letter Logical. Whether to show group letters (default = TRUE).
#' @param adjust_only Logical. If TRUE and adjust != "none", only adjusted p-values (adj_p) are shown (default = TRUE).
#'
#' @return A tibble with ANOVA results per IV level, group letters, and p-values.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"))
#'
#' # Using Bonferroni adjustment
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"),
#'                adjust = "bonferroni", adjust_only = FALSE)
#'
#' # Showing only adjusted p-values
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"),
#'                adjust = "holm", adjust_only = TRUE)
#'
#' # Tukey fallback warning
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"),
#'                adjust = "tukey")
#'
#' # Unsupported methods fallback
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"),
#'                adjust = "duncan")
#' }

aov_df_emmeans <- function(data,
                           dv_var,
                           iv_vars,
                           adjust = "none",
                           alpha = 0.05,
                           digits = 2,
                           show_group_letter = TRUE,
                           adjust_only = TRUE) {

  library(emmeans)
  library(multcompView)
  library(dplyr)
  library(tibble)
  library(broom)

  results <- list()

  for (iv in iv_vars) {
    formula_str <- reformulate(iv, response = dv_var)
    model <- aov(formula_str, data = data)

    emmeans_obj <- emmeans(model, specs = iv)
    emmeans_tbl <- as.data.frame(emmeans_obj)

    if (adjust == "tukey" && length(iv_vars) > 1) {
      message("⚠️  adjust = 'tukey' is only for one IV. Switching to 'sidak'.")
      adjust <- "sidak"
    }

    if (adjust %in% c("duncan", "lsd")) {
      message(paste0("⚠️  adjust = '", adjust, "' is not supported in emmeans. Switching to 'none' (Unadjusted pairwise comparisons)."))
      adjust <- "none"
    }

    comp <- pairs(emmeans_obj, adjust = adjust)
    comp_df <- as.data.frame(summary(comp))

    if (show_group_letter) {
      cld_letters <- multcomp::cld(emmeans_obj, adjust = adjust, alpha = alpha, Letters = letters, sort = FALSE)
      group_letters <- cld_letters[, c(iv, ".group")]
      colnames(group_letters) <- c("level", "POSTHOC")
    } else {
      group_letters <- data.frame(level = emmeans_tbl[[iv]], POSTHOC = NA)
    }

    tidy_aov <- tidy(model)

    base_df <- emmeans_tbl %>%
      rename(level = !!iv, Mean = emmean) %>%
      left_join(group_letters, by = "level") %>%
      mutate(
        iv = iv,
        dv = dv_var,
        df1 = tidy_aov$df[1],
        df2 = tidy_aov$df[2],
        Mean = round(Mean, digits),
        F_value = round(tidy_aov$statistic[1], digits),
        p_value = round(tidy_aov$p.value[1], digits)
      )

    if (adjust != "none" && "p.value" %in% names(comp_df)) {
      adj_p <- round(min(comp_df$p.value, na.rm = TRUE), digits)

      final <- base_df %>%
        mutate(
          adj_p = adj_p,
          sig = case_when(
            adj_p < 0.001 ~ "***",
            adj_p < 0.01 ~ "**",
            adj_p < 0.05 ~ "*",
            TRUE ~ "ns"
          )
        )

      final <- if (adjust_only) {
        final %>%
          dplyr::select(iv, dv, level, Mean, POSTHOC, df1, df2, F_value, adj_p, sig)
      } else {
        final %>%
          dplyr::select(iv, dv, level, Mean, POSTHOC, df1, df2, F_value, p_value, adj_p, sig)
      }

    } else {
      final <- base_df %>%
        mutate(
          sig = case_when(
            p_value < 0.001 ~ "***",
            p_value < 0.01 ~ "**",
            p_value < 0.05 ~ "*",
            TRUE ~ "ns"
          )
        ) %>%
        dplyr::select(iv, dv, level, Mean, POSTHOC, df1, df2, F_value, p_value, sig)
    }

    results[[iv]] <- final
  }

  if (adjust == "none") {
    cat("\n🔎 POSTHOC method used: Unadjusted pairwise comparisons\n\n")
  } else {
    cat("\n✅ POSTHOC method used:", adjust, "\n\n")
  }

  return(bind_rows(results))
}


#' Perform ANOVA with emmeans and extract group letters and p-values
#'
#' This function performs ANOVA for each independent variable (IV) and extracts
#' the estimated marginal means (emmeans), post-hoc comparison group letters, and
#' p-values, based on the specified multiple comparisons adjustment method.
#'
#' ## Adjustment Methods
#' - `"none"`: No adjustment for multiple comparisons (raw p-values). ⚠️ Can inflate Type I error.
#' - `"tukey"`: Tukey's Honest Significant Difference method. Simultaneously compares all pairwise mean differences.
#' - `"bonferroni"`: Conservative method; adjusts p-values by multiplying by the number of comparisons.
#' - `"holm"`: Sequential Bonferroni procedure; less conservative and more powerful alternative.
#' - `"sidak"`: Adjustment assuming independent comparisons.
#' - `"fdr"`: Controls the False Discovery Rate (Benjamini-Hochberg procedure).
#' - `"scheffe"`: Not supported in `emmeans`; typically used for general contrasts.
#' - `"duncan"`, `"lsd"`: Not supported in `emmeans`; internally treated as `"none"`.
#'
#' @param data A data.frame containing the variables.
#' @param dv_var The name of the dependent variable (character).
#' @param iv_vars A character vector of independent variable names.
#' @param adjust Adjustment method for pairwise comparisons. One of:
#' "none", "tukey", "bonferroni", "holm", "sidak", "fdr", "scheffe".
#' @param alpha Significance level (default = 0.05).
#' @param digits Number of digits to round numeric results (default = 2).
#' @param show_group_letter Logical. Whether to show group letters (default = TRUE).
#' @param adjust_only Logical. If TRUE and adjust != "none", only adjusted p-values (adj_p) are shown (default = TRUE).
#'
#' @return A tibble with ANOVA results per IV level, group letters, and p-values.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"))
#'
#' # Using Bonferroni adjustment
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"),
#'                adjust = "bonferroni", adjust_only = FALSE)
#'
#' # Showing only adjusted p-values
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"),
#'                adjust = "holm", adjust_only = TRUE)
#'
#' # Tukey fallback warning
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"),
#'                adjust = "tukey")
#'
#' # Unsupported methods fallback
#' aov_df_emmeans(data = neulbom_p2_stat2,
#'                dv_var = "relation",
#'                iv_vars = c("gender", "age", "child"),
#'                adjust = "duncan")
#' }

aov_df2 <- function(data,
                           dv_var,
                           iv_vars,
                           adjust = "none",
                           alpha = 0.05,
                           digits = 2,
                           show_group_letter = TRUE,
                           adjust_only = TRUE) {

  library(emmeans)
  library(multcompView)
  library(dplyr)
  library(tibble)
  library(broom)

  results <- list()

  for (iv in iv_vars) {
    formula_str <- reformulate(iv, response = dv_var)
    model <- aov(formula_str, data = data)

    emmeans_obj <- emmeans(model, specs = iv)
    emmeans_tbl <- as.data.frame(emmeans_obj)

    if (adjust == "tukey" && length(iv_vars) > 1) {
      message("⚠️  adjust = 'tukey' is only for one IV. Switching to 'sidak'.")
      adjust <- "sidak"
    }

    if (adjust %in% c("duncan", "lsd")) {
      message(paste0("⚠️  adjust = '", adjust, "' is not supported in emmeans. Switching to 'none' (Unadjusted pairwise comparisons)."))
      adjust <- "none"
    }

    comp <- pairs(emmeans_obj, adjust = adjust)
    comp_df <- as.data.frame(summary(comp))

    if (show_group_letter) {
      cld_letters <- multcomp::cld(emmeans_obj, adjust = adjust, alpha = alpha, Letters = letters, sort = FALSE)
      group_letters <- cld_letters[, c(iv, ".group")]
      colnames(group_letters) <- c("level", "POSTHOC")
    } else {
      group_letters <- data.frame(level = emmeans_tbl[[iv]], POSTHOC = NA)
    }

    tidy_aov <- tidy(model)

    base_df <- emmeans_tbl %>%
      rename(level = !!iv, Mean = emmean) %>%
      left_join(group_letters, by = "level") %>%
      mutate(
        iv = iv,
        dv = dv_var,
        df1 = tidy_aov$df[1],
        df2 = tidy_aov$df[2],
        Mean = round(Mean, digits),
        F_value = round(tidy_aov$statistic[1], digits),
        p_value = round(tidy_aov$p.value[1], digits)
      )

    if (adjust != "none" && "p.value" %in% names(comp_df)) {
      adj_p <- round(min(comp_df$p.value, na.rm = TRUE), digits)

      final <- base_df %>%
        mutate(
          adj_p = adj_p,
          sig = case_when(
            adj_p < 0.001 ~ "***",
            adj_p < 0.01 ~ "**",
            adj_p < 0.05 ~ "*",
            TRUE ~ "ns"
          )
        )

      final <- if (adjust_only) {
        final %>%
          dplyr::select(iv, dv, level, Mean, POSTHOC, df1, df2, F_value, adj_p, sig)
      } else {
        final %>%
          dplyr::select(iv, dv, level, Mean, POSTHOC, df1, df2, F_value, p_value, adj_p, sig)
      }

    } else {
      final <- base_df %>%
        mutate(
          sig = case_when(
            p_value < 0.001 ~ "***",
            p_value < 0.01 ~ "**",
            p_value < 0.05 ~ "*",
            TRUE ~ "ns"
          )
        ) %>%
        dplyr::select(iv, dv, level, Mean, POSTHOC, df1, df2, F_value, p_value, sig)
    }

    results[[iv]] <- final
  }

  if (adjust == "none") {
    cat("\n🔎 POSTHOC method used: Unadjusted pairwise comparisons\n\n")
  } else {
    cat("\n✅ POSTHOC method used:", adjust, "\n\n")
  }

  return(bind_rows(results))
}

