

#' Perform ANOVA and emmeans-based posthoc comparisons (data frame or aov object)
#'
#' This function supports either a data.frame with specified dv/iv variables, or an `aov` model object via piping.
#'
#' @param data a data.frame or an `aov` object.
#' @param dv_var (optional) dependent variable name as character.
#' @param iv_vars (optional) independent variable names as character vector.
#' @param adjust method for p-value adjustment. Options: "none", "tukey", "bonferroni", "holm", "sidak", "fdr".
#' @param alpha significance level for group lettering. Default 0.05.
#' @param digits numeric rounding digits. Default is 2.
#' @param show_group_letter logical, show grouping letters. Default TRUE.
#' @param adjust_only logical, if TRUE show only adjusted p-values. Default TRUE.
#'
#' @return A tibble containing ANOVA and posthoc results.
#' @export
#'
#' @examples
#' \dontrun{
#' aov_post3(data = iris, dv_var = "Sepal.Length", iv_vars = "Species")
#' aov(Sepal.Length ~ Species, data = iris) %>% aov_post3()
#' }
aov_post <- function(data,
                     dv_var = NULL,
                     iv_vars = NULL,
                     posthoc = "none",  # <- 이름 변경됨!
                     alpha = 0.05,
                     digits = 2,
                     show_group_letter = TRUE,
                     adjust_only = TRUE) {

  # 필요 패키지 로드
  stop_if_missing <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(paste0("Package '", pkg, "' is required."))
  }
  purrr::walk(c("emmeans", "multcomp", "dplyr", "broom", "tibble"), stop_if_missing)

  # aov 객체일 경우 자동 추출
  if (inherits(data, "aov")) {
    model <- data
    form_chr <- as.character(formula(model))
    dv_var <- form_chr[2]
    iv_vars <- form_chr[3] %>% strsplit(" \\+ ") %>% unlist()
    data <- model$model
  }

  if (is.null(dv_var) || is.null(iv_vars)) {
    stop("❌ When passing a data frame, 'dv_var' and 'iv_vars' must be specified.")
  }

  posthoc <- tolower(posthoc)
  supported_methods <- c("none", "tukey", "bonferroni", "holm", "sidak", "fdr", "scheffe")
  if (!posthoc %in% supported_methods) {
    stop(paste0("❌ Unsupported posthoc method: '", posthoc, "'. Choose from: ", paste(supported_methods, collapse = ", ")))
  }

  results <- list()

  for (iv in iv_vars) {
    formula_str <- reformulate(iv, response = dv_var)
    model <- aov(formula_str, data = data)
    emmeans_obj <- emmeans::emmeans(model, specs = iv)
    emmeans_tbl <- as.data.frame(emmeans_obj)

    if (posthoc == "tukey" && length(iv_vars) > 1) {
      message("⚠️  posthoc = 'tukey' is only for one IV. Switching to 'sidak'.")
      posthoc <- "sidak"
    }

    # 'scheffe'는 특별한 처리
    if (posthoc == "scheffe") {
      comp <- emmeans::contrast(emmeans_obj, method = "scheffe")
      comp_df <- as.data.frame(summary(comp))
    } else {
      comp <- pairs(emmeans_obj, adjust = posthoc)
      comp_df <- as.data.frame(summary(comp))
    }

    # 그룹 레터 생성
    if (show_group_letter) {
      if (posthoc == "scheffe") {
        cld_letters <- multcomp::cld(comp, alpha = alpha, Letters = letters, sort = FALSE)
      } else {
        cld_letters <- multcomp::cld(emmeans_obj, adjust = posthoc, alpha = alpha, Letters = letters, sort = FALSE)
      }
      group_letters <- cld_letters[, c(iv, ".group")]
      colnames(group_letters) <- c("level", "POSTHOC")
    } else {
      group_letters <- data.frame(level = emmeans_tbl[[iv]], POSTHOC = NA)
    }

    tidy_aov <- broom::tidy(model)

    base_df <- emmeans_tbl %>%
      dplyr::rename(level = !!iv, Mean = emmean) %>%
      dplyr::left_join(group_letters, by = "level") %>%
      dplyr::mutate(
        iv = iv,
        dv = dv_var,
        df1 = tidy_aov$df[1],
        df2 = tidy_aov$df[2],
        Mean = round(Mean, digits),
        F_value = round(tidy_aov$statistic[1], digits),
        p_value = round(tidy_aov$p.value[1], digits)
      )

    if (posthoc != "none" && "p.value" %in% names(comp_df)) {
      adj_p <- round(min(comp_df$p.value, na.rm = TRUE), digits)
      final <- base_df %>%
        dplyr::mutate(
          adj_p = adj_p,
          sig = dplyr::case_when(
            adj_p < 0.001 ~ "***",
            adj_p < 0.01 ~ "**",
            adj_p < 0.05 ~ "*",
            TRUE ~ "ns"
          )
        )
      final <- if (adjust_only) {
        final %>% dplyr::select(iv, dv, level, Mean, POSTHOC, df1, df2, F_value, adj_p, sig)
      } else {
        final %>% dplyr::select(iv, dv, level, Mean, POSTHOC, df1, df2, F_value, p_value, adj_p, sig)
      }

    } else {
      final <- base_df %>%
        dplyr::mutate(
          sig = dplyr::case_when(
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
  if (posthoc == "none") {
    cat("\n🔎 POSTHOC method used: Unadjusted pairwise comparisons\n\n")
  } else {
    cat("\n✅ POSTHOC method used:", posthoc, "\n\n")
  }
  return(dplyr::bind_rows(results))
}

