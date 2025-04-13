#' ANOVA Summary with agricolae-based Posthoc Comparison
#'
#' @description
#' Performs one-way ANOVA for the specified dependent variable across one or more categorical independent variables.
#' Supports posthoc comparison using methods from the `agricolae` package: `"lsd"`, `"duncan"`, `"scheffe"`, and `"tukey"`.
#' The resulting ANOVA p-value is optionally adjusted using the `p.adjust()` method.
#'
#' @param data A data frame containing the data to analyze.
#' @param dv_var A string indicating the name of the dependent variable (e.g., "mpg").
#' @param iv_vars A character vector of independent variable names (e.g., c("group", "gender")).
#' @param grp_mean Logical. Whether to include group means in the output (default = TRUE).
#' @param posthoc Posthoc test method. One of "lsd", "duncan", "scheffe", or "tukey".
#' @param p.adj Method for p-value adjustment in ANOVA. If NULL, it is set automatically based on the posthoc method.
#' @param sig Logical. Whether to include significance symbols (e.g., "***", "**", "*", "ns") in the output.
#' @param alpha_posthoc Significance level for posthoc tests (default = 0.05).
#' @param digits Number of digits to round the output values (default = 2).
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{iv}{Independent variable name}
#'   \item{dv}{Dependent variable name}
#'   \item{level}{Levels of the independent variable}
#'   \item{Mean}{Group mean (optional)}
#'   \item{POSTHOC}{Group classification from posthoc test (e.g., "a", "b", "ab")}
#'   \item{df1, df2}{Degrees of freedom for ANOVA}
#'   \item{F_value}{F statistic from ANOVA}
#'   \item{p_value}{Adjusted p-value from ANOVA}
#'   \item{sig}{Significance symbols (optional)}
#' }
#'
#' @examples
#' # Load packages
#' library(dplyr)
#' library(agricolae)
#' library(broom)
#' library(tibble)
#'
#' # Load example data
#' data("mtcars")
#' mtcars$cyl <- as.factor(mtcars$cyl)
#'
#' # Base ANOVA model
#' aov_model <- aov(mpg ~ cyl, data = mtcars)
#'
#' # 1) LSD
#' cat("===== LSD (custom function) =====\n")
#' print(aov_df(mtcars, dv_var = "mpg", iv_vars = "cyl", posthoc = "lsd"))
#' cat("===== LSD (agricolae) =====\n")
#' print(agricolae::LSD.test(aov_model, "cyl", console = FALSE)$groups)
#'
#' # 2) Duncan
#' cat("===== Duncan (custom function) =====\n")
#' print(aov_df(mtcars, dv_var = "mpg", iv_vars = "cyl", posthoc = "duncan"))
#' cat("===== Duncan (agricolae) =====\n")
#' print(agricolae::duncan.test(aov_model, "cyl", console = FALSE)$groups)
#'
#' # 3) Scheffe
#' cat("===== Scheffe (custom function) =====\n")
#' print(aov_df(mtcars, dv_var = "mpg", iv_vars = "cyl", posthoc = "scheffe"))
#' cat("===== Scheffe (agricolae) =====\n")
#' print(agricolae::scheffe.test(aov_model, "cyl", console = FALSE)$groups)
#'
#' # 4) Tukey
#' cat("===== Tukey (custom function) =====\n")
#' print(aov_df(mtcars, dv_var = "mpg", iv_vars = "cyl", posthoc = "tukey"))
#' cat("===== Tukey (agricolae) =====\n")
#' print(agricolae::HSD.test(aov_model, "cyl", console = FALSE)$groups)
#'
#' @export


aov_df <- function(data,
                   dv_var = NULL,
                   iv_vars = NULL,
                   grp_mean = TRUE,
                   posthoc = "lsd",
                   p.adj = NULL,
                   sig = TRUE,
                   alpha_posthoc = 0.05,
                   digits = 2) {

  if (!is.data.frame(data)) stop("Input must be a data.frame.")
  if (is.null(dv_var) | is.null(iv_vars)) stop("Both dv_var and iv_vars must be provided.")
  if (!posthoc %in% c("lsd", "duncan", "scheffe", "tukey"))
    stop("posthoc must be one of: lsd, duncan, scheffe, tukey")

  if (is.null(p.adj)) {
    p.adj <- switch(posthoc,
                    lsd     = "none",
                    duncan  = "bonferroni",
                    scheffe = "none",
                    tukey   = "none")
  }

  data <- as_trt(data, iv_vars)
  all_result <- list()

  for (iv in iv_vars) {
    aov_model <- aov(formula(paste(dv_var, "~", iv)), data = data)
    tidy_aov <- broom::tidy(aov_model)

    raw_p <- tidy_aov$p.value[1]
    df1 <- tidy_aov$df[1]
    df2 <- tidy_aov$df[2]
    Fval <- tidy_aov$statistic[1]
    adj_p <- p.adjust(raw_p, method = p.adj)

    mean_tbl <- data %>%
      group_by_at(iv) %>%
      summarise(Mean = mean(!!sym(dv_var), na.rm = TRUE), .groups = "drop") %>%
      rename(level = !!iv)

    posthoc_obj <- switch(posthoc,
                          lsd     = agricolae::LSD.test(aov_model, iv, console = FALSE, p.adj = p.adj, alpha = alpha_posthoc),
                          duncan  = agricolae::duncan.test(aov_model, iv, console = FALSE, alpha = alpha_posthoc),
                          scheffe = agricolae::scheffe.test(aov_model, iv, console = FALSE, alpha = alpha_posthoc),
                          tukey   = agricolae::HSD.test(aov_model, iv, console = FALSE, alpha = alpha_posthoc))

    posthoc_df <- posthoc_obj$groups %>%
      tibble::rownames_to_column("level") %>%
      rename(POSTHOC = 3)

    merged <- mean_tbl %>%
      left_join(posthoc_df, by = "level") %>%
      mutate(iv = iv,
             dv = dv_var,
             df1 = df1,
             df2 = df2,
             F_value = round(Fval, digits),
             p_value = round(adj_p, digits))

    all_result[[iv]] <- merged
  }

  result_df <- dplyr::bind_rows(all_result) %>% tibble::as_tibble()

  if (!grp_mean) {
    result_df <- result_df %>% dplyr::select(-Mean)
  }

  if (sig) {
    result_df <- result_df %>%
      mutate(sig = dplyr::case_when(
        p_value < 0.001 ~ '***',
        p_value < 0.01  ~ '**',
        p_value < 0.05  ~ '*',
        TRUE            ~ 'ns'
      ))
  }

  return(result_df %>%
           dplyr::select(any_of(c("iv", "dv", "level", "Mean", "POSTHOC", "df1", "df2", "F_value", "p_value", "sig"))))
}


#'
#'
#' #'
#' #'
#' #'#' Generate an AOV results table for multiple independent variables
#' #'
#' #' @param data data.frame
#' #' @param dv_var dv
#' #' @param iv_vars c(iv1, iv2....)
#' #' @param unite_F Fvalue unite
#' #' @param unite_p pvalue unite
#' #' @param sig 'sig = T' is add star
#' #' @param grp_mean 'grooup mean remove
#' #' @param posthoc 'tukey, scheffe, duncan, lsd
#' #' @param p.adj  p.adj=c("none","holm","hommel", "hochberg", "bonferroni", "BH", "BY", "fdr") only lsd test
#' #'
#' #' @return table
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #' aov_table(data = mtcars, dv_var = "mpg",
#' #' iv_vars = c("cyl", "gear", "carb"))
#' #'
#' #' aov_table(data = mtcars, dv_var = "mpg",
#' #' iv_vars = c("cyl", "gear", "carb"),
#' #'            mean = TRUE)
#' #'
#' #' aov_table(data = mtcars, dv_var = "mpg",
#' #' iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE)
#' #' aov_table(data = mtcars, dv_var = "mpg",
#' #' iv_vars = c("cyl", "gear", "carb"), grp_mean = FALSE) %>%
#' #'
#' #'
#' #' aov_table(data = mtcars, dv_var = "mpg",
#' #' iv_vars = c("cyl", "gear", "carb"),
#' #'  grp_mean = TRUE)
#' #'
#' #' aov_table(data = mtcars, dv_var = "mpg",
#' #'  iv_vars = c("cyl", "gear", "carb"),
#' #'  grp_mean = TRUE, unite_F = TRUE)
#' #'
#' #' aov_table(data = mtcars, dv_var = "mpg",
#' #' iv_vars = c("cyl", "gear", "carb"),
#' #'  grp_mean = TRUE, unite_F = TRUE) %>%
#' #'   markdown_table()
#' #'
#' #' # park516d데이터 이용
#' #' park516d %>%
#' #'   select(1:6,teaching, academyType) %>%  #주요변수 선택
#' #'   bind_add_stat(c("RV_Teah",1:6)) %>% # 평균을 이용하여 항목묶음
#' #'   aov_df(dv_var="RV_Teah",
#' #'          iv_vars =c("teaching", "academyType"), sig = T)
#' #'
#' #'
#' #' }
#' #'
#' #'
#' #
#' aov_df <- function(data,
#'                    dv_var = NULL,
#'                    iv_vars = NULL,
#'                    grp_mean = TRUE,
#'                    unite_p = FALSE,
#'                    unite_F = FALSE,
#'                    digits = 2,
#'                    posthoc = "lsd",
#'                    p.adj = "bonferroni",
#'                    sig = FALSE
#' ) {
#'
#'   if (!is.data.frame(data)) {
#'     stop("you need input data.frame")
#'   }
#'
#'   if (is.null(dv_var) | is.null(iv_vars)) {
#'     stop("Please provide both dv_var and iv_vars.")
#'   }
#'
#'   cat("\naov_df & aov_table Result\n",
#'       "grp_mean is TRUE -> add grp_mean / grp_mean is FALSE -> only levels \n\n")
#'
#'   data <- as_trt(data, iv_vars)  # factor treatment
#'
#'   # Initialize a data frame to store the results
#'   result_df <- data.frame(Indv_Variable = character(0),
#'                           F_value = numeric(0),
#'                           p_value = numeric(0))
#'
#'   # Perform an ANOVA analysis for each independent variable
#'   for (iv in iv_vars) {
#'     # ANOVA
#'     anova_result <- aov(formula(paste(dv_var, "~", iv)), data = data)
#'
#'     # Group-by mean calculation using dplyr to ensure correct matching
#'     meandata <- data %>%
#'       group_by_at(iv) %>%
#'       summarise(mean_value = mean(!!sym(dv_var), na.rm = TRUE))
#'
#'     # Posthoc tests
#'     lsd = agricolae::LSD.test(anova_result, iv, console = FALSE, p.adj = p.adj)
#'     duncan = agricolae::duncan.test(anova_result, iv, console = FALSE)
#'     scheffe = agricolae::scheffe.test(anova_result, iv, console = FALSE)
#'     tukey = agricolae::HSD.test(anova_result, iv, console = FALSE)
#'
#'     posthoc_groups_lsd <- lsd$groups[, 2]
#'     posthoc_groups_duncan <- duncan$groups[, 2]
#'     posthoc_groups_scheffe <- scheffe$groups[, 2]
#'     posthoc_groups_tukey <- tukey$groups[, 2]
#'
#'     group_levels <- rownames(lsd$groups)
#'
#'     # Reorder meandata to match the posthoc group order
#'     meandata <- meandata %>%
#'       mutate(group = !!sym(iv)) %>%
#'       arrange(match(group, group_levels))
#'
#'     # Extract ANOVA result statistics
#'     tidy_result <- broom::tidy(anova_result)
#'
#'     # Create result dataframe
#'     result_df <- rbind(result_df,
#'                        data.frame(
#'                          iv = iv,
#'                          dv = dv_var,
#'                          level = group_levels,  # Add levels to the result
#'                          Mean = meandata$mean_value,  # Matching means with groups
#'                          posthoc_lsd = posthoc_groups_lsd,
#'                          posthoc_duncan = posthoc_groups_duncan,
#'                          posthoc_scheffe = posthoc_groups_scheffe,
#'                          posthoc_tukey = posthoc_groups_tukey,
#'                          df1 = tidy_result$df[1],
#'                          df2 = tidy_result$df[2],
#'                          F_value = tidy_result$statistic[1],
#'                          p_value = tidy_result$p.value[1]
#'                        ))
#'   }
#'
#'   # Posthoc selection and column renaming
#'   if (posthoc == "lsd") {
#'     result_df <- result_df %>%
#'       dplyr::select(-posthoc_duncan, -posthoc_scheffe, -posthoc_tukey) %>%
#'       dplyr::rename(POSTHOC = posthoc_lsd)
#'     cat("   Using The Least Significant Difference (LSD) posthoc \n\n")
#'   } else if (posthoc == "duncan") {
#'     result_df <- result_df %>%
#'       dplyr::select(-posthoc_lsd, -posthoc_scheffe, -posthoc_tukey) %>%
#'       dplyr::rename(POSTHOC = posthoc_duncan)
#'     cat("   Using Duncan's posthoc \n\n")
#'   } else if (posthoc == "scheffe") {
#'     result_df <- result_df %>%
#'       dplyr::select(-posthoc_lsd, -posthoc_duncan, -posthoc_tukey) %>%
#'       dplyr::rename(POSTHOC = posthoc_scheffe)
#'     cat("   Using Scheffe's posthoc \n\n")
#'   } else if (posthoc == "tukey") {
#'     result_df <- result_df %>%
#'       dplyr::select(-posthoc_lsd, -posthoc_duncan, -posthoc_scheffe) %>%
#'       dplyr::rename(POSTHOC = posthoc_tukey)
#'     cat("   Using Tukey's HSD posthoc \n\n")
#'   }
#'
#'   # Group Mean Inclusion
#'   if (grp_mean) {
#'     result_df <- result_df %>%
#'       tibble::tibble()
#'
#'     result_df$p_value <- as.numeric(format(result_df$p_value, nsmall = 3))
#'
#'     if (sig) {
#'       result_df <- result_df %>% mutate(sig = add_sig(result_df$p_value))
#'     }
#'
#'   } else {
#'     result_df <- result_df %>%
#'       dplyr::select(-Mean)
#'
#'     result_df <- result_df %>%
#'       tibble::tibble()
#'
#'     result_df$p_value <- as.numeric(format(result_df$p_value, nsmall = 3))
#'
#'     if (sig) {
#'       result_df <- result_df %>% mutate(sig = add_sig(result_df$p_value))
#'     }
#'   }
#'
#'   # Unite F-value and p-value if needed
#'   if (unite_F) {
#'     result_df <- result_df %>%
#'       mutate(sig = ifelse(p_value < 0.001, "***",
#'                           ifelse(p_value < 0.01, "**",
#'                                  ifelse(p_value < 0.05, "*", "")))) %>%
#'       unite(F_value, F_value, sig, sep = "")
#'   } else if (unite_p) {
#'     result_df <- result_df %>%
#'       mutate(sig = ifelse(p_value < 0.001, "***",
#'                           ifelse(p_value < 0.01, "**",
#'                                  ifelse(p_value < 0.05, "*", "")))) %>%
#'       unite(p_value, p_value, sig, sep = "")
#'   }
#'
#'   return(result_df)
#' }


