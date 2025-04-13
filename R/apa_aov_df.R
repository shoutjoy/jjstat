
#' APA style ANOVA summary
#'
#' @param data A data frame. This can be either the result of `aov_df()` / `aov_table()`,
#'   or raw data to be analyzed if `dv_var` and `iv_vars` are provided.
#' @param show Logical. Whether to print the ANOVA table and APA-style summaries to the console. Default is TRUE.
#' @param dv_var A string. The name of the dependent variable (DV) to be used in the ANOVA.
#' @param iv_vars A character vector. The names of the independent variables (IVs) to be included in the ANOVA.
#'
#' @return Prints APA-style one-way ANOVA summaries for each IV, and (invisibly) returns the input data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Analyze raw data
#' apa_aov_df(data = mtcars, dv_var = "mpg", iv_vars = c("cyl", "gear"))
#'
#' # Example 2: Pipe analysis results into APA report
#' mtcars %>%
#'   aov_df(dv_var = "mpg", iv_vars = c("cyl", "gear")) %>%
#'   apa_aov_df()
#'
#'
#'
#'
#' }
apa_aov_df <- function(data, show = TRUE, dv_var = NULL, iv_vars = NULL) {

  # dv_var와 iv_vars가 주어졌을 경우, 분석을 직접 수행
  if (!is.null(dv_var) & !is.null(iv_vars)) {
    data <- aov_table(data = data, dv_var = dv_var, iv_vars = iv_vars, grp_mean = FALSE)
  }

  # 출력
  if (show) {
    cat("\nANOVA result\n")
    print(data)
  }
  cat("\n\n")

  # 중복 제거: 같은 iv 변수에 대해 한 번만 출력
  unique_data <- data %>% dplyr::distinct(iv, .keep_all = TRUE)

  for (i in 1:nrow(unique_data)) {
    iv <- unique_data$iv[i]
    dv <- unique_data$dv[i]
    df1 <- unique_data$df1[i]
    df2 <- unique_data$df2[i]
    F_value <- sprintf("%.2f", unique_data$F_value[i])
    p_value <- unique_data$p_value[i]

    p_value_sig <- ifelse(p_value < 0.001, "< .001", sprintf("%.3f", p_value))
    significance <- ifelse(p_value < 0.001, '***',
                           ifelse(p_value < 0.01, '**',
                                  ifelse(p_value < 0.05, '*', '')))

    cat(paste0(iv, "에 따른 ", dv, "의 일원배치 분산분석(oneway-ANOVA) 분석결과, "))
    cat(paste0(ifelse(p_value < 0.05, "통계적으로 유의하게 나타났다",
                      "통계적으로 유의하지 않았다"),
               " (F(", df1, ", ", df2, ") = ", F_value, significance,
               ", p = ", p_value_sig, ").\n"))
  }

  invisible(data)
}
