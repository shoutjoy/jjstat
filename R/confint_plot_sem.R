
#' Plot Confidence Intervals for SEM Paths using lavaan
#'
#' Create a confidence interval plot of structural paths from a lavaan SEM object.
#'
#' @param lavaan_obj A fitted `lavaan` object (e.g., from `sem()` or `cfa()`).
#' @param type Type of output: `"g"` for the ggplot object, `"res"` for the result table, `"all"` for both.
#' @param effect Operator type to filter by, default is `"~"` for regression paths. Use `"=~"` for latent loadings.
#' @param var_named Named vector for renaming variables using `replace_df_rep()`. Default is `NULL`.
#' @param size_axis Numeric. Font size for axis labels in the plot. Default is 14.
#' @param size_text Numeric. Size of text showing estimates in the plot. Default is 4.
#' @param linewidth Numeric. Width of confidence interval error bars. Default is 2.
#'
#' @return A ggplot object, a data frame, or a list depending on the `type` argument.
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#'   speed ~ visual + textual
#' '
#' fit <- sem(model, data = HolzingerSwineford1939)
#' named_vec <- c("visual" = "시각", "textual" = "언어", "speed" = "속도")
#' confint_plot_sem(fit, type = "g", effect = "~", var_named = named_vec)
#' }
confint_plot_sem = function(lavaan_obj,
                            type = "g",
                            effect = "~",
                            var_named = NULL,
                            size_axis = 14,
                            size_text = 4,
                            linewidth = 2) {

  # 기본 추정 결과
  result = lavaan_obj %>%
    parameterEstimates(ci = TRUE) %>%
    filter(op == effect) %>%
    p_mark_sig("pvalue")

  # 이름 치환 적용 (선택적)
  if (!is.null(var_named)) {
    result <- result %>% replace_df_rep(var_named)
  }

  # 그래프 생성
  g = result %>%
    p_mark_sig("pvalue", ns = "") %>%
    tidyr::unite(lhs, lhs, rhs, sep = " <- ") %>%
    Round(3) %>%
    tidyr::unite(Est, est, sig, sep = "", remove = FALSE) %>%
    ggplot(aes(x = est, y = lhs, xmin = ci.lower, xmax = ci.upper, height = 0)) +
    geom_errorbarh(color = "steelblue", linewidth = linewidth, alpha = 0.8) +
    geom_text(aes(label = Est), vjust = -0.5, size = size_text) +
    geom_vline(xintercept = 0, lty = 2, color = "red") +
    labs(x = "estimate", y = "path") +
    theme_bw() +
    theme(axis.text = element_text(size = size_axis, face = "bold"))

  all = list(result = result, g = g)

  switch(type,
         all = all,
         res = result,
         g = g)
}




#'
#' #' lavaan data confint plot
#' #'
#' #' @param lavaan_obj lavaan
#' #' @param op op
#' #' @param type g, res, all
#' #' @param size_axis axis test size
#' #' @param size_text size ext
#' #' @param linewidth linewidth=2
#' #'
#' #' @return plot
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' \dontrun{
#' #'
#' #'
#' #'
#' #' }
#' #'
#' confint_plot_sem = function(lavaan_obj,type="g",
#'                             effect = "~",
#'                             size_axis = 14,
#'                             size_text = 4,
#'                             linewidth = 2
#'                             ){
#'
#'   result = lavaan_obj %>%
#'             parameterEstimates(ci = TRUE) %>%
#'             filter(op == effect) %>%
#'             p_mark_sig("pvalue")
#'
#'
#'   g = lavaan_obj %>%
#'     parameterEstimates(ci = TRUE)%>%  filter(op == effect) %>%
#'     p_mark_sig("pvalue", ns="") %>%
#'
#'     tidyr::unite(lhs, lhs, rhs, sep = " <- ") %>%
#'     Round(3)%>%
#'     tidyr::unite(Est, est, sig, sep = "", remove = FALSE) %>%
#'     ggplot(aes(x = est, y = lhs, xmin = ci.lower, xmax = ci.upper, height=0 ))+
#'     geom_errorbarh(color = "steelblue", linewidth = linewidth, alpha = 0.8)+
#'     geom_text(aes(label = Est) , vjust = -0.5, size = size_text)+
#'     geom_vline(xintercept = 0, lty = 2, color = "red")+
#'     labs(x = "estimate", y = "path")+
#'     theme_bw()+
#'     theme(axis.text = element_text(size = size_axis, face="bold"))
#'
#'   all = list(result = result, g = g)
#'
#'   switch(type, all = all, res = result, g = g )
#'
#' }
