
#' lavaan data confint plot
#'
#' @param lavaan_obj lavaan
#' @param op op
#' @param type g, res, all
#' @param size_axis axis test size
#' @param size_text size ext
#' @param linewidth linewidth=2
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#'
#' }
#'
confint_plot_sem = function(lavaan_obj,type="g",
                            effect = "~",
                            size_axis = 14,
                            size_text = 4,
                            linewidth = 2
                            ){

  result = lavaan_obj %>%
            parameterEstimates(ci = TRUE) %>%
            filter(op == effect) %>%
            p_mark_sig("pvalue")


  g = lavaan_obj %>%
    parameterEstimates(ci = TRUE)%>%  filter(op == effect) %>%
    p_mark_sig("pvalue", ns="") %>%

    tidyr::unite(lhs, lhs, rhs, sep = " <- ") %>%
    Round(3)%>%
    tidyr::unite(Est, est, sig, sep = "", remove = FALSE) %>%
    ggplot(aes(x = est, y = lhs, xmin = ci.lower, xmax = ci.upper, height=0 ))+
    geom_errorbarh(color = "steelblue", linewidth = linewidth, alpha = 0.8)+
    geom_text(aes(label = Est) , vjust = -0.5, size = size_text)+
    geom_vline(xintercept = 0, lty = 2, color = "red")+
    labs(x = "estimate", y = "path")+
    theme_bw()+
    theme(axis.text = element_text(size = size_axis, face="bold"))

  all = list(result = result, g = g)

  switch(type, all = all, res = result, g = g )

}
