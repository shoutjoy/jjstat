
#' lavaan data confin tplot
#'
#' @param data lavaan data parameterEstimates %>% filter(op==":=")
#' @param y lhs
#' @param x est
#' @param lower ci.lower
#' @param upper ci.upper
#' @param size_text 12
#' @param color steelblue
#' @param linewidth 1
#' @param alpha 0.8
#' @param vlinewidth 1,2
#' @param vlinecolor red
#'
#' @return plot
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Create a tibble named semdata with the given data
#'
#' semdata%>%confint_plot_sem()
#'
#' }
confint_plot_sem <- function(data,
                             y="lhs",
                             x="est",
                             lower="ci.lower",
                             upper="ci.upper",
                             size_text=12,
                             color="steelblue",
                             linewidth=1,
                             alpha=0.8,
                             vlinewidth=1.2,
                             vlinecolor="red"
){
  library(tidyverse)


  data%>% rename(
    lhs = all_of(y),
    est = all_of(x),
    ci.lower = all_of(lower),
    ci.upper = all_of(upper),
  )%>%
    tibble::tibble()%>%
    ggplot(aes(y = lhs,
               x = est,
               xmin = ci.lower,
               xmax = ci.upper,
               height = 0)) +
    geom_errorbarh(color=color,
                   linewidth=linewidth+1,
                   alpha=alpha)+
    geom_point(size=3) +
    geom_vline(xintercept = 0,
               lty = 2,
               color=vlinecolor,
               linewidth = vlinewidth-0.2) +
    theme_bw()+
    theme(axis.text.y = element_text(size=size_text))
}
