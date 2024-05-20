#' mutate data generate
#' @importFrom grDevices x11
#' @importFrom graphics abline barplot hist legend lines mosaicplot pairs par stars text
#' @importFrom stats AIC BIC addmargins aggregate anova aov ave bartlett.test binom.test chisq.test coef complete.cases confint cor density df filter fisher.test formula na.omit p.adjust pairwise.table pchisq pnorm pt qchisq qnorm qt sd shapiro.test summary.aov t.test uniroot var var.test vcov xtabs
#' @importFrom utils browseURL combn read.table
#' @param p.value pvalue
#'
#' @return in data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' aov_table(data = mtcars, dv_var = "mpg",
#' iv_vars = c("cyl", "gear", "carb"), grp_mean = T,
#' unite_F=TRUE) %>%
#'  mutate(STAR= add_sig(p_value))
#'
#' }
add_sig <- function(var){


  ifelse(var < 0.001, "***",
         ifelse(var < 0.01, "**",
                ifelse(var < 0.05, "*",
                       " ns")))
}

