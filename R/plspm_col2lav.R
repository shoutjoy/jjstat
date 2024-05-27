#' plspm_col2lav, path, or measurement
#'
#' @param data plspm$boot$loadings
#' @param lhs lhs
#' @param rhs rhs
#' @param coef coeff
#' @param op "=~","~"
#'
#' @return lavaan lavaan syntac
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' satpls_boot <- data.frame(
#'   block = c("IMAG", "IMAG", "IMAG", "IMAG", "IMAG", "EXPE", "EXPE", "EXPE", "EXPE", "EXPE", "QUAL", "QUAL", "QUAL", "QUAL", "QUAL", "VAL", "VAL", "VAL", "VAL", "SAT", "SAT", "SAT", "SAT", "LOY", "LOY", "LOY", "LOY"),
#'   name = c("imag1", "imag2", "imag3", "imag4", "imag5", "expe1", "expe2", "expe3", "expe4", "expe5", "qual1", "qual2", "qual3", "qual4", "qual5", "val1", "val2", "val3", "val4", "sat1", "sat2", "sat3", "sat4", "loy1", "loy2", "loy3", "loy4"),
#'   Original = c(0.709, 0.877, 0.842, 0.569, 0.778, 0.766, 0.837, 0.76, 0.718, 0.837, 0.781, 0.882, 0.794, 0.789, 0.807, 0.865, 0.796, 0.75, 0.844, 0.92, 0.916, 0.826, 0.818, 0.907, 0.671, 0.905, 0.682)
#' )
#' #data
#' satpls_boot
#'
#' #method 1
#' satpls_boot %>% plspm_col2lav(lhs = "block", rhs = "name", coef = "Original")
#'
#' #method 2
#' satpls_boot %>% plspm_col2lav(1,2,3)
#'
#' #ethod 3
#' satpls_boot %>% plspm_col2lav( lhs = 1, rhs = 2, coef = 3)
#'
#' # draw model
#' satpls_boot %>% plspm_col2lav( lhs = 1, rhs = 2, coef = 3)%>%
#'   diagram_model(whatLabels = "est", exoCov = F, sizeLat=6, sizeMan=6,sizeMan2=2,
#'                 edge.label.cex = 0.6)
#'
#'
#' }
#'
plspm_col2lav <- function(data, lhs, rhs, coef, op="=~") {
  lhs_col <- if (is.numeric(lhs)) colnames(data)[lhs] else lhs
  rhs_col <- if (is.numeric(rhs)) colnames(data)[rhs] else rhs
  coef_col <- if (is.numeric(coef)) colnames(data)[coef] else coef

  data %>%
    mutate(path = paste0(!!sym(lhs_col), op, !!sym(coef_col), "*", !!sym(rhs_col))) %>%
    select(path) %>%
    pull(path) %>%
    paste(collapse = "\n")
}
