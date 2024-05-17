#' Pairwise comparisons after a chi-squared goodness-of-fit test
#'
#' @param x numeric vector (counts).
#' @param p.method method for p-values correction. See help of p.adjust
#'
#' @return result
#' @export

#' @examples
#' \dontrun{
#' #'
#' counts <- c(49,30,63,59)
#' chisq.test(counts)
#' chisq.multcomp(counts)
#' }
chisq_multcomp <-function(x, p.method="fdr") {
  #FDR(False Discovery Rate)
  x = as.numeric(x)
  #   x <- sort(x)

  fun.p <- function(i,j) {
    xi <- x[i]
    xj <- x[j]
    suppressWarnings(chisq.test(c(xi, xj)))$p.value
  }
  tab.p <- pairwise.table(fun.p, as.character(x),
                          p.adjust.method=p.method)
  plist= tab.p%>%
    long_df("cell_1","cell_2","p.value") %>%
    drop_na () %>%
    mutate(cell_1 = gsub("X", "", cell_1),
           cell_2 = gsub("X", "", cell_2)) %>%
    p_mark_sig()%>%
    dplyr::select(cell_2, cell_1, p.value, sig)%>%
    tidyr::unite(pairwise, cell_2, cell_1)
cat("\n")
cat(p.method)
cat("\n")
  plist
  # tab.p

}
