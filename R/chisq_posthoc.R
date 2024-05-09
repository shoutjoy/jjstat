
#' chisq post hoc
#'
#' @param tbl tavke
#' @param test fusger.test, chisq.test
#' @param Rows rows , FLASE = cols
#' @param control p adjust"bonferroni","fdr","BH","BY","holm","hochberg","hommel"
#' @param digits 4
#' @param ... ...
#'
#' @return result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' as.table(cbind("T"=c(30,40,60,120),"S"= c(50,70,60,80)))%>%
#'   chisq_posthoc(Rows  = TRUE)
#'
#' as.table(cbind("T"=c(30,40,60,120),"S"= c(50,70,60,80)))%>%
#'   chisq_posthoc(Rows  = FALSE)
#'
#' as.table(cbind("T"=c(30,40,60,120),"S"= c(50,70,60,80)))%>%
#'   chisq_posthoc(test="chisq.test", Rows  = TRUE)
#' }
#'
#'
chisq_posthoc <- function(tbl, test = fisher.test,
                          Rows = TRUE,
                          control = c("bonferroni","fdr","BH","BY","holm","hochberg","hommel"),
                          digits = 4, ...) {
  #### extract correction method
  control <- match.arg(control)

  #### extract which test (fisher or chi square)
  test <- match.fun(test)

  # 	#### test rows or columns
  if (!Rows) tbl <- t(tbl)
  popsNames <- rownames(tbl)

  #### come up with all possible comparisons
  prs <- combn(1:nrow(tbl), 2)

  #### preallocate
  tests <- ncol(prs)
  pvals <- numeric(tests)
  chisq <- numeric(tests)
  lbls <- character(tests)

  if (identical(test, fisher.test)) {
    for (i in 1:tests) {
      pvals[i] <- test(tbl[prs[,i],], ...)$p.value
      # chisq[i] <- test(tbl[prs[,i],], ...)$parameter
      lbls[i] <- paste(popsNames[prs[,i]], collapse = " vs. ")
    }
    adj.pvals <- p.adjust(pvals, method = control)
    cat("Adjusted p-values used the", control, "method.\n\n")
    res <- data.frame(comparison = lbls,
                      # chisq = round(chisq, digits),
                      raw.p = round(pvals, digits),
                      adj.p = round(adj.pvals, digits))
  } else if (identical(test, chisq.test)) {
    for (i in 1:tests) {
      pvals[i] <- test(tbl[prs[,i],], ...)$p.value
      chisq[i] <- test(tbl[prs[,i],], ...)$statistic
      lbls[i] <- paste(popsNames[prs[,i]], collapse = " vs. ")
    }
    adj.pvals <- p.adjust(pvals, method = control)
    cat("Adjusted p-values used the", control, "method.\n\n")

    res <- data.frame(comparison = lbls,
                      chisq = round(chisq, digits),
                      raw.p = round(pvals, digits),
                      adj.p = round(adj.pvals, digits))#%>%p_mark_sig("raw.p", unite=TRUE)
  }
  return(res)
}
