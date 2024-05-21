#' Pairwise comparisons after a chi-squared goodness-of-fit test
#'
#' @param x numeric vector (counts).
#' @param p.method method for p-values correction. See help of p.adjust
#' @describeIn Performs pairwise comparisons after a global chi-squared goodness-of-fit test.
#'
#' @return result
#' @export

#' @examples
#' \dontrun{
#' #'
#' counts <- c(49,30,63,59)
#' chisq.test(counts)
#' chisq_multcomp(counts)
#' # compare
#' chisq.multcomp(counts)
#'
#' }
chisq_multcomp <- function(x, p.method = "fdr") {
library(dplyr)
library(tidyr)
library(stats)

printx = as.character(x)
x = as.numeric(x)

fun.p <- function(i, j) {
  xi <- x[i]
  xj <- x[j]
  suppressWarnings(chisq.test(c(xi, xj)))$p.value
}

tab.p <- pairwise.table(fun.p, as.character(x), p.adjust.method = p.method)

plist <- as.data.frame(as.table(tab.p)) %>%
  dplyr::rename(cell_1 = Var1, cell_2 = Var2, p.value = Freq) %>%
  dplyr::mutate(
    cell_1 = gsub("X", "", cell_1),
    cell_2 = gsub("X", "", cell_2)
  ) %>%
  dplyr::filter(!is.na(p.value)) %>%
  dplyr::mutate(sig = ifelse(p.value < 0.001, "***",
                             ifelse(p.value < 0.01, "**",
                                    ifelse(p.value < 0.05, "*", "ns")))) %>%
  dplyr::select(cell_2, cell_1, p.value, sig) %>%
  tidyr::unite(pairwise, cell_2, cell_1, sep = "_")

cat("\n")
cat(paste0("p adjust method: ", p.method))
cat("\n")
cat(paste0("Your Data counts: ", paste(printx, collapse=",")))
cat("\n")

return(plist%>%tibble::tibble())
}



#' RVAideMemoire::chisq.multcomp, Pairwise comparisons after a chi-squared goodness-of-fit test
#'
#' @param x numeric vector (counts).
#' @param p.method 	method for p-values correction. See help of p.adjust.
#'
#' @return result
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' counts <- c(49,30,63,59)
#' chisq.test(counts)
#' chisq.multcomp(counts)
#' #comapre
#' chisq_multcomp(counts)
#'
#' }
chisq.multcomp = function (x, p.method = "fdr")
{
  x <- sort(x)
  fun.p <- function(i, j) {
    xi <- x[i]
    xj <- x[j]
    suppressWarnings(chisq.test(c(xi, xj)))$p.value
  }
  tab.p <- pairwise.table(fun.p, as.character(x), p.adjust.method = p.method)
  call <- match.call()
  dname.x <- if (length(call$x) == 1) {
    call$x
  }
  else {
    paste(call$x[1], "(", paste(call$x[-1], collapse = ","),
          ")", sep = "")
  }
  result <- list(method = "chi-squared tests", data.name = dname.x,
                 p.adjust.method = p.method, p.value = tab.p)
  class(result) <- "pairwise.htest"
  return(result)
}
