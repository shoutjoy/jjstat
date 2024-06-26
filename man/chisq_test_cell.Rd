% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chisq_test_cell.R
\name{chisq_test_cell}
\alias{chisq_test_cell}
\title{Chi-square analysis by cell}
\usage{
chisq_test_cell(data, type = "res", trans = FALSE, plot = TRUE, digits = 3)
}
\arguments{
\item{data}{table}

\item{type}{res, alll()default), chisq_result , observed   expected,   residuals ,  chisq_cell ,  stdres, chisq_cell_sig ,  cramersv ,  OE_ratio,    OE_sig ,   graph}

\item{trans}{plot transpose}

\item{plot}{TRUE}

\item{digits}{digits= 3}
}
\value{
chisq
}
\description{
Chi-square analysis by cell
}
\examples{

\dontrun{
#'


M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
M \%>\% chisq_test_cell()

#notsig
examtest  = matrix(c(7,14,13,6), nrow=2,
                   dimnames= list(c("사전검사","사후검사"),
                                  c("합격","불합격")))
examtest
fisher.test(examtest)

examtest \%>\% chisq_test_cell()
}


}
