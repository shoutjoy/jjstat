% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chisq_multcomp.R
\name{chisq_multcomp}
\alias{chisq_multcomp}
\title{Pairwise comparisons after a chi-squared goodness-of-fit test}
\usage{
chisq_multcomp(x, p.method = "fdr")
}
\arguments{
\item{x}{numeric vector (counts).}

\item{p.method}{method for p-values correction. See help of p.adjust}
}
\value{
result
}
\description{
Pairwise comparisons after a chi-squared goodness-of-fit test
}
\section{Functions}{
\itemize{
\item \code{chisq_multcomp()}: pairwise comparisons after a global chi-squared goodness-of-fit test.

}}
\examples{
\dontrun{
#'
counts <- c(49,30,63,59)
chisq.test(counts)
chisq_multcomp(counts)
# compare
chisq.multcomp(counts)

}
}
