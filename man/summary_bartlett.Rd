% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_bartlett.R
\name{summary_bartlett}
\alias{summary_bartlett}
\title{Factorization Bartlett test results}
\usage{
summary_bartlett(data, test = "df", cat = TRUE)
}
\arguments{
\item{data}{data. cor data. result data}

\item{test}{perform test df, cor, none}

\item{cat}{show method}
}
\value{
result
}
\description{
Factorization Bartlett test results
}
\examples{

\dontrun{
#'
# Henry Kaiser (1970) introduced an Measure of Sampling
# Adequacy  (MSA) of factor analytic data matrices.
# Kaiser and Rice (1974) then modified it.
#'

New = data.frame(
  A01 = c(4, 4, 5, 4, 5, 5, 5, 4, 4, 2, 4, 4, 5, 4, 4, 4, 4, 5, 1, 3),
  A02 = c(2, 3, 3, 5, 5, 4, 5, 5, 4, 4, 4, 4, 4, 3, 3, 5, 4, 4, 3, 1),
  A03 = c(3, 5, 4, 5, 4, 4, 4, 4, 4, 4, 5, 4, 5, 4, 4, 4, 4, 5, 3, 3),
  A04 = c(4, 4, 4, 4, 5, 4, 5, 4, 4, 3, 5, 4, 5, 5, 5, 4, 5, 5, 1, 5),
  A05 = c(3, 2, 3, 5, 4, 4, 4, 4, 4, 4, 4, 3, 5, 2, 3, 4, 5, 5, 4, 2)
)
New \%>\%summary_bartlett("df")
New \%>\%cor()\%>\% summary_bartlett("cor")
New \%>\%cor()\%>\%psych::cortest.bartlett()\%>\% summary_bartlett()

}
}
