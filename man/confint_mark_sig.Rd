% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confint_mark_sig.R
\name{confint_mark_sig}
\alias{confint_mark_sig}
\title{Confint mark significant}
\usage{
confint_mark_sig(data, unite = FALSE, digits = 3)
}
\arguments{
\item{data}{data.frame Confidence interval significance test: When the confidence interval is in the last column.  Significance test when the last two columns of data are confidence intervals}

\item{unite}{TRUE is combine CI and sig}

\item{digits}{default 3}
}
\description{
Confint mark significant
}
\examples{

\dontrun{

lm(mpg ~ hp + wt, data=mtcars)\%>\%
 confint()\%>\%as.data.frame()\%>\%
  rownames_to_column()\%>\%
  confint_mark_sig()

## combind data unite=TRUE
  lm(mpg ~ hp + wt, data=mtcars)\%>\%
     confint()\%>\%
     as.data.frame()\%>\%
     rownames_to_column()\%>\%
     confint_mark_sig(unite=TRUE, digits=2)



  }

}
