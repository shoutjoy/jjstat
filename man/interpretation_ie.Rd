% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apa_sem.R
\name{interpretation_ie}
\alias{interpretation_ie}
\title{interpretation_ie}
\usage{
interpretation_ie(
  data,
  msg = "IE",
  md = FALSE,
  est = 3,
  std = 4,
  sig = 7,
  p = 6
)
}
\arguments{
\item{data}{data}

\item{msg}{msg="IE"}

\item{md}{md = FALSE}

\item{est}{est=3}

\item{std}{std=4}

\item{sig}{sig = 7}

\item{p}{p=6}
}
\value{
ie report
}
\description{
interpretation_ie
}
\examples{

\dontrun{
example(sem)
IE_effect(fit)
IE_effect(fit) \%>\% interpretation_ie()
}
}
