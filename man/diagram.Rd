% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sem_diagram.R
\name{diagram}
\alias{diagram}
\title{diagram sem plot}
\usage{
diagram(
  lav_obj,
  what = "est",
  rotation = 4,
  nDigits = 2,
  residScale = 18,
  sig = TRUE,
  edge.label.cex = 1.5
)
}
\arguments{
\item{lav_obj}{lavaan}

\item{what}{est, std, model}

\item{rotation}{4}

\item{nDigits}{3}

\item{residScale}{18}

\item{sig}{FALSE}

\item{edge.label.cex}{1}
}
\value{
plot
}
\description{
diagram sem plot
}
\examples{

\dontrun{
example(cfa)
fit \%>\% diagram()

}

}
