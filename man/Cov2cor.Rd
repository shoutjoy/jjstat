% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov2cor.R
\name{Cov2cor}
\alias{Cov2cor}
\title{cov to cor}
\usage{
Cov2cor(cov_matrix)
}
\arguments{
\item{cov_matrix}{cov matrix}
}
\value{
cor mat
}
\description{
cov to cor
}
\examples{

\dontrun{
cov(mtcars[,c("mpg","hp")])\%>\%cov2cor()
}
}
