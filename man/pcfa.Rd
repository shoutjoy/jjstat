% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcfa.R
\name{pcfa}
\alias{pcfa}
\title{principal component factor analysis function}
\usage{
pcfa(
  R = NULL,
  nfactor = NULL,
  rowname = NULL,
  digits = 2,
  xlim = NULL,
  ylim = NULL,
  cex = 1.2,
  vjust = -0.5,
  yadd = 10,
  name = "auto",
  pos = 1
)
}
\arguments{
\item{R}{correlation matrix}

\item{nfactor}{number of factor}

\item{rowname}{matrix rowname}

\item{digits}{digits = 3}

\item{xlim}{NULL}

\item{ylim}{NULL}

\item{cex}{size =1}

\item{yadd}{yadd = 10,}

\item{name}{'auto', 'manual'}

\item{pos}{positon  1}
}
\value{
result table, biplot, dimension
}
\description{
principal component factor analysis function
}
\examples{
\dontrun{
R= matrix(
c(1.00, 0.83, 0.78, 0.70, 0.66, 0.63,
  0.83, 1.00, 0.67, 0.67, 0.65, 0.57,
  0.78, 0.67, 1.00, 0.64, 0.54, 0.51,
  0.70, 0.67, 0.64, 1.00, 0.45, 0.51,
  0.66, 0.65, 0.54, 0.45, 1.00, 0.40,
  0.63, 0.57, 0.51, 0.51, 0.40,1.00),
 byrow=T, nrow=6)
subject=c("classic","france","english","math","pitch","music")

colnames(R)=subject
rownames(R)=subject
R
pcfa(R, nfactor = 2)

library(MVT)
data(examScor)

#if you when input cor
pcfa(cor(examScor), nfactor = 2)

## if you When input data
pcfa( examScor, nfactor = 2)

}

}
