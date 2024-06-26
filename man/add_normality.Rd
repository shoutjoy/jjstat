% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_normality.R
\name{add_normality}
\alias{add_normality}
\title{Add skewness and kurtosis normality checks to descriptive statistics results}
\usage{
add_normality(data, skew = "Skew", kurt = "Kurt")
}
\arguments{
\item{data}{mysummary data}

\item{skew}{select skew}

\item{kurt}{select kurt}
}
\value{
normalitydata
}
\description{
Add skewness and kurtosis normality checks to descriptive statistics results
}
\examples{

\dontrun{

mysummary(mtcars)
mysummary(mtcars)\%>\%add_normality()


}

}
