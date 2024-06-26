% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_aov_ko.R
\name{report_aov_ko}
\alias{report_aov_ko}
\title{repot anova}
\usage{
report_aov_ko(data, digits = 2, print = TRUE)
}
\arguments{
\item{data}{aov data}

\item{digits}{3}

\item{print}{table show}
}
\value{
table
}
\description{
repot anova
}
\examples{

\dontrun{
aov(mpg ~ cyl* gear +carb, data=mtcars) \%>\%report_aov_ko()

aov(mpg ~ cyl* gear +carb, data=mtcars) \%>\%report_aov_ko(print=T)
aov(mpg ~ cyl* gear +carb, data=mtcars) \%>\%report_aov_ko(print=F)

}

}
