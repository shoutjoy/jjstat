% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msdn-ttest.R
\name{msdn_ttest}
\alias{msdn_ttest}
\title{Using ttest with mean, sd, and n}
\usage{
msdn_ttest(
  n1,
  m1,
  sd1,
  n2,
  m2,
  sd2,
  var1 = NULL,
  var2 = NULL,
  var.equal = FALSE,
  digits = 2
)
}
\arguments{
\item{n1}{sample1}

\item{m1}{mean1}

\item{sd1}{sd1}

\item{n2}{n2}

\item{m2}{m2}

\item{sd2}{sd2}

\item{var1}{var1 name}

\item{var2}{var2 name}

\item{var.equal}{TRUE Student FALSE, whech}

\item{digits}{3}
}
\value{
ttest result
}
\description{
Using ttest with mean, sd, and n
}
\examples{

\dontrun{
#'
# # Example usage
msdn_ttest(n1 = 603, m1 = 59.6, sd1 = 0.6,
           n2 = 10785, m2 = 46.7, sd2 = 0.3)

#  mysummaryBy(mpg ~ vs, data = mtcars)

# library(jjstat)
msdn_ttest(n1 = 18, m1 = 16.617, sd1 = 3.8607,
           n2 = 14, m2 = 24.557, sd2 = 5.3790,  var.equal=FALSE)
t.test(mpg ~ vs, data = mtcars, var.equal=F)\%>\%tidy()\%>\%dall

msdn_ttest(n1 = 18, m1 = 16.617, sd1 = 3.8607,
           n2 = 14, m2 = 24.557, sd2 = 5.3790, var.equal=TRUE)
t.test(mpg ~ vs, data = mtcars, var.equal=T)\%>\%tidy()

#'
msdn_ttest(n1 = 18, m1 = 16.617, sd1 = 3.8607, var1 = "vs0",
           n2 = 14, m2 = 24.557, sd2 = 5.3790, var2 = "vs1", var.equal = FALSE)
msdn_ttest(n1 = 18, m1 = 16.617, sd1 = 3.8607, var1 = "vs1",
           n2 = 14, m2 = 24.557, sd2 = 5.3790, var2 = "vs2", var.equal = FALSE)


}

}
