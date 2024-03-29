% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_stat.R
\name{report_stat}
\alias{report_stat}
\title{report nomality , t.test , var.test}
\usage{
report_stat(data, type = "normality", digits = 3, trans = FALSE, show = "data")
}
\arguments{
\item{data}{test result}

\item{type}{normality, t.test, var.test(leven)}

\item{digits}{default 3}

\item{trans}{translate TRUE korean language}

\item{show}{normal is source and output, data is output}
}
\value{
report data
}
\description{
report nomality , t.test , var.test
}
\examples{
\dontrun{

shapiro.test(final) \%>\% report_stat()

var.test(mpg ~ vs, data = mtcars)\%>\%
report_stat("var.test")

car::leveneTest(mpg~ factor(vs), mtcars)\%>\%
 report_stat("leven.test")

car::leveneTest(mpg~ factor(vs), mtcars)\%>\%
report_stat("leven.test", trans=TRUE)


fligner.test(mpg ~ vs,data= mtcars) \%>\%
report_stat("t.test")

t.test(mpg ~ vs, data= mtcars, var.equal = FALSE)\%>\%
report_stat("t.test")

t.test(len ~ supp, data = ToothGrowth, var.equal=TRUE) \%>\%
report_stat("t.test")

shapiro.test(final) \%>\% report_stat()\%>\% jjstat::k("en","ko")
shapiro.test(final) \%>\% report_stat(trans =TRUE)

car::leveneTest(mpg~ factor(vs), mtcars)\%>\%
report_stat("leven.test", trans=TRUE)


}

}
