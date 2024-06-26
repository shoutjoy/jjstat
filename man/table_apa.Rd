% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apa_table.R
\name{table_apa}
\alias{table_apa}
\title{Report frequency analysis result}
\usage{
table_apa(data, print = TRUE, md = FALSE, unit = "명")
}
\arguments{
\item{data}{table_df objecte}

\item{print}{consolt outpur}

\item{md}{markdown_table output}

\item{unit}{unit ="명" if you want chage unit}
}
\value{
report data apa
}
\description{
Report frequency analysis result
}
\examples{

\dontrun{

sats \%>\%table_df(digits=3) \%>\% table_apa()
sats \%>\%table_df(digits=3) \%>\% table_apa(print=F)
sats \%>\%table_df(digits=3) \%>\% table_apa(print=T, md= T)
}

}
