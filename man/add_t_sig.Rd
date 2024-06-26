% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_t_sig.R
\name{add_t_sig}
\alias{add_t_sig}
\title{add_t_sig}
\usage{
add_t_sig(
  data,
  est,
  se,
  col = ncol(data) + 1,
  unite = FALSE,
  digits = 3,
  ns = " ns"
)
}
\arguments{
\item{data}{data}

\item{col}{position t value}

\item{unite}{unite sig star +t}

\item{digits}{3}

\item{ns}{ns is ns sig}
}
\value{
data
}
\description{
add_t_sig
}
\examples{

\dontrun{
#'
New_df = data.frame(rownames = c('IMAG -> EXPE', 'IMAG -> QUAL', 'IMAG -> VAL', 'IMAG -> SAT'),
  Original = c(0.578959128452789, 0.491156739303251, 0.393411171344413, 0.490971137879492),
 Mean.Boot = c(0.584251828061525, 0.495822941940513, 0.400070801457516, 0.5022633230577),
 Std.Error = c(0.0518709126012371, 0.0510037451930923, 0.0528454527326974, 0.0681551639743143),
 perc.025 = c(0.477709787839589, 0.391876781940297, 0.308196019466015, 0.369649328752085),
  perc.975 = c(0.664779330969341, 0.58560953201285, 0.495717252789497, 0.614490783702303)
)
#Auto-create a separate T without creating a SIG and combine it with the SIG to move it
New_df \%>\% add_t_sig(est=3, se=4, col=4, unite=T)


# Move only sigs after creating a t
New_df \%>\% add_t_sig(est=3, se=4, col=4, unite=F)

# 열을 결합
New_df \%>\% add_t_sig(est=3, se=4, col=4, unite=F) \%>\%Unite(2,4)

New_df \%>\% add_t_sig(est=3, se=4, col=4, unite=F)\%>\%Round(3) \%>\%Unite(2,4)

}
}
