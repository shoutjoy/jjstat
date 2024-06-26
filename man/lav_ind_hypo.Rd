% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lav_hypo.R
\name{lav_ind_hypo}
\alias{lav_ind_hypo}
\title{Function that takes path data as input and outputs an indirect effect hypothesis.}
\usage{
lav_ind_hypo(paths)
}
\arguments{
\item{paths}{indpath}
}
\value{
text
}
\description{
Function that takes path data as input and outputs an indirect effect hypothesis.
}
\examples{

\dontrun{
#'
# 모델 문자열
model1a <- "
EXPE ~ H1*IMAG
QUAL ~ H2*EXPE
VAL ~ H3*QUAL + H4*EXPE
SAT ~ H5*IMAG + H6*EXPE + H7*QUAL + H8*VAL
LOY ~ H9*SAT + H10*IMAG
"

model1 <- "
EXPE ~ IMAG
QUAL ~ EXPE
VAL ~ QUAL + EXPE
SAT ~ IMAG + EXPE + QUAL + VAL
LOY ~ SAT + IMAG
"
lav_extract_path(model1a)\%>\%
  lav_generate_ind_paths() \%>\%
  lav_ind_hypo()

}

}
