% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/msdn_contrast.R
\name{contrast_msdn}
\alias{contrast_msdn}
\title{Contrast test with mean, standard deviation, and number of samples}
\usage{
contrast_msdn(M, sd, n, ..., digits = 3, type = "res", meanadd = TRUE)
}
\arguments{
\item{M}{means M = c(26.66, 19.74, 15.10)}

\item{sd}{sd  sd = c(4.51, 1.45, 2.56)}

\item{n}{sample  n = c(11, 7, 14)}

\item{...}{contrast c(1, -1,  0), c(1,  0, -1), c(0,  1, -1) ....}

\item{digits}{default 3}

\item{type}{res, res_ci}

\item{meanadd}{include means}
}
\value{
conrast result
}
\description{
Contrast test with mean, standard deviation, and number of samples
}
\examples{
\dontrun{


contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              c(1, -1, 0) )

contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              c(1, -1, 0) ,
              meanadd = FALSE)

contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              c(1, -1, 0) ,
             meanadd = FALSE,
              type="res_ci")


contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
             n = c(11, 7, 14),
              c(1, -1,  0),
              c(1,  0, -1),
              c(0,  1, -1)
)
contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
              contr1 = c(1, -1,  0),
              contr2= c(1,  0, -1),
              contr3= c(0,  1, -1),
              meanadd=F
)
contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
              n = c(11, 7, 14),
             contr1 = c(1, -1,  0),
             contr2= c(1,  0, -1),
              contr3= c(0,  1, -1),
              meanadd=FALSE, type="res_ci"
)
#error
contrast_msdn(M = c(26.66, 19.74, 15.10),
              sd = c(4.51, 1.45, 2.56),
             n = c(11, 7, 14),
              contr = list(c(1, -1,  0),
                            c(1,  0, -1),
                           c(0,  1, -1)
              ))


}
}
