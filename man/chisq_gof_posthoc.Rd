% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chisq_gof_posthoc.R
\name{chisq_gof_posthoc}
\alias{chisq_gof_posthoc}
\title{chisq_gof_posthoc}
\usage{
chisq_gof_posthoc(vector, type = "res", method = "bonferroni", names = TRUE)
}
\arguments{
\item{vector}{data}

\item{type}{all, res, p, chisq}

\item{method}{p.adust fdr, bonferroni, ...}

\item{names}{TRUE using names}
}
\value{
data
}
\description{
chisq_gof_posthoc
}
\examples{

\dontrun{
#'

X <- c(49, 30, 63, 59)

chisq_gof_posthoc(X, "res")
chisq_gof_posthoc(X,"all")$gof_test
chisq_gof_posthoc(X,"all")$p
chisq_gof_posthoc(X,"all")$chisq
chisq_gof_posthoc(X, typ="all")
chisq_gof_posthoc(X, typ="res")
chisq_gof_posthoc(X, type="p")
chisq_gof_posthoc(X, type="chisq")

chisq_gof_posthoc(X, method="bonferroni")

X1 <- c(4, 3, 6, 19)
chisq_gof_posthoc(X1)
chisq_gof_posthoc(X1, typ="all")
chisq_gof_posthoc(X1, type="p")
chisq_gof_posthoc(X1, type="chisq")
chisq_gof_posthoc(X1, method="bonferroni")


chisq_gof_posthoc(X, method="bonferroni")

chisq_gof_posthoc(c(49, 30, 63, 59, 40, 60))

#' #'
}



}
