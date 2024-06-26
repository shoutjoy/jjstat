% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sem_diagram.R
\name{diagram2}
\alias{diagram2}
\title{diagram sem plot 2}
\usage{
diagram2(
  lav_obj,
  whatLabels = "est",
  rotation = 2,
  nDigits = 2,
  residScale = 18,
  mar = c(2, 8, 3, 8),
  edge.label.cex = 1,
  edge.label.position = 0.5,
  sizeMan = 6,
  sizeMan2 = 4,
  sizeLat = 8,
  sizeLat2 = 6,
  border.width = 2,
  edge.width = 1.5,
  style = "lisrel",
  sig = TRUE,
  asize = 1.5,
  shapeLat = "circle",
  residuals = FALSE,
  curve = 1,
  layout = "tree",
  exoVar = TRUE,
  exoCov = TRUE,
  structual = FALSE,
  nodeLabels = NULL,
  groups = FALSE
)
}
\arguments{
\item{lav_obj}{lavaan}

\item{whatLabels}{est, std, model(sig=FALSE)}

\item{rotation}{rotation= 4}

\item{nDigits}{3}

\item{residScale}{residScale=18}

\item{mar}{c(2,8,3,10)}

\item{edge.label.cex}{edge.label.cex= 1}

\item{edge.label.position}{edge.label.position= 0.5}

\item{sizeMan}{sizeMan = 8,}

\item{sizeMan2}{sizeMan2 = 4,}

\item{sizeLat}{sizeLat = 10,}

\item{sizeLat2}{sizeLat2 = 6}

\item{border.width}{border.width 2}

\item{edge.width}{edge.width 1.5}

\item{style}{style="lisrel"}

\item{sig}{sig=TRUE}

\item{asize}{asize = 1.5}

\item{shapeLat}{shapeLat ="circle",ellipse}

\item{residuals}{residuals=TRUE}

\item{curve}{curve=1}

\item{layout}{"tree"}

\item{exoVar}{exoVar=TRUE,}

\item{exoCov}{exoCov=TRUE,}

\item{structual}{structual=FALSE}

\item{nodeLabels}{nodeLabels=NULL}

\item{groups}{true color}
}
\value{
plot
}
\description{
diagram sem plot 2
}
\examples{

\dontrun{

example(sem)
fit\%>\% diagram2()
#'
fit \%>\%diagram2(mar=c(3,8,3,6), sizeLat = 6, sizeMan = 6,
                edge.label.cex = 0.7)

#model
fit \%>\%diagram2("model", sig=F)

fit \%>\%diagram2("model", sig=F, residuals=F, curve=2)

fit \%>\%diagram2("model", sig=F, residuals=F, curve=2, rotation = 4)

#making method layout
make_layout(nc=10)
make_layout(nc=10, nr=6)
make_layout(3,6)

lay1 = make_layout(6)
lay1

edit_layout(lay1)
lay1 \%>\%edit_layout()
lay2 = lay1 \%>\%edit_layout()
lay2

}


}
