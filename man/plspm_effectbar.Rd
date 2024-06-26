% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plspm_effectbar.R
\name{plspm_effectbar}
\alias{plspm_effectbar}
\title{plspm effect bar graph}
\usage{
plspm_effectbar(
  x_pls,
  axis_x = 1,
  axis_y = 1.1,
  mar = c(axis_x * 10, 3, 1, 0.5),
  border = 1,
  ylim = c(0, 1),
  col = c("#9E9AC8", "#DADAEB")
)
}
\arguments{
\item{x_pls}{plspm result data}

\item{axis_x}{axis text size}

\item{axis_y}{y axis size}

\item{mar}{mar = c(cex.names*11, 3, 1, 0.5)}

\item{border}{1}

\item{ylim}{default c(0,1)}

\item{col}{NA bar color 1:black, 2 redc("#9E9AC8", "#DADAEB")}
}
\value{
data
}
\description{
plspm effect bar graph
}
\examples{

\dontrun{
#'
dftest = data.frame(relationships = c("자기효능감 -> 진로동기",
"자기효능감 -> 진로태도성숙",
 "자기효능감 -> 진로준비행동",
"진로동기 -> 진로태도성숙", "진로동기 -> 진로준비행동",
"진로태도성숙 -> 진로준비행"동),
 direct = c(0.883044251414839, 0.455400556420822,
  0.414089720886173, 0, 0.39512331888417,
  0.115100737249484),
 indirect = c(0, 0, 0.40132831512848, 0, 0, 0),
 total = c(0.883044251414839, 0.455400556420822,
  0.815418036014653, 0, 0.39512331888417,
  0.115100737249484)
)
dftest\%>\%plspm_effectbar(axis_x=1.2)
dftest\%>\%plspm_effectbar(axis_x=2)
jutpls \%>\%plspm_effectbar(axis_x=1.2, border=1)
jutpls \%>\%plspm_effectbar(axis_x=1.2, col=c("gray30","gray70"))
#' total
}

}
