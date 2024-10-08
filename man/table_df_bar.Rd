% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_df_bar.R
\name{table_df_bar}
\alias{table_df_bar}
\title{table_df_bar grap}
\usage{
table_df_bar(
  df,
  size.x = 20,
  size.text = 6,
  add_range = 100,
  x.title = "Items",
  flip = TRUE,
  hjust = -0.1,
  vjust = -0.5,
  x11_width = 8,
  x11_height = 5,
  linetype = "dashed",
  linecolor = "gray50",
  show_line = FALSE,
  sort = FALSE
)
}
\arguments{
\item{df}{table_df res}

\item{size.x}{size.x =14  axis text}

\item{size.text}{size text label 4.5}

\item{add_range}{graph ragne +150}

\item{x.title}{x title}

\item{flip}{flip FALSE}

\item{hjust}{hjust -0.1}

\item{vjust}{vjust -0.5}

\item{x11_width}{x11_width 10}

\item{x11_height}{x11_height 7}

\item{show_line}{show_line, default FALSE}

\item{sort}{FALSE (arrange graph )}
}
\value{
graph
}
\description{
table_df_bar grap
}
\examples{
\dontrun{
#'
table_df_multit(eduteck2 \%>\% select(contains("Q4_")))\%>\%
  table_df_bar(sort=TRUE)
}

}
