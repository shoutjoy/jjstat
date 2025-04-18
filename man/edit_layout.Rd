% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edit_layout.R
\name{edit_layout}
\alias{edit_layout}
\title{Edit an Existing Layout Matrix}
\usage{
edit_layout(data)
}
\arguments{
\item{data}{A matrix or data frame to be edited interactively.}
}
\value{
A matrix or data frame representing the edited layout.
}
\description{
Edit an Existing Layout Matrix
}
\examples{
\dontrun{
layout <- matrix(1:9, ncol = 3)
edited_layout <- edit_layout(layout)
print(edited_layout)
}
}
