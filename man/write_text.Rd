% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_text.R
\name{write_text}
\alias{write_text}
\title{write_text: Save R Object as Text File and Provide Read Instructions}
\usage{
write_text(data, file_path = NULL, folder_open = TRUE)
}
\arguments{
\item{data}{Any R object to be saved as a text file.}

\item{file_path}{Optional. A string specifying the file path where the text file will be saved.
If not provided, the file name is derived from the object's name with ".text" extension.}

\item{folder_open}{Logical indicating whether to open the folder containing the saved file. Defaults to TRUE.}
}
\value{
NULL. The function saves a text file and optionally opens the folder where it is saved.
}
\description{
write_text: Save R Object as Text File and Provide Read Instructions
}
\examples{
\dontrun{
# Example: Save a summary of a linear model
lmres <- lm(mpg ~ hp + wt, data = mtcars) \%>\% summary()
write_text(lmres)
#'
#'
lmres = lm(mpg~ hp+wt, data=mtcars) \%>\% summary()
write_text(lmres)

# resultlm <- read_text("F:/Rwork/07 futureDuck/2024FEI/lmres.text")
# resultlm
resultlm <- readLines("F:/Rwork/07 futureDuck/2024FEI/lmres.text")


}
}
