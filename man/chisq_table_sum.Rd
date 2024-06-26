% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chisq_table_sum.R
\name{chisq_table_sum}
\alias{chisq_table_sum}
\title{chisq table resize}
\usage{
chisq_table_sum(data, ..., trans = FALSE, sep = "_")
}
\arguments{
\item{data}{mat}

\item{...}{con 1,1,2,2}

\item{trans}{matr transpose}

\item{sep}{sep rowname combine  "-"}
}
\value{
mat
}
\description{
chisq table resize
}
\examples{

\dontrun{
#'
# Test data
data <- data.frame(T = c(30, 40, 60, 120),
                   S = c(50, 70, 60, 80))
rownames(data) <- LETTERS[1:4]
data <- as.matrix(data)

# Example usage
print(chisq_table_sum(data, 1, 1, 2, 2))

chisq_table_sum(data, 1, 1, 2, 2)
chisq_table_sum(data, con = c(1, 1, 2, 2))
chisq_table_sum(data, la = c(1, 1, 2, 2))

}



}
