% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aov_table.R
\name{aov_table}
\alias{aov_table}
\title{Generate an AOV results table for multiple independent variables}
\usage{
aov_table(data, dv_var, iv_vars, mean = FALSE, unite = FALSE)
}
\arguments{
\item{data}{data.frame}

\item{dv_var}{dv}

\item{iv_vars}{c(iv1, iv2....)}

\item{mean}{Option to average by group and add}

\item{unite}{pvalue unite}
}
\value{
table
}
\description{
Generate an AOV results table for multiple independent variables
}
\examples{

\dontrun{
aov_table(data = mtcars, dv_var = "mpg",
          iv_vars = c("cyl", "gear", "carb"))

aov_table(data = mtcars, dv_var = "mpg",
          iv_vars = c("cyl", "gear", "carb"),
           mean = TRUE)

}


}
