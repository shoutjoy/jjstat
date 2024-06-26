% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alpha_table.R
\name{alpha_table}
\alias{alpha_table}
\title{Function to tabulate reliability}
\usage{
alpha_table(
  alpha_data_res,
  title = "",
  show = "data",
  format = "markdown",
  digits = 3,
  check.keys = TRUE,
  variable = "."
)
}
\arguments{
\item{alpha_data_res}{alpha result}

\item{title}{your title}

\item{show}{'data', 'markdown'}

\item{format}{markdown, html}

\item{digits}{default 3}

\item{check.keys}{check.keys=TRUE}

\item{variable}{. output}
}
\description{
Function to tabulate reliability
}
\examples{
\dontrun{

stat_onl \%>\% select(S_Review, S_Add_learn, S_Feedback) \%>\%
   jjstat::alpha_table(show="data", variable = "A" )


bind_rows(
 stat_onl \%>\% select(S_Review, S_Add_learn, S_Feedback) \%>\%
   jjstat::alpha_table(show="data", variable = "A" ),
 stat_onl \%>\% select(SE_Place, SE_Time) \%>\%
   jjstat::alpha_table(show="data", variable = "B"),
 stat_onl \%>\% select(On_Satisfy, On_Joy, On_Easy, On_Engage) \%>\%
   jjstat::alpha_table(show="data", variable = "C"),
 stat_onl \%>\% select(upgrade,satisfy,fatigue_inverse) \%>\%
   jjstat::alpha_table(show="data", variable = "D")
) |> markdown_table(digits=3, font_size = 12,
                   caption= "total Cronbach alpha")
}




}
