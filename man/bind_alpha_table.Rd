% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind_alpha_table.R
\name{bind_alpha_table}
\alias{bind_alpha_table}
\title{Alpha Table Combination}
\usage{
bind_alpha_table(data, ..., check.keys = TRUE)
}
\arguments{
\item{data}{Data frame containing the variables}

\item{...}{Variable names to be included in the alpha table, column name and number. Variable names to be included in the alpha table}

\item{check.keys}{check.keys=TRUE}
}
\value{
alpha table
}
\description{
Alpha Table Combination
}
\examples{
\dontrun{
## select variable
bind_alpha_table(data = stat_onl, c("S_Review", "S_Add_learn", "S_Feedback"))

#error
bind_alpha_table(data = stat_onl, c(S_Review, S_Add_learn, S_Feedback))

#select col number
bind_alpha_table(data = stat_onl, c(9:11))

# mutilple data
bind_alpha_table( stat_onl,
                  c("S_Review", "S_Add_learn", "S_Feedback"),
                  c("SE_Place", "SE_Time"),
                  c("On_Satisfy", "On_Joy", "On_Easy", "On_Engage"),
                  c("upgrade","satisfy","fatigue_inverse") )


#mixed multiple
bind_alpha_table( stat_onl,
                  c("S_Review", "S_Add_learn", "S_Feedback"),
                  c("SE_Place", "SE_Time"),
                  c("On_Satisfy", "On_Joy", "On_Easy", "On_Engage"),
                  c(5:6, 21) )




}

}
