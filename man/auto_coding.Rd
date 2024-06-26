% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto_coding.R
\name{auto_coding}
\alias{auto_coding}
\title{Automated Likert scale conversion functions}
\usage{
auto_coding(
  data,
  ...,
  range = 1:ncol(data),
  custom = NULL,
  rev = FALSE,
  data.fram = FALSE
)
}
\arguments{
\item{data}{data.frame, vector}

\item{...}{coding levels}

\item{range}{data col range}
}
\value{
transforamtion data
}
\description{
Automated Likert scale conversion functions
}
\examples{

\dontrun{
#'

# test
jut1_custom <- c("그렇다", "매우 그렇다", "보통이다",
                 "그렇지 않다", "전혀 그렇지 않다")
jut1_custom
jut1_custom\%>\%rev()


jut1_custom <- c("그렇다", "매우 그렇다", "보통이다", "그렇지 않다", "전혀 그렇지 않다")

auto_coding(jut1_custom, custom=jut1_custom)
auto_coding(jut1_custom, custom=jut1_custom, rev=TRUE)


# Example data
vec1 <- c("그렇다", "그렇다", "매우 그렇다", "그렇다", "매우 그렇다", "매우 그렇다",
          "매우 그렇다", "그렇다", "그렇다", "그렇지 않다", "그렇다", "그렇다",
          "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다", "매우 그렇다",
          "전혀 그렇지 않다", "보통이다", "그렇다", "매우 그렇다", "매우 그렇다",
          "보통이다", "그렇다", "매우 그렇다", "그렇다", "그렇다", "매우 그렇다",
          "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다", "보통이다",
          "그렇다", "그렇다", "그렇다", "매우 그렇다", "보통이다", "매우 그렇다",
          "매우 그렇다", "그렇다", "그렇다", "그렇지 않다", "그렇다", "그렇다",
          "그렇다", "보통이다", "그렇다", "그렇다", "보통이다", "그렇지 않다",
          "그렇지 않다", "매우 그렇다", "보통이다", "그렇다", "매우 그렇다",
          "그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다",
          "그렇다", "매우 그렇다", "그렇다", "보통이다", "그렇다", "그렇다",
          "그렇다", "매우 그렇다", "보통이다", "그렇다", "그렇다", "그렇지 않다",
          "그렇지 않다", "그렇지 않다", "그렇지 않다", "매우 그렇다", "매우 그렇다",
          "그렇지 않다", "그렇다", "전혀 그렇지 않다", "그렇다", "그렇다",
          "그렇다", "보통이다", "그렇다", "그렇지 않다", "그렇다", "보통이다",
          "매우 그렇다")

auto_coding(vec1, custom=jut1_custom, rev=FALSE)
auto_coding(vec1, custom=jut1_custom, rev=TRUE)



# Example data
data1 <- data.frame(
  V1 = c("그렇다", "그렇다", "매우 그렇다", "그렇다", "매우 그렇다",
         "매우 그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇지 않다",
         "그렇다", "그렇다", "매우 그렇다", "그렇다", "그렇다",
         "그렇다", "그렇다", "매우 그렇다",  "전혀 그렇지 않다", "보통이다",
         "그렇다", "매우 그렇다", "매우 그렇다", "보통이다", "그렇다",
         "매우 그렇다", "그렇다", "그렇다", "매우 그렇다",   "매우 그렇다",
         "그렇다", "그렇다", "그렇다", "그렇다", "보통이다",
         "그렇다", "그렇다", "그렇다", "매우 그렇다", "보통이다",
         "매우 그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇지 않다",
         "그렇다", "그렇다",
         "그렇다", "보통이다", "그렇다", "그렇다", "보통이다", "그렇지 않다",
         "그렇지 않다", "매우 그렇다", "보통이다", "그렇다", "매우 그렇다",
         "그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다",
         "그렇다", "매우 그렇다", "그렇다", "보통이다", "그렇다", "그렇다",
         "그렇다", "매우 그렇다", "보통이다", "그렇다", "그렇다", "그렇지 않다",
         "그렇지 않다", "그렇지 않다", "그렇지 않다", "매우 그렇다", "매우 그렇다",
         "그렇지 않다", "그렇다", "전혀 그렇지 않다", "그렇다", "그렇다",
         "그렇다", "보통이다", "그렇다", "그렇지 않다", "그렇다", "보통이다",
         "매우 그렇다", "보통이다")
)

auto_coding(data1)
data1

# Example data
data2 <- data.frame(
  V1 = c("그렇다", "그렇다", "매우 그렇다", "그렇다", "매우 그렇다",
         "매우 그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇지 않다",
         "그렇다", "그렇다", "매우 그렇다", "그렇다", "그렇다",
         "그렇다", "그렇다", "매우 그렇다",  "전혀 그렇지 않다", "보통이다",
         "그렇다", "매우 그렇다", "매우 그렇다", "보통이다", "그렇다",
         "매우 그렇다", "그렇다", "그렇다", "매우 그렇다",   "매우 그렇다",
         "그렇다", "그렇다", "그렇다", "그렇다", "보통이다",
         "그렇다", "그렇다", "그렇다", "매우 그렇다", "보통이다",
         "매우 그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇지 않다",
         "그렇다", "그렇다"),
  v2=c( "그렇다", "보통이다", "그렇다", "그렇다", "보통이다", "그렇지 않다",
        "그렇지 않다", "매우 그렇다", "보통이다", "그렇다", "매우 그렇다",
        "그렇다", "매우 그렇다", "그렇다", "그렇다", "그렇다", "그렇다",
        "그렇다", "매우 그렇다", "그렇다", "보통이다", "그렇다", "그렇다",
        "그렇다", "매우 그렇다", "보통이다", "그렇다", "그렇다", "그렇지 않다",
        "그렇지 않다", "그렇지 않다", "그렇지 않다", "매우 그렇다", "매우 그렇다",
        "그렇지 않다", "그렇다", "전혀 그렇지 않다", "그렇다", "그렇다",
        "그렇다", "보통이다", "그렇다", "그렇지 않다", "그렇다", "보통이다",
        "매우 그렇다", "보통이다")
)
data2
auto_coding(data2)


}
}
