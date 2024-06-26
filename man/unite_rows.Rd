% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unite_rows.R
\name{unite_rows}
\alias{unite_rows}
\title{행을 결합해주는 함수}
\usage{
unite_rows(data, row1, row2, sep = ", ", left = "[", right = "]")
}
\arguments{
\item{data}{data}

\item{row1}{row1 합쳐질 곳}

\item{row2}{row2 가져올 곳}

\item{sep}{sep "," default}

\item{left}{left "["}

\item{right}{right"]"}
}
\value{
data
}
\description{
행을 결합해주는 함수
}
\examples{

\dontrun{
#'
data17 <- data.frame(
  criterian = c("Chisq", "df", "p >0.05", "RMSEA <0.05",
                "90\%CI.lower", "90\%CI.upper", "p <= 0.05",
                "SRMR <0.08", "GFI >0.95", "CFI >0.95", "TLI >0.90", "lower", "lower"),
  Value = c(40.179, 38.000, 0.374, 0.028, 0.000, 0.087,
            0.665, 0.056, 0.920, 0.997, 0.995, 3153.636, 3218.526)
)
data17
# 함수 실행 예시
unite_rows(data17, row1 = 5, row2 = 6)
unite_rows(data17, row1 = 5, row2 = 6)\%>\%move_row(6,5)

mtcars[1:5,]
mtcars[1:5,] \%>\%unite_rows(1,3)
}



}
