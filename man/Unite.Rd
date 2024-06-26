% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Unite.R
\name{Unite}
\alias{Unite}
\title{UInte data}
\usage{
Unite(data, col1, col2, Col = NULL, sep = "", remove = TRUE)
}
\arguments{
\item{data}{data}

\item{col1}{select col1}

\item{col2}{select col2}

\item{Col}{col names}

\item{sep}{""}

\item{remove}{true}

\item{left}{" or "["}

\item{right}{""  or  "]"}
}
\value{
data
}
\description{
UInte data
}
\examples{
\dontrun{
#'
# 예시 데이터 생성
data <- data.frame(
  block = c("IMAG", "IMAG", "IMAG", "IMAG"),
  name = c("imag1", "imag2", "imag3", "imag4"),
  Original = c("0.7093265 ***", "0.8773078 ***", "0.8417485 ***", "0.5691934 ***"),
  sig = c("***", "***", "***", "***"),
  Mean.Boot = c(0.7088215, 0.8750787, 0.8401757, 0.5647333),
  Std.Error = c(0.05949724, 0.02402834, 0.02970117, 0.07686419),
  perc.025 = c(0.5774529, 0.8231642, 0.7784983, 0.3876417),
  perc.975 = c(0.8105249, 0.9146114, 0.8863100, 0.6961572)
)

# 예시 호출

# Unite using column numbers
result1 <- Unite(data, 3, 4)
print(result1)

# Unite using column numbers with a new column name
result2 <- Unite(data, 3, 4, "New")
print(result2)

# Unite using column names
result3 <- Unite(data, "Original", "sig")
print(result3)

# Unite using column names with a new column name
result4 <- Unite(data, "Original", "sig", "New")
print(result4)


}
#
}
