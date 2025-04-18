% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Freq_plot_pie_chart.R
\name{plot_pie_chart}
\alias{plot_pie_chart}
\title{pie chart(using fei)}
\usage{
plot_pie_chart(
  data,
  title = "",
  size.text = 5,
  title.size = 20,
  vjust = 0.5,
  palette = "Set2"
)
}
\arguments{
\item{data}{data}

\item{title}{title}

\item{size.text}{5}

\item{title.size}{20}

\item{vjust}{0.5}

\item{palette}{"Set2", "Set3"}
}
\value{
graph
}
\description{
pie chart(using fei)
}
\examples{

\dontrun{
#'
# 예시 데이터
data <- data.frame(
  구분 = c("전혀 아니다", "아니다", "중간이다", "그렇다", "매우 그렇다"),
  빈도 = c(52, 90, 304, 257, 114),
  비율 = c(6.36, 11.02, 37.21, 31.46, 13.95)
)

# 함수 호출
plot_pie_chart(data,,"Q1응답",0.5)
}

}
