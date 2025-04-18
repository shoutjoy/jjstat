% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fei_dv_res.R
\name{fei_dv_res}
\alias{fei_dv_res}
\title{T종속변수의 빈도를 세는 함수 (에듀테크 연구보고용 )}
\usage{
fei_dv_res(
  df,
  title = "",
  x_size = 20,
  label_text = 5,
  x_range = 100,
  type = "res"
)
}
\arguments{
\item{df}{df}

\item{title}{변수명}

\item{x_size}{20 사이즈로 조절}

\item{label_text}{막대그래프 끄트}

\item{x_range}{범위조절}

\item{type}{res all, freq, console, md, gg, graph na_Count, cleandata}
}
\value{
df
}
\description{
T종속변수의 빈도를 세는 함수 (에듀테크 연구보고용 )
}
\examples{
\dontrun{

eduteck_s$Q14_5 \%>\% fei_dv_res("자기주도 공부력 향상")


}
}
