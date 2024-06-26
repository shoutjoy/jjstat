% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_print.R
\name{cut_print}
\alias{cut_print}
\title{Functions that cutoff a given value}
\usage{
cut_print(
  data,
  cut = 0,
  digits = 3,
  rm_row = FALSE,
  rm_cut = FALSE,
  star = FALSE,
  each = NULL
)
}
\arguments{
\item{data}{data.frame}

\item{cut}{cutoff  default 0}

\item{digits}{round 3}

\item{rm_row}{remove row}

\item{rm_cut}{remove cutoff rows}

\item{star}{over cut marked star pasted}

\item{each}{each col using cut}
}
\value{
data
}
\description{
Functions that cutoff a given value
}
\examples{

\dontrun{
#'
mtcars\%>\% cut_print(cut = 280)
mtcars\%>\% cut_print(cut = 280, rm_row = T)
mtcars\%>\% cut_print(cut = 280, rm_row = T,rm_cut=T)

#'
# 예제 데이터
data <- data.frame(
  name = c("C_S1", "C_S2", "C_S3", "C_S4", "C_S5", "A_M1", "A_M2",
           "A_M3", "B_A2", "B_A3", "D_P1", "D_P2", "D_P3"),
  block = c("자기효능감", "자기효능감", "자기효능감", "자기효능감",
            "자기효능감", "진로동기", "진로동기", "진로동기",
            "진로태도", "진로태도", "진로준비", "진로준비", "진로준비"),
  자기효능감 = c(0.88334202, 0.82571836, 0.79357537, 0.78069220,
            0.63356092, 0.79936329, 0.75998517, 0.66930167, 0.08384875,
            0.67950207, 0.75093254, 0.70621491, 0.33677926),
  진로동기 = c(0.819634450, 0.756922172, 0.732994108, 0.617655739,
           0.507011139, 0.859130054, 0.851836536, 0.815780379, 0.006420011,
           0.699152545, 0.769048149, 0.688524060, 0.336711208),
  진로태도 = c(0.6497353, 0.6010873, 0.5346407, 0.5128086,
           0.3223576, 0.6769730, 0.6030949, 0.4615263, 0.1463340,
           0.9964532, 0.5030203, 0.6625537, 0.4041093),
  진로준비 = c(0.76921767, 0.67147592, 0.58862568, 0.63738845,
           0.50527519, 0.69871013, 0.68835534, 0.66810321, 0.03263257,
           0.69001534, 0.86708694, 0.84986139, 0.55547251)
)

# 각 열이 numeric 변수인지 확인
numeric_cols <- sapply(data, is.numeric)
print(numeric_cols)

# 함수 실행 예시
result <- cut_print(data, cut = 0.01, digits = 2, rm_row = FALSE,
                    rm_cut = TRUE, star = FALSE, each = c(0.78, 0.8, 0.8, 0.8))
print(result)

#'
}
}
