% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lav_extract_paths.R
\name{lav_extract_ind_paths}
\alias{lav_extract_ind_paths}
\title{lav_extract_ind_paths, Create an indirect effect hypothesis}
\usage{
lav_extract_ind_paths(
  paths_data,
  first_node = NULL,
  paths_name = FALSE,
  allpaths = FALSE
)
}
\arguments{
\item{paths_data}{direct path}

\item{first_node}{start node name}

\item{paths_name}{TRUE hypothesis output}

\item{allpaths}{all and ind(FALSE)}
}
\value{
text
}
\description{
lav_extract_ind_paths, Create an indirect effect hypothesis
}
\examples{

\dontrun{
#'
# 예시 데이터
paths_data <- data.frame(
  hypo = c("H1", "H2", "H3", "H4", "H5",
  "H6", "H7", "H8", "H9", "H10"),
  paths = c("IMAG->EXPE", "EXPE->QUAL",
  "QUAL->VAL", "EXPE->VAL", "IMAG->SAT",
   "EXPE->SAT", "QUAL->SAT", "VAL->SAT", "SAT->LOY", "IMAG->LOY")
)
# 함수 실행
lav_extract_ind_paths(paths_data, allpaths = TRUE)
lav_extract_ind_paths(paths_data, allpaths = FALSE)
#'
lav_extract_ind_paths(paths_data, allpaths = FALSE, paths_name = TRUE)
lav_extract_ind_paths(paths_data, allpaths = FALSE, paths_name = FALSE)
#'
lav_extract_path(model1a)\%>\%lav_extract_ind_paths()

lav_extract_path(model1)\%>\%lav_extract_ind_paths()
#'
lav_extract_path(model1a) \%>\%
  lav_extract_ind_paths() \%>\%
  sem_lav_ind_hypo()
#'
## 구조모형에서 나타나는 간접효과 가설은 다음과 같다.
## 간접효과[H1]: 간접효과 IMAG -> SAT ->
LOY 는 통계적으로 유의할 것이다.
## 간접효과[H2]: 간접효과 IMAG -> EXPE -
> SAT -> LOY 는 통계적으로 유의할 것이다.
## 간접효과[H3]: 간접효과 IMAG -> EXPE -
> VAL -> SAT -> LOY 는 통계적으로 유의할 것이다.
## 간접효과[H4]: 간접효과 IMAG -> EXPE
-> QUAL -> SAT -> LOY 는 통계적으로 유의할 것이다.
## 간접효과[H5]: 간접효과 IMAG -> EXPE
-> QUAL -> VAL -> SAT -> LOY 는 통계적으로 유의할 것이다.
#' #'
#'

# Example usage with the provided model
model10 <- "
IMAG =~ imag1 + imag2 + imag3 + imag4 + imag5
EXPE =~ expe1 + expe2 + expe3 + expe4 + expe5
QUAL =~ qual1 + qual2 + qual3 + qual4 + qual5
VAL =~ val1 + val2 + val3 + val4
SAT =~ sat1 + sat2 + sat3 + sat4
LOY =~ loy1 + loy2 + loy3 + loy4

EXPE ~ IMAG
QUAL ~ EXPE
VAL ~  QUAL+ EXPE
SAT ~ IMAG + EXPE + QUAL + VAL
LOY ~ SAT + IMAG
"


# Extract paths from the model
lav_extract_path(model10)
lav_extract_path(model10)\%>\%lav_extract_ind_paths()

lav_extract_sm(model10)\%>\%lav_extract_path()\%>\%lav_extract_ind_paths()
lav_extract_sm(model10)\%>\%lav_extract_ind_paths()
lav_extract_sm(model10)\%>\%lav_extract_ind_paths("EXPE")

}

}
