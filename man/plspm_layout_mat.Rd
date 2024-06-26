% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plspm_layout_mat.R
\name{plspm_layout_mat}
\alias{plspm_layout_mat}
\title{layout_mat}
\usage{
plspm_layout_mat(semplot, text = TRUE, byrow = TRUE, variable = TRUE)
}
\arguments{
\item{semplot}{semPaths2 data}

\item{text}{text True output text matrix}

\item{byrow}{byrow TRUE}

\item{variable}{variable is rownames adapt}
}
\value{
data
}
\description{
layout_mat
}
\examples{


\dontrun{
#'
data(offense)
offense \%>\%str()

nfl_path = matrix(
  c(0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 1, 1, 0, 0,
    1, 0, 0, 1, 0),
  nrow = 5, ncol = 5, byrow = TRUE )

rownames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
colnames(nfl_path) <- c("Speical", "Rushing", "Passing", "Offense", "Scoring")
nfl_path

nfl_blocks = list(7:8, 1:3, 4:6,1:6, 9:11)

plspm_blocks_match(data=offense, blocks=nfl_blocks,
                   paths=nfl_path)

nfl_mode =c("B","A","A","A","A")

nfl_pls = plspm(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)
nfl_pls\%>\%summary()

#########
nfl_pls_boot = plspm_sem(Data=offense, nfl_path, nfl_blocks, modes = nfl_mode)

#Layouts extracted from mockups
plspm_semPaths2(nfl_pls_boot,sizeLat = 6)\%>\%
  plspm_factor_layout()

#Make it input material
plspm_semPaths2(nfl_pls_boot,sizeLat =6)\%>\%
  plspm_factor_layout() \%>\%plspm_layout_mat(text=T)

#Organizing materials for input
plspm_semPaths2(nfl_pls_boot,sizeLat =6)\%>\%
  plspm_factor_layout() \%>\%plspm_layout_mat()\%>\%
  mat_text_arrange()
}

}
