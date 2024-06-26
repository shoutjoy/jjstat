% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plspm_semptools.R
\name{plspm_boot_factor_layout}
\alias{plspm_boot_factor_layout}
\title{plspm_boot_factor_layout}
\usage{
plspm_boot_factor_layout(
  plsres_boot,
  positions = NULL,
  nrow = 4,
  ncol = 5,
  ...
)
}
\arguments{
\item{plsres_boot}{bootresult}

\item{positions}{position change}

\item{nrow}{4,}

\item{ncol}{5}

\item{...}{add c()}
}
\value{
matrix
}
\description{
plspm_boot_factor_layout
}
\examples{

\dontrun{
#'
# 예시 호출
plsres_boot <- list(model = list(gens = list(
                  lvs_names = c("LV1", "LV2", "LV3", "LV4"))))
result <- plspm_boot_factor_layout(plsres_boot)
print(result)

# 예시 호출
plsres_boot <- list(model =
list(gens = list(
lvs_names = c("LV1", "LV2", "LV3", "LV4", "LV5"))))

plspm_boot_factor_layout(plsres_boot)

# 예시 호출
plsres_boot <- list(model = list(gens =
list(lvs_names = c("LV1", "LV2", "LV3",
"LV4", "LV5", "LV6", "LV7","LV8"))))


plspm_boot_factor_layout(plsres_boot )
###EX)
positions <- list("LV1" = c(2, 3), "LV2" = c(1, 4))

}


}
