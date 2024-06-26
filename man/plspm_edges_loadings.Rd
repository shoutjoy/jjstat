% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plspm_edges_loadings.R
\name{plspm_edges_loadings}
\alias{plspm_edges_loadings}
\title{plspm lodaings significant  vecgtor or df}
\usage{
plspm_edges_loadings(plsres, type = "df")
}
\arguments{
\item{plsres}{plspm result}

\item{type}{df, vec, data.frame, vector}
}
\value{
data
}
\description{
plspm lodaings significant  vecgtor or df
}
\examples{

\dontrun{

# Method 2: paths
sat_path = plspm_paths(
  row_names = c("IMAG","EXPE","QUAL","VAL","SAT","LOY"),
  relationship = list(
    path(from="IMAG", to=c("EXPE","SAT","LOY")),
    path("EXPE", c("QUAL","VAL","SAT")),
    path("QUAL", c("VAL","SAT")),
    path("VAL",c("SAT")),
    path("SAT","LOY")
  )
)
# blokcs
sat_blocks1 <- plspm_blocks(
  IMAG = item(paste0("imag",1:5)),
  EXPE = item(paste0("expe", 1:5)),
  QUAL = item( paste0("qual", 1:5)),
  VAL = item(paste0("val", 1:4)),
  SAT = item(paste0("sat", 1:4)),
  LOY = item(paste0("loy", 1:4))
)
sat_blocks1

# vector of modes (reflective indicators):auto
sat_mod = rep("A", 6)
#############

# apply plspm
satpls = plspm_sem(satisfaction,
                   path_matrix = sat_path,
                   blocks = sat_blocks1,
                   scaled = FALSE)

satpls_boot = plspm_sem(satisfaction, sat_path, sat_blocks1, scaled = FALSE,
                        boot.val =TRUE, br=100)




# plspm_loadings_values
satpls \%>\%plspm_edge_loadings()
satpls \%>\%plspm_edge_loadings("vec")
satpls$boot \%>\%plspm_edge_loadings()
satpls \%>\%plspm_edge_loadings()
satpls$boot$loadings\%>\%plspm_edge_loadings()
satpls$boot$loadings\%>\%plspm_edge_loadings("vec")

}
}
