% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plspm_grp_comp_apa.R
\name{plspm_grp_comp_apa}
\alias{plspm_grp_comp_apa}
\title{plspm_grp_comp_apa: Generate APA-style Group Comparison Descriptions}
\usage{
plspm_grp_comp_apa(df, edteck = TRUE)
}
\arguments{
\item{df}{A data frame containing the group comparison results. It should have the columns:
- `comp`: Group comparison identifiers (e.g., "1_vs_2").
- `Paths`: The path being analyzed.
- `t`: The t-value for the comparison.
- `p_value`: The p-value for the comparison.
- `df`: Degrees of freedom for the t-test.
- `sig`: Significance indicator ("yes" if significant, otherwise "no").}

\item{edteck}{Logical. If `TRUE`, predefined group names are used for educational technology profiles.
If `FALSE`, group names are auto-generated as "그룹1", "그룹2", etc.}
}
\value{
None. The function prints APA-style descriptions for significant group comparisons.
}
\description{
This function generates APA-style text descriptions for PLS-PM group comparisons.
}
\examples{
\dontrun{
df <- data.frame(
  comp = c("1_vs_2", "2_vs_3"),
  Paths = c("Engagement", "Performance"),
  t = c(3.21, 2.45),
  p_value = c(0.0023, 0.0184),
  df = c(45, 50),
  sig = c("yes", "no")
)
plspm_grp_comp_apa(df, edteck = TRUE)
}
}
