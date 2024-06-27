#' lpa_explore_modelfit
#'
#' @param df data
#' @param n_profiles_range 1:9
#' @param modelNames "EEV","EEI","EEE","EII"
#'
#' @return
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' df <- select(iris, -Species)
#' lpa_explore_modelfit(df, 1:4)
#' lpa_explore_modelfit(df, 1:9)
#'
#' }
#'
lpa_explore_modelfit <- function(df, n_profiles_range = 1:9, #profile n
                               modelNames =c("EEV","EEI","EEE","EII")) {
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes) #Additional Themes and Theme Components for 'ggplot2'

  x <- mclustBIC(df, G = n_profiles_range, modelNames = modelNames)
  y <- x %>%
    as.data.frame.matrix() %>%
    rownames_to_column("n_profiles") %>%
    rename(`[EEV:ellipsoidal, equal volume and equal shape]`=EEV,
           `[EEI:diagonal, equal volume and shape]` = EEI,
           # `diagonal, varying volume and shape` = VVI,
           `[EEE:ellipsoidal, equal volume, shape, and orientation]` = EEE,
           # `ellipsoidal, varying volume, shape, and orientation` = VVV
           `[EII:spherical, equal volume]` = EII
    )

  y
}
