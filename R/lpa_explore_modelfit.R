#' lpa_explore_modelfit
#'
#' @param df data
#' @param n_profiles_range 1:9
#' @param modelNames "EEV","EEI","EEE","EII"
#'
#' @return data
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
lpa_explore_modelfit <- function(df, n_profiles_range = 1:9, # profile n
                                 modelNames = c("EEV", "EEI", "EEE", "EII","VVI","VII","VVV")) {
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes) # Additional Themes and Theme Components for 'ggplot2'

  # Perform BIC analysis
  x <- mclustBIC(df, G = n_profiles_range, modelNames = modelNames)

  # Convert the BIC results to a data frame and rename the columns
  y <- x %>%
    as.data.frame.matrix() %>%
    rownames_to_column("n_profiles") %>%
    rename(EEV = EEV,
           EEI = EEI,
           EEE = EEE,
           EII = EII,
           VVI = VVI,
           VII = VII,
           VVV = VVV
    )

  # Round the values to 3 decimal places to avoid precision issues
  y <- y %>%
    mutate(across(c(EEV, EEI, EEE, EII, VVI, VII,VVV), ~round(.x, 3))) %>%
    mutate(across(c(EEV, EEI, EEE, EII, VVI, VII,VVV), ~ifelse(is.na(.x), 0, .x)))

  # Find the minimum value for each model
  eev_min <- min(y$EEV, na.rm = TRUE)
  eei_min <- min(y$EEI, na.rm = TRUE)
  eee_min <- min(y$EEE, na.rm = TRUE)
  eii_min <- min(y$EII, na.rm = TRUE)
  vvi_min <- min(y$VVI, na.rm = TRUE)
  vii_min <- min(y$VII, na.rm = TRUE)
  vvv_min <- min(y$VVV, na.rm = TRUE)

  # Create new _chk columns showing the minimum value or 0 for each model
  y <- y %>%
    mutate(EEV_chk = ifelse(EEV == eev_min, EEV, 0),
           EEI_chk = ifelse(EEI == eei_min, EEI, 0),
           EEE_chk = ifelse(EEE == eee_min, EEE, 0),
           EII_chk = ifelse(EII == eii_min, EII, 0),
           VVI_chk = ifelse(VVI == vvi_min, VVI, 0),
           VII_chk = ifelse(VII == vii_min, VII, 0),
           VVV_chk = ifelse(VVV == vvv_min, VVV, 0)
    )

  # Create a 'final' column that shows the minimum value among the _chk columns
  y <- y %>%
    mutate(final = pmin(EEV_chk, EEI_chk, EEE_chk, EII_chk, VVI_chk, VII_chk,VVV_chk))

  # Find the minimum value in 'final' column
  min_final <- min(y$final, na.rm = TRUE)

  # Create a 'sig' column that marks the minimum final value with '*' and others with '-'
  y <- y %>%
    mutate(sig = ifelse(final == min_final, "*", "-"))

  return(y)
}

# lpa_explore_modelfit <- function(df, n_profiles_range = 1:9, #profile n
#                                modelNames =c("EEV","EEI","EEE","EII")) {
#   library(tidyverse, warn.conflicts = FALSE)
#   library(mclust)
#   library(hrbrthemes) #Additional Themes and Theme Components for 'ggplot2'
#
#   x <- mclustBIC(df, G = n_profiles_range, modelNames = modelNames)
#   y <- x %>%
#     as.data.frame.matrix() %>%
#     rownames_to_column("n_profiles") %>%
#     rename(`[EEV:ellipsoidal, equal volume and equal shape]`=EEV,
#            `[EEI:diagonal, equal volume and shape]` = EEI,
#            # `diagonal, varying volume and shape` = VVI,
#            `[EEE:ellipsoidal, equal volume, shape, and orientation]` = EEE,
#            # `ellipsoidal, varying volume, shape, and orientation` = VVV
#            `[EII:spherical, equal volume]` = EII
#     )
#
#   y
# }
