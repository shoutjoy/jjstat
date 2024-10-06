#' lpa_explore_modelfit
#'
#' @param df data
#' @param n_profiles_range 1:9
#' @param modelNames "EEV", "EEI", "EEE", "EII", "VVI", "VII", "VVV"
#' @param chk default FALSE, TRUE show
#' @param basic select EII, EEE, VII, EEI
#' @param set_n_profile default select row number then result show
#' @param na2zero na2zero is NA data to 0
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
#' #' lpa_explore_modelfit(df, 1:9)
#' lpa_explore_modelfit(pwjlpa,1:4, basic=TRUE)
#' lpa_explore_modelfit(pwjlpa,1:4, basic=TRUE, set_n_profile=4)
#' lpa_explore_modelfit(pwjlpa,1:6, basic=TRUE)
#'
#' lpa_explore_modelfit(pwjlpa,1:6, basic=FALSE)
#' lpa_explore_modelfit(pwjlpa,1:10, basic=TRUE, set_n_profile=4)
#' lpa_explore_modelfit(pwjlpa,1:10, basic=TRUE, set_n_profile=4, chk=TRUE)
#'
#' }
#'
lpa_explore_modelfit <- function(df, n_profiles_range = 1:9,
                                 modelNames = c("EEV", "EEI", "EEE",
                                                "EII", "VVI", "VII", "VVV"),
                                 chk = FALSE,
                                 basic = FALSE,
                                 set_n_profile = NULL,
                                 na2zero = TRUE) {
  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes) # Additional Themes and Theme Components for 'ggplot2'

  # Perform BIC analysis
  x <- mclustBIC(df, G = n_profiles_range, modelNames = modelNames)

  # Convert the BIC results to a data frame and rename the columns
  y <- x %>%
    as.data.frame.matrix() %>%
    rownames_to_column("n_profiles") %>%
    rename(EEV = EEV, EEI = EEI, EEE = EEE, EII = EII, VVI = VVI, VII = VII, VVV = VVV)

  # Round the values to 3 decimal places
  y <- y %>%
    mutate(across(c(EEV, EEI, EEE, EII, VVI, VII, VVV), ~round(.x, 3)))

  # NA를 0으로 변환할지 여부를 na2zero 인자로 처리
  if (na2zero) {
    y <- y %>%
      mutate(across(c(EEV, EEI, EEE, EII, VVI, VII, VVV), ~ifelse(is.na(.x), 0, .x)))
  }

  # General case: calculate minimum values for each model
  eei_min <- min(y$EEI, na.rm = TRUE)
  eee_min <- min(y$EEE, na.rm = TRUE)
  eii_min <- min(y$EII, na.rm = TRUE)
  vii_min <- min(y$VII, na.rm = TRUE)
  eev_min <- if ("EEV" %in% colnames(y)) min(y$EEV, na.rm = TRUE) else NA
  vvi_min <- if ("VVI" %in% colnames(y)) min(y$VVI, na.rm = TRUE) else NA
  vvv_min <- if ("VVV" %in% colnames(y)) min(y$VVV, na.rm = TRUE) else NA

  # Create new _chk columns showing the minimum value or 0 for each model
  y <- y %>%
    mutate(EEI_chk = ifelse(EEI == eei_min, EEI, 0),
           EEE_chk = ifelse(EEE == eee_min, EEE, 0),
           EII_chk = ifelse(EII == eii_min, EII, 0),
           VII_chk = ifelse(VII == vii_min, VII, 0),
           EEV_chk = if ("EEV" %in% colnames(y)) ifelse(EEV == eev_min, EEV, 0) else NA,
           VVI_chk = if ("VVI" %in% colnames(y)) ifelse(VVI == vvi_min, VVI, 0) else NA,
           VVV_chk = if ("VVV" %in% colnames(y)) ifelse(VVV == vvv_min, VVV, 0) else NA)

  # Create a 'final' column that shows the minimum value among the _chk columns
  y <- y %>%
    rowwise() %>%
    mutate(final = min(c_across(ends_with("_chk")), na.rm = TRUE))

  # If set_n_profile is provided, adjust only the row for that profile
  if (!is.null(set_n_profile)) {
    y <- y %>%
      mutate(
        EII_chk = ifelse(n_profiles == set_n_profile, EII, EII_chk),
        EEE_chk = ifelse(n_profiles == set_n_profile, EEE, EEE_chk),
        VII_chk = ifelse(n_profiles == set_n_profile, VII, VII_chk),
        EEI_chk = ifelse(n_profiles == set_n_profile, EEI, EEI_chk)
      ) %>%
      # Recalculate 'final' column for the set_n_profile row
      mutate(final = ifelse(n_profiles == set_n_profile, pmin(EEI_chk, EEE_chk, EII_chk, VII_chk, na.rm = TRUE), final))
  }

  # Create a 'sig' column that indicates the model name of the minimum final value from original columns (not _chk),
  # or "" if final is NA or 0
  y <- y %>%
    rowwise() %>%
    mutate(sig = case_when(
      final == 0 | is.na(final) ~ ".",  # If final is 0 or NA, set sig to "."
      final == EEI_chk ~ "EEI",
      final == EEE_chk ~ "EEE",
      final == EII_chk ~ "EII",
      final == VII_chk ~ "VII",
      final == EEV_chk ~ "EEV",
      final == VVI_chk ~ "VVI",
      final == VVV_chk ~ "VVV",
      TRUE ~ "-"
    )) %>%
    ungroup()

  # If basic = TRUE, select only specified columns
  if (basic) {
    y <- y %>%
      select(n_profiles, EII, EEE, VII, EEI, final, sig)
  }

  # Remove _chk columns if chk is FALSE
  if (!chk) {
    y <- y %>% dplyr::select(-contains("_chk"))
  }

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
