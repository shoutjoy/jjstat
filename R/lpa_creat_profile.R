#' lpa_create_profiles
#'
#' @param df df
#' @param n_profiles default = 3
#' @param variance_structure freed
#' @param covariance_structure freed
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#' iris[,-5]%>% lpa_create_profiles(n_profiles =  4,
#'                                     variance_structure = "constrained",
#'                                     covariance_structure = "constrained")
#' }
#'
#'
lpa_create_profiles<- function(df,
                                   n_profiles=3,
                                   variance_structure = "freed",
                                   covariance_structure = "freed"){

  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)

  if (variance_structure == "constrained" &
      covariance_structure == "fixed") {

    model_name <- "EEI"

  } else if (variance_structure == "freed" &
             covariance_structure == "fixed") {

    model_name <- "VVI"

  } else if (variance_structure == "constrained" &
             covariance_structure == "constrained") {

    model_name <- "EEE"

  } else if (variance_structure == "freed" &
             covariance_structure == "freed") {

    model_name <- "VVV"

  } else if (variance_structure == "fixed") {

    stop("variance_structure cannot equal 'fixed' using this function; change this to 'constrained' or 'freed' or try one of the models from mclust::Mclust()")

  }

  x <- Mclust(df, G = n_profiles, modelNames = model_name)

  print(summary(x))

  dff <- bind_cols(df, classification = x$classification)

  proc_df <- dff %>%
    mutate_at(vars(-classification), scale) %>%
    group_by(classification) %>%
    summarize_all(list(mean)) %>%
    mutate(classification = paste0("Profile ", 1:n_profiles)) %>%
    mutate_at(vars(-classification), function(x) round(x, 3)) %>%
    rename(profile = classification)

  return(proc_df)

}



#' lpa_create_profiles2
#'
#' @param df data
#' @param n_profiles 3, ...
#' @param model_name  EEI, VVI, EEE, VVV
#'
#' @return data
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #'
#'
#' iris[,-5] %>% lpa_create_profiles2(n_profiles = 4)
#'
#' #v:constrained, Co: fixed
#' iris[,-5] %>% lpa_create_profiles2(n_profiles = 4, model_name="EEE")
#' #variance_structure == "fixed"
#' iris[,-5] %>% lpa_create_profiles2(n_profiles = 4, model_name="VVV")
#'
#'
#' # variance_structure == "freed" &covariance_structure == "fixed"
#' iris[,-5] %>% lpa_create_profiles2(n_profiles = 4, model_name="VVI")
#'
#'
#' #variance_structure == "constrained" & covariance_structure == "fixed"
#' iris[,-5] %>% lpa_create_profiles2(n_profiles = 4, model_name="EEI")
#' #'
#' }
#'
#'
lpa_create_profiles2 <- function(df,
                                 n_profiles=3,
                                 model_name=NULL){

  library(tidyverse, warn.conflicts = FALSE)
  library(mclust)
  library(hrbrthemes)


  x <- Mclust(df, G = n_profiles, modelNames = model_name)

  print(summary(x))

  dff <- bind_cols(df, classification = x$classification)

  proc_df <- dff %>%
    mutate_at(vars(-classification), scale) %>%
    group_by(classification) %>%
    summarize_all(list(mean)) %>%
    mutate(classification = paste0("Profile ", 1:n_profiles)) %>%
    mutate_at(vars(-classification), function(x) round(x, 3)) %>%
    rename(profile = classification)

  return(proc_df)

}

